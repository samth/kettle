#lang racket/base

;; top.rkt -- A top/htop-style process viewer.
;; Demonstrates subscriptions (timers, resize), styled tables, sorting,
;; and responsive layout with flex images.

(require racket/file
         racket/format
         racket/list
         racket/match
         racket/string
         racket/port
         kettle)

(provide (struct-out top-state)
         make-top-state)

;;; ============================================================
;;; Process info
;;; ============================================================

(struct proc-info (pid user cpu mem stat time command ni rss vsz) #:transparent)

(define CLK_TCK 100)   ; clock ticks per second (standard on Linux)
(define PAGE_SIZE 4096) ; bytes per page (standard on Linux x86_64)

;; Cache /etc/passwd uid -> username mapping
(define passwd-table #f)
(define (ensure-passwd-table!)
  (unless passwd-table
    (set! passwd-table (make-hash))
    (with-handlers ([exn:fail? void])
      (for ([line (in-list (file->lines "/etc/passwd"))])
        (define parts (string-split line ":"))
        (when (>= (length parts) 3)
          (define uid (string->number (list-ref parts 2)))
          (when uid (hash-set! passwd-table uid (list-ref parts 0))))))))

(define (uid->username uid)
  (ensure-passwd-table!)
  (hash-ref passwd-table uid (lambda () (number->string uid))))

;; Read uptime in seconds
(define (read-uptime-secs)
  (with-handlers ([exn:fail? (lambda (_) 0)])
    (string->number (first (string-split (file->string "/proc/uptime"))))))

;; Find last occurrence of char in string
(define (string-rindex s ch)
  (let loop ([i (sub1 (string-length s))])
    (cond [(< i 0) #f]
          [(char=? (string-ref s i) ch) i]
          [else (loop (sub1 i))])))

;; Find first occurrence of char in string
(define (string-lindex s ch)
  (let loop ([i 0])
    (cond [(>= i (string-length s)) #f]
          [(char=? (string-ref s i) ch) i]
          [else (loop (add1 i))])))

;; Read process list directly from /proc (no subprocess)
;; prev-ticks: hash of pid-string -> cpu-jiffies, or #f for first sample
;; uptime-secs: system uptime in seconds
;; mem-total-kb: total memory in kB
;; tick-interval: seconds between samples (for instantaneous %CPU)
;; Returns: (values proc-list new-ticks-hash)
(define (read-processes prev-ticks uptime-secs mem-total-kb tick-interval)
  (with-handlers ([exn:fail? (lambda (_) (values '() (or prev-ticks (hash))))])
    (define new-ticks (make-hash))
    (define entries (directory-list "/proc"))
    (define procs
      (for/list ([entry (in-list entries)]
                 #:when (regexp-match? #rx"^[0-9]+$" (path->string entry)))
        (define pid-str (path->string entry))
        (with-handlers ([exn:fail? (lambda (_) #f)])
          (define stat-content (file->string (build-path "/proc" pid-str "stat")))
          ;; Find last ) to handle comm fields with spaces/parens
          (define close-idx (string-rindex stat-content #\)))
          (and close-idx (> (string-length stat-content) (+ close-idx 2))
               (let ()
                 (define after (string-split (substring stat-content (+ close-idx 2))))
                 ;; after[0]=state [11]=utime [12]=stime [16]=nice [19]=starttime [20]=vsize [21]=rss
                 (and (>= (length after) 22)
                      (let ()
                        (define state (list-ref after 0))
                        (define utime (or (string->number (list-ref after 11)) 0))
                        (define stime (or (string->number (list-ref after 12)) 0))
                        (define nice-val (list-ref after 16))
                        (define starttime (or (string->number (list-ref after 19)) 0))
                        (define vsize-bytes (or (string->number (list-ref after 20)) 0))
                        (define rss-pages (or (string->number (list-ref after 21)) 0))

                        ;; comm from stat line (between first ( and last ))
                        (define open-idx (string-lindex stat-content #\())
                        (define comm
                          (if (and open-idx close-idx (< open-idx close-idx))
                              (substring stat-content (add1 open-idx) close-idx)
                              "?"))

                        ;; UID from /proc/[pid]/status (read line by line, stop at Uid)
                        (define uid
                          (with-handlers ([exn:fail? (lambda (_) 0)])
                            (call-with-input-file (build-path "/proc" pid-str "status")
                              (lambda (in)
                                (let loop ()
                                  (define line (read-line in))
                                  (cond
                                    [(eof-object? line) 0]
                                    [(regexp-match #rx"^Uid:\t([0-9]+)" line)
                                     => (lambda (m) (or (string->number (second m)) 0))]
                                    [else (loop)]))))))

                        ;; CPU ticks for delta computation
                        (define total-ticks (+ utime stime))
                        (hash-set! new-ticks pid-str total-ticks)

                        ;; %CPU: instantaneous if we have prev sample, else lifetime average
                        (define cpu-pct
                          (cond
                            [(and prev-ticks (hash-ref prev-ticks pid-str #f))
                             => (lambda (prev)
                                  (define delta (max 0 (- total-ticks prev)))
                                  (* 100.0 (/ delta (* tick-interval CLK_TCK))))]
                            [else
                             (define elapsed (- (* uptime-secs CLK_TCK) starttime))
                             (if (> elapsed 0) (* 100.0 (/ total-ticks elapsed)) 0.0)]))

                        ;; %MEM
                        (define rss-kb (* rss-pages (quotient PAGE_SIZE 1024)))
                        (define mem-pct
                          (if (> mem-total-kb 0)
                              (* 100.0 (/ rss-kb mem-total-kb))
                              0.0))

                        ;; Elapsed time
                        (define elapsed-secs
                          (max 0 (- uptime-secs (/ starttime CLK_TCK))))
                        (define etime-str
                          (let* ([s (inexact->exact (truncate elapsed-secs))]
                                 [days (quotient s 86400)]
                                 [hours (quotient (remainder s 86400) 3600)]
                                 [mins (quotient (remainder s 3600) 60)]
                                 [secs (remainder s 60)])
                            (if (> days 0)
                                (format "~a-~a:~a:~a" days
                                        (~r hours #:min-width 2 #:pad-string "0")
                                        (~r mins #:min-width 2 #:pad-string "0")
                                        (~r secs #:min-width 2 #:pad-string "0"))
                                (format "~a:~a:~a"
                                        (~r hours #:min-width 2 #:pad-string "0")
                                        (~r mins #:min-width 2 #:pad-string "0")
                                        (~r secs #:min-width 2 #:pad-string "0")))))

                        (define vsz-kb (quotient vsize-bytes 1024))

                        (proc-info pid-str
                                   (uid->username uid)
                                   (~r cpu-pct #:precision '(= 1))
                                   (~r mem-pct #:precision '(= 1))
                                   state
                                   etime-str
                                   comm
                                   nice-val
                                   (number->string rss-kb)
                                   (number->string vsz-kb)))))))))
    (values (filter values procs) new-ticks)))

;; Read system load averages
(define (read-load-average)
  (with-handlers ([exn:fail? (lambda (_) "?")])
    (define content (file->string "/proc/loadavg"))
    (define parts (string-split content))
    (if (>= (length parts) 3)
        (format "~a ~a ~a" (first parts) (second parts) (third parts))
        "?")))

;; Read uptime
(define (read-uptime)
  (with-handlers ([exn:fail? (lambda (_) "?")])
    (define content (file->string "/proc/uptime"))
    (define secs (inexact->exact (truncate (string->number (first (string-split content))))))
    (define days (quotient secs 86400))
    (define hours (quotient (remainder secs 86400) 3600))
    (define mins (quotient (remainder secs 3600) 60))
    (cond
      [(> days 0) (format "~a day~a, ~a:~a"
                          days (if (= days 1) "" "s")
                          (~r hours #:min-width 2 #:pad-string "0")
                          (~r mins #:min-width 2 #:pad-string "0"))]
      [else (format "~a:~a"
                    (~r hours #:min-width 2 #:pad-string "0")
                    (~r mins #:min-width 2 #:pad-string "0"))])))

;; Read memory info from /proc/meminfo
;; Returns: mem-total mem-free buffers cached swap-total swap-free mem-available (all in kB)
(define (read-mem-info)
  (with-handlers ([exn:fail? (lambda (_) (values 0 0 0 0 0 0 0))])
    (define content (file->string "/proc/meminfo"))
    (define (extract key)
      (define rx (pregexp (format "~a:\\s+(\\d+)" key)))
      (define m (regexp-match rx content))
      (if m (string->number (second m)) 0))
    (values (extract "MemTotal")
            (extract "MemFree")
            (extract "Buffers")
            (extract "Cached")
            (extract "SwapTotal")
            (extract "SwapFree")
            (extract "MemAvailable"))))

;; Compute task/process state summary from an already-fetched process list
;; Returns: (values total running sleeping stopped zombie)
(define (summarize-tasks procs)
  (define total (length procs))
  (define running 0)
  (define sleeping 0)
  (define stopped 0)
  (define zombie 0)
  (for ([p (in-list procs)])
    (define st (proc-info-stat p))
    (when (> (string-length st) 0)
      (case (string-ref st 0)
        [(#\R) (set! running (add1 running))]
        [(#\S #\I) (set! sleeping (add1 sleeping))]
        [(#\T #\t) (set! stopped (add1 stopped))]
        [(#\Z) (set! zombie (add1 zombie))]
        [else (void)])))
  (values total running sleeping stopped zombie))

;; Read CPU counters from /proc/stat
;; Returns a list of numbers: (user nice system idle iowait irq softirq steal) or #f
(define (read-cpu-counters)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define content (file->string "/proc/stat"))
    (define first-line (first (string-split content "\n")))
    (define parts (string-split first-line))
    ;; parts is: "cpu" user nice system idle iowait irq softirq steal ...
    (if (>= (length parts) 9)
        (for/list ([p (in-list (take (cdr parts) 8))])
          (string->number p))
        #f)))

;; Compute CPU percentages from two counter snapshots
;; Returns: (values user% system% nice% idle% iowait%)
(define (compute-cpu-pcts prev curr)
  (if (or (not prev) (not curr))
      (values 0.0 0.0 0.0 100.0 0.0)
      (let ()
        (define deltas (map - curr prev))
        (define total (apply + deltas))
        (if (<= total 0)
            (values 0.0 0.0 0.0 100.0 0.0)
            (let ([pct (lambda (i) (* 100.0 (/ (list-ref deltas i) total)))])
              (values (pct 0)    ; user
                      (pct 2)    ; system
                      (pct 1)    ; nice
                      (pct 3)    ; idle
                      (pct 4)))))))  ; iowait

;; Read number of logged-in users
(define (read-user-count)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define-values (proc stdout stdin stderr)
      (subprocess #f #f #f (find-executable-path "who")))
    (define lines (port->lines stdout))
    (close-input-port stdout)
    (close-output-port stdin)
    (close-input-port stderr)
    (subprocess-wait proc)
    (length (filter (lambda (l) (not (string=? (string-trim l) ""))) lines))))

;; Format kB value as human-readable (e.g., "1.2g", "512m", "100k")
(define (format-kb kb-str)
  (define kb (string->number kb-str))
  (cond
    [(not kb) kb-str]
    [(>= kb (* 1024 1024)) (format "~ag" (~r (/ kb 1024.0 1024.0) #:precision '(= 1)))]
    [(>= kb 1024) (format "~am" (inexact->exact (round (/ kb 1024.0))))]
    [else (format "~ak" kb)]))

;;; ============================================================
;;; State
;;; ============================================================

(define sort-columns '(cpu mem pid user command))

(struct top-state (processes load uptime mem-total mem-used
                             sort-by scroll width height
                             paused?
                             prev-cpu-counters cpu-user cpu-sys cpu-nice cpu-idle cpu-iowait
                             swap-total swap-free mem-avail
                             task-total task-running task-sleeping task-stopped task-zombie
                             user-count
                             prev-proc-ticks)
  #:transparent
  #:methods gen:kettle-model
  [(define (init s) s)
   (define (subscriptions s) (list (every 2 tick-msg) (on-resize window-size-msg)))
   (define (update s msg) (top-update s msg))
   (define (view s) (top-view s))])

(define (make-top-state #:width [w 80] #:height [h 24])
  (define-values (total free buffers cached swap-total swap-free mem-available) (read-mem-info))
  (define used (- total free buffers cached))
  (define uptime-secs (read-uptime-secs))
  (define-values (procs proc-ticks) (read-processes #f uptime-secs total 2))
  (define-values (t-total t-running t-sleeping t-stopped t-zombie) (summarize-tasks procs))
  (define cpu-counters (read-cpu-counters))
  (top-state procs (read-load-average) (read-uptime)
             total used
             'cpu 0 w h #f
             cpu-counters 0.0 0.0 0.0 100.0 0.0
             swap-total swap-free mem-available
             t-total t-running t-sleeping t-stopped t-zombie
             (read-user-count)
             proc-ticks))

;;; ============================================================
;;; Update
;;; ============================================================

(define (sort-processes procs sort-by)
  (case sort-by
    [(cpu) (sort procs > #:key (lambda (p) (string->number (proc-info-cpu p))))]
    [(mem) (sort procs > #:key (lambda (p) (string->number (proc-info-mem p))))]
    [(pid) (sort procs < #:key (lambda (p) (string->number (proc-info-pid p))))]
    [(user) (sort procs string<? #:key proc-info-user)]
    [(command) (sort procs string<? #:key proc-info-command)]
    [else procs]))

(define (next-sort-col current)
  (define idx (index-of sort-columns current))
  (list-ref sort-columns (remainder (add1 (or idx 0)) (length sort-columns))))

(define (top-update s msg)
    (match msg
      ;; Timer tick: refresh process list
      [(tick-msg _)
       (if (top-state-paused? s)
           s
           (let ()
             (define-values (total free buffers cached swap-t swap-f mem-a) (read-mem-info))
             (define used (- total free buffers cached))
             (define uptime-secs (read-uptime-secs))
             (define-values (procs proc-ticks)
               (read-processes (top-state-prev-proc-ticks s) uptime-secs total 2))
             (define-values (t-total t-running t-sleeping t-stopped t-zombie) (summarize-tasks procs))
             (define new-cpu (read-cpu-counters))
             (define prev-cpu (top-state-prev-cpu-counters s))
             (define-values (cu cs cn ci cw) (compute-cpu-pcts prev-cpu new-cpu))
             (struct-copy top-state s
                          [processes procs]
                          [load (read-load-average)]
                          [uptime (read-uptime)]
                          [mem-total total]
                          [mem-used used]
                          [prev-cpu-counters new-cpu]
                          [cpu-user cu] [cpu-sys cs] [cpu-nice cn]
                          [cpu-idle ci] [cpu-iowait cw]
                          [swap-total swap-t] [swap-free swap-f] [mem-avail mem-a]
                          [task-total t-total] [task-running t-running]
                          [task-sleeping t-sleeping] [task-stopped t-stopped]
                          [task-zombie t-zombie]
                          [user-count (read-user-count)]
                          [prev-proc-ticks proc-ticks])))]
      ;; Resize
      [(window-size-msg w h)
       (struct-copy top-state s [width w] [height h])]
      ;; Keys
      [(key-msg #\q _ _) (cmd s (quit-cmd))]
      [(key-msg #\s _ _)
       (struct-copy top-state s [sort-by (next-sort-col (top-state-sort-by s))])]
      [(key-msg #\p _ _)
       (struct-copy top-state s [paused? (not (top-state-paused? s))])]
      [(key-msg 'up _ _)
       (struct-copy top-state s [scroll (max 0 (sub1 (top-state-scroll s)))])]
      [(key-msg 'down _ _)
       (struct-copy top-state s [scroll (add1 (top-state-scroll s))])]
      [(key-msg 'page-up _ _)
       (struct-copy top-state s [scroll (max 0 (- (top-state-scroll s) (- (top-state-height s) 7)))])]
      [(key-msg 'page-down _ _)
       (struct-copy top-state s [scroll (+ (top-state-scroll s) (- (top-state-height s) 7))])]
      [(key-msg 'home _ _)
       (struct-copy top-state s [scroll 0])]
      [(key-msg 'end _ _)
       (struct-copy top-state s [scroll (max 0 (- (length (top-state-processes s))
                                                   (- (top-state-height s) 7)))])]
      ;; Mouse scroll
      [(mouse-scroll-event _ _ _ _ _ dir _)
       (case dir
         [(up) (struct-copy top-state s [scroll (max 0 (- (top-state-scroll s) 3))])]
         [(down) (struct-copy top-state s [scroll (+ (top-state-scroll s) 3)])])]
      [_ s]))

(define (top-view s)
    (define w (top-state-width s))
    (define h (top-state-height s))
    (define procs (sort-processes (top-state-processes s) (top-state-sort-by s)))

    ;; Header styles
    (define header-style (make-style #:bold #t #:reverse #t))
    (define label-style (make-style #:bold #t #:foreground fg-cyan))
    (define value-style (make-style #:foreground fg-white))
    (define bar-used-style (make-style #:foreground fg-green))
    (define bar-free-style (make-style #:foreground fg-black))
    (define sort-style (make-style #:foreground fg-yellow #:bold #t))

    ;; Current time
    (define now (seconds->date (current-seconds)))
    (define current-time
      (format "~a:~a:~a"
              (~r (date-hour now) #:min-width 2 #:pad-string "0")
              (~r (date-minute now) #:min-width 2 #:pad-string "0")
              (~r (date-second now) #:min-width 2 #:pad-string "0")))

    ;; User count
    (define uc (top-state-user-count s))
    (define user-str (if uc (format "~a user~a" uc (if (= uc 1) "" "s")) "? users"))

    ;; System info line
    (define info-line
      (hcat 'top
            (styled label-style (text " top - "))
            (styled value-style
                    (text (format "~a up ~a, ~a, load average: ~a~a"
                                  current-time
                                  (top-state-uptime s)
                                  user-str
                                  (top-state-load s)
                                  (if (top-state-paused? s) " [PAUSED]" ""))))))

    ;; Task summary line
    (define task-line
      (hcat 'top
            (styled label-style (text " Tasks: "))
            (styled value-style
                    (text (format "~a total, ~a running, ~a sleeping, ~a stopped, ~a zombie"
                                  (top-state-task-total s)
                                  (top-state-task-running s)
                                  (top-state-task-sleeping s)
                                  (top-state-task-stopped s)
                                  (top-state-task-zombie s))))))

    ;; CPU bar
    (define cpu-bar-label " %Cpu: ")
    (define cpu-user-pct (top-state-cpu-user s))
    (define cpu-sys-pct (top-state-cpu-sys s))
    (define cpu-iowait-pct (top-state-cpu-iowait s))
    (define cpu-idle-pct (top-state-cpu-idle s))
    (define cpu-pct-str (format " ~a us, ~a sy, ~a wa, ~a id"
                                (~r cpu-user-pct #:precision '(= 1))
                                (~r cpu-sys-pct #:precision '(= 1))
                                (~r cpu-iowait-pct #:precision '(= 1))
                                (~r cpu-idle-pct #:precision '(= 1))))
    (define cpu-bar-width (max 1 (- w (string-length cpu-bar-label) 2 (string-length cpu-pct-str))))
    (define cpu-used-total (+ cpu-user-pct cpu-sys-pct cpu-iowait-pct))
    (define cpu-used-chars (min cpu-bar-width (max 0 (inexact->exact (round (* (/ cpu-used-total 100.0) cpu-bar-width))))))
    (define cpu-user-chars (min cpu-used-chars (max 0 (inexact->exact (round (* (/ cpu-user-pct 100.0) cpu-bar-width))))))
    (define cpu-sys-chars (min (- cpu-used-chars cpu-user-chars)
                               (max 0 (inexact->exact (round (* (/ cpu-sys-pct 100.0) cpu-bar-width))))))
    (define cpu-wa-chars (- cpu-used-chars cpu-user-chars cpu-sys-chars))
    (define cpu-sys-style (make-style #:foreground fg-red))
    (define cpu-wa-style (make-style #:foreground fg-cyan))
    (define cpu-line
      (hcat 'top
            (styled label-style (text cpu-bar-label))
            (text "[")
            (styled bar-used-style (text (make-string cpu-user-chars #\|)))
            (styled cpu-sys-style (text (make-string cpu-sys-chars #\s)))
            (styled cpu-wa-style (text (make-string cpu-wa-chars #\w)))
            (styled bar-free-style (text (make-string (- cpu-bar-width cpu-used-chars) #\space)))
            (text "]")
            (styled value-style (text cpu-pct-str))))

    ;; Memory bar
    (define mem-total (max 1 (top-state-mem-total s)))
    (define mem-used (top-state-mem-used s))
    (define mem-str (format " ~a/~aM" (quotient mem-used 1024) (quotient mem-total 1024)))
    (define bar-width (max 1 (- w 6 1 1 (string-length mem-str))))
    (define used-chars (min bar-width (max 0 (quotient (* mem-used bar-width) mem-total))))
    (define mem-line
      (hcat 'top
            (styled label-style (text " Mem: "))
            (text "[")
            (styled bar-used-style (text (make-string used-chars #\|)))
            (styled bar-free-style (text (make-string (- bar-width used-chars) #\space)))
            (text "]")
            (styled value-style (text mem-str))))

    ;; Swap line
    (define swap-total-kb (top-state-swap-total s))
    (define swap-free-kb (top-state-swap-free s))
    (define swap-used-kb (- swap-total-kb swap-free-kb))
    (define avail-kb (top-state-mem-avail s))
    (define swap-line
      (hcat 'top
            (styled label-style (text " Swap: "))
            (styled value-style
                    (text (format "~a/~aM   Avail: ~aM"
                                  (quotient swap-used-kb 1024)
                                  (quotient swap-total-kb 1024)
                                  (quotient avail-kb 1024))))))

    ;; Column headers
    (define (col-header name label width)
      (define s*
        (if (eq? name (top-state-sort-by s))
            sort-style
            header-style))
      (styled s* (text (~a label #:min-width width #:max-width width))))

    ;; Column widths (not including separators)
    (define col-pid 7)
    (define col-user 9)
    (define col-ni 3)
    (define col-virt 6)
    (define col-res 6)
    (define col-cpu 5)
    (define col-mem 5)
    (define col-stat 5)
    (define col-time 12)
    (define col-fixed (+ col-pid 1 col-user 1 col-ni 1 col-virt 1 col-res 1
                         col-cpu 1 col-mem 1 col-stat 1 col-time 1))
    (define col-cmd (max 10 (- w col-fixed)))

    (define (sep) (text " "))

    (define header-row
      (styled header-style
              (hcat 'top
                    (col-header 'pid "PID" col-pid) (sep)
                    (col-header 'user "USER" col-user) (sep)
                    (text (~a "NI" #:min-width col-ni #:max-width col-ni)) (sep)
                    (text (~a "VIRT" #:min-width col-virt #:max-width col-virt)) (sep)
                    (text (~a "RES" #:min-width col-res #:max-width col-res)) (sep)
                    (col-header 'cpu "%CPU" col-cpu) (sep)
                    (col-header 'mem "%MEM" col-mem) (sep)
                    (text (~a "STAT" #:min-width col-stat #:max-width col-stat)) (sep)
                    (text (~a "TIME" #:min-width col-time #:max-width col-time)) (sep)
                    (col-header 'command "COMMAND" col-cmd))))

    ;; Process rows
    (define visible-rows (max 0 (- h 7)))
    (define scroll (min (top-state-scroll s) (max 0 (- (length procs) visible-rows))))
    (define visible-procs (take (drop procs (min scroll (length procs)))
                                (min visible-rows
                                     (max 0 (- (length procs) scroll)))))

    (define (proc-row p i)
      (define cpu-val (string->number (proc-info-cpu p)))
      (define row-style
        (cond
          [(and cpu-val (> cpu-val 50.0))
           (make-style #:foreground fg-red #:bold #t)]
          [(and cpu-val (> cpu-val 10.0))
           (make-style #:foreground fg-yellow)]
          [(even? i)
           (make-style #:foreground fg-white)]
          [else #f]))
      (define row-img
        (hcat 'top
              (text (~a (proc-info-pid p) #:min-width col-pid #:max-width col-pid)) (sep)
              (text (~a (proc-info-user p) #:min-width col-user #:max-width col-user)) (sep)
              (text (~a (proc-info-ni p) #:min-width col-ni #:max-width col-ni #:align 'right)) (sep)
              (text (~a (format-kb (proc-info-vsz p)) #:min-width col-virt #:max-width col-virt #:align 'right)) (sep)
              (text (~a (format-kb (proc-info-rss p)) #:min-width col-res #:max-width col-res #:align 'right)) (sep)
              (text (~a (proc-info-cpu p) #:min-width col-cpu #:max-width col-cpu #:align 'right)) (sep)
              (text (~a (proc-info-mem p) #:min-width col-mem #:max-width col-mem #:align 'right)) (sep)
              (text (~a (proc-info-stat p) #:min-width col-stat #:max-width col-stat)) (sep)
              (text (~a (proc-info-time p) #:min-width col-time #:max-width col-time)) (sep)
              (text (~a (proc-info-command p) #:min-width col-cmd #:max-width col-cmd))))
      (if row-style
          (styled row-style row-img)
          row-img))

    (define process-rows
      (for/list ([p (in-list visible-procs)]
                 [i (in-naturals)])
        (proc-row p i)))

    ;; Footer
    (define footer-style (make-style #:reverse #t))
    (define footer
      (styled footer-style
              (text (~a (format " q:quit  s:sort(~a)  p:~a  ↑↓/PgUp/PgDn:scroll"
                                (top-state-sort-by s)
                                (if (top-state-paused? s) "resume" "pause"))
                        #:min-width w #:max-width w))))

    ;; Compose
    (define body
      (if (null? process-rows)
          (vcat 'left (text " No processes"))
          (apply vcat 'left process-rows)))

    (vcat 'left
          info-line
          task-line
          cpu-line
          mem-line
          swap-line
          header-row
          (crop w visible-rows 0 0 body)
          footer))

;;; ============================================================
;;; Main
;;; ============================================================

(module+ main
  (program-run (make-program (make-top-state)
                             #:alt-screen #t
                             #:mouse 'cell-motion)))
