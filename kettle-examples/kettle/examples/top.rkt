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

(struct proc-info (pid user cpu mem stat time command) #:transparent)

;; Read process list from /proc or ps
(define (read-processes)
  (with-handlers ([exn:fail? (lambda (_) '())])
    (define-values (proc stdout stdin stderr)
      (subprocess #f #f #f (find-executable-path "ps")
                  "ax" "-o" "pid,user,pcpu,pmem,stat,etime,comm"
                  "--no-headers" "--sort=-pcpu"))
    (define lines (port->lines stdout))
    (close-input-port stdout)
    (close-output-port stdin)
    (close-input-port stderr)
    (subprocess-wait proc)
    (for/list ([line (in-list lines)]
               #:when (not (string=? (string-trim line) "")))
      (define parts (string-split (string-trim line)))
      (if (>= (length parts) 7)
          (proc-info (list-ref parts 0)
                     (list-ref parts 1)
                     (list-ref parts 2)
                     (list-ref parts 3)
                     (list-ref parts 4)
                     (list-ref parts 5)
                     (string-join (drop parts 6) " "))
          (proc-info (list-ref parts 0)
                     (if (> (length parts) 1) (list-ref parts 1) "?")
                     "0.0" "0.0" "?" "0:00"
                     (string-join (drop parts (min (length parts) 6)) " "))))))

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
(define (read-mem-info)
  (with-handlers ([exn:fail? (lambda (_) (values 0 0 0 0))])
    (define content (file->string "/proc/meminfo"))
    (define (extract key)
      (define rx (pregexp (format "~a:\\s+(\\d+)" key)))
      (define m (regexp-match rx content))
      (if m (string->number (second m)) 0))
    (values (extract "MemTotal")
            (extract "MemFree")
            (extract "Buffers")
            (extract "Cached"))))

;;; ============================================================
;;; State
;;; ============================================================

(define sort-columns '(cpu mem pid user command))

(struct top-state (processes load uptime mem-total mem-used
                             sort-by scroll width height
                             paused?)
  #:transparent
  #:methods gen:kettle-model
  [(define (init s) s)
   (define (subscriptions s) (list (every 2 tick-msg) (on-resize window-size-msg)))
   (define (update s msg) (top-update s msg))
   (define (view s) (top-view s))])

(define (make-top-state #:width [w 80] #:height [h 24])
  (define procs (read-processes))
  (define-values (total free buffers cached) (read-mem-info))
  (define used (- total free buffers cached))
  (top-state procs (read-load-average) (read-uptime)
             total used
             'cpu 0 w h #f))

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
             (define procs (read-processes))
             (define-values (total free buffers cached) (read-mem-info))
             (define used (- total free buffers cached))
             (struct-copy top-state s
                          [processes procs]
                          [load (read-load-average)]
                          [uptime (read-uptime)]
                          [mem-total total]
                          [mem-used used])))]
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
       (struct-copy top-state s [scroll (max 0 (- (top-state-scroll s) (- (top-state-height s) 5)))])]
      [(key-msg 'page-down _ _)
       (struct-copy top-state s [scroll (+ (top-state-scroll s) (- (top-state-height s) 5))])]
      [(key-msg 'home _ _)
       (struct-copy top-state s [scroll 0])]
      [(key-msg 'end _ _)
       (struct-copy top-state s [scroll (max 0 (- (length (top-state-processes s))
                                                   (- (top-state-height s) 5)))])]
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

    ;; System info line
    (define info-line
      (hcat 'top
            (styled label-style (text " top "))
            (styled value-style
                    (text (format "up ~a, load: ~a, ~a processes~a"
                                  (top-state-uptime s)
                                  (top-state-load s)
                                  (length procs)
                                  (if (top-state-paused? s) " [PAUSED]" ""))))))

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
    (define col-cpu 5)
    (define col-mem 5)
    (define col-stat 5)
    (define col-time 12)
    (define col-fixed (+ col-pid 1 col-user 1 col-cpu 1 col-mem 1 col-stat 1 col-time 1))
    (define col-cmd (max 10 (- w col-fixed)))

    (define (sep) (text " "))

    (define header-row
      (styled header-style
              (hcat 'top
                    (col-header 'pid "PID" col-pid) (sep)
                    (col-header 'user "USER" col-user) (sep)
                    (col-header 'cpu "%CPU" col-cpu) (sep)
                    (col-header 'mem "%MEM" col-mem) (sep)
                    (text (~a "STAT" #:min-width col-stat #:max-width col-stat)) (sep)
                    (text (~a "TIME" #:min-width col-time #:max-width col-time)) (sep)
                    (col-header 'command "COMMAND" col-cmd))))

    ;; Process rows
    (define visible-rows (max 0 (- h 4)))
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
          mem-line
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
