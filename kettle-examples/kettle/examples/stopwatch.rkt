#lang racket/base

;; stopwatch.rkt -- Attractive stopwatch with ASCII-art digits.
;; Demonstrates define-kettle-program, subscriptions (every), tick-based updates,
;; lap recording, and styled image rendering.

(require racket/format
         racket/list
         racket/match
         racket/math
         racket/string
         kettle)

(provide (struct-out stopwatch)
         make-stopwatch)

;;; ASCII-art digit font (each digit is 5 lines tall, 5 chars wide)
(define digit-font
  #hash((#\0 . ("╔═══╗" "║   ║" "║   ║" "║   ║" "╚═══╝"))
        (#\1 . ("    ╗" "    ║" "    ║" "    ║" "    ╝"))
        (#\2 . ("╔═══╗" "    ║" "╔═══╝" "║    " "╚═══╝"))
        (#\3 . ("╔═══╗" "    ║" " ═══╣" "    ║" "╚═══╝"))
        (#\4 . ("╗   ╗" "║   ║" "╚═══╣" "    ║" "    ╝"))
        (#\5 . ("╔═══╗" "║    " "╚═══╗" "    ║" "╚═══╝"))
        (#\6 . ("╔═══╗" "║    " "╠═══╗" "║   ║" "╚═══╝"))
        (#\7 . ("╔═══╗" "    ║" "    ║" "    ║" "    ╝"))
        (#\8 . ("╔═══╗" "║   ║" "╠═══╣" "║   ║" "╚═══╝"))
        (#\9 . ("╔═══╗" "║   ║" "╚═══╣" "    ║" "╚═══╝"))
        (#\: . ("     " "  ●  " "     " "  ●  " "     "))
        (#\. . ("   " "   " "   " " ● " "   "))))

;; Render a string of digit characters as a big ASCII-art image
(define (big-digits str)
  (define chars (string->list str))
  (define columns
    (for/list ([ch (in-list chars)])
      (define lines (hash-ref digit-font ch '("     " "     " "     " "     " "     ")))
      (apply vcat 'left (map text lines))))
  (apply hcat 'top columns))

;; A vertical bar of n lines, for bordering multi-line content
(define (left-bar n)
  (apply vcat 'left (make-list n (text "│ "))))

(define (right-bar n pad-w)
  (apply vcat 'left (make-list n (text (string-append (make-string pad-w #\space) "│")))))

;;; Time formatting

(define (format-time secs)
  ;; Returns "MM:SS.HH" string for the big digits
  (define mins (exact-floor (/ secs 60)))
  (define s (- secs (* mins 60)))
  (define whole-s (exact-floor s))
  (define hundredths (exact-floor (* 100 (- s whole-s))))
  (format "~a:~a.~a"
          (~r mins #:min-width 2 #:pad-string "0")
          (~r whole-s #:min-width 2 #:pad-string "0")
          (~r hundredths #:min-width 2 #:pad-string "0")))

(define (format-time-compact secs)
  ;; "MM:SS.T" for lap display
  (define mins (exact-floor (/ secs 60)))
  (define s (- secs (* mins 60)))
  (format "~a:~a"
          (~r mins #:min-width 2 #:pad-string "0")
          (~r s #:min-width 5 #:precision '(= 1) #:pad-string "0")))

;;; Stopwatch program

(define-kettle-program
 stopwatch
 #:fields ([elapsed 0.0] ;; seconds
           [running? #f]
           [last-tick #f] ;; timestamp of last tick
           [laps '()]) ;; list of elapsed times at each lap
 #:update
 (lambda (self msg)
   (match msg
     ;; Tick while running
     [(tick-msg now)
      #:when (stopwatch-running? self)
      (define last (stopwatch-last-tick self))
      (define dt
        (if last
            (/ (- now last) 1000.0)
            0.0))
      (struct-copy stopwatch self [elapsed (+ (stopwatch-elapsed self) dt)] [last-tick now])]

     ;; Space toggles start/stop
     [(key-msg #\space _ _)
      (if (stopwatch-running? self)
          (struct-copy stopwatch self [running? #f] [last-tick #f])
          (struct-copy stopwatch self [running? #t] [last-tick (current-inexact-milliseconds)]))]

     ;; l records a lap
     [(key-msg #\l _ _)
      #:when (stopwatch-running? self)
      (struct-copy stopwatch
                   self
                   [laps (append (stopwatch-laps self) (list (stopwatch-elapsed self)))])]

     ;; r resets
     [(key-msg #\r _ _)
      (struct-copy stopwatch self [elapsed 0.0] [running? #f] [last-tick #f] [laps '()])]

     ;; q quits
     [(key-msg #\q _ _) (cmd self (quit-cmd))]

     [_ self]))
 #:subscriptions (lambda (self)
                   (if (stopwatch-running? self)
                       (list (every 0.01 tick-msg))
                       '()))
 #:view (lambda (self)
          (define secs (stopwatch-elapsed self))
          (define running (stopwatch-running? self))
          (define laps (stopwatch-laps self))
          (define time-str (format-time secs))

          ;; Color: green when running, dim when stopped
          (define time-style
            (if running
                (make-style #:foreground fg-green #:bold #t)
                (make-style #:faint #t)))

          ;; Big digit display
          (define digits-image (styled time-style (big-digits time-str)))

          ;; Status indicator
          (define status
            (if running
                (styled (make-style #:foreground fg-green) "● RUNNING")
                (styled (make-style #:faint #t) "○ STOPPED")))

          ;; Lap list
          (define lap-images
            (for/list ([lap-time (in-list laps)]
                       [i (in-naturals 1)])
              (format "  Lap ~a: ~a" i (format-time-compact lap-time))))

          ;; Border helpers
          ;; cw = number of chars between left │ and right │
          (define cw
            (max (+ 1 (image-w digits-image) 1) ;; space + digits + space
                 (if (null? lap-images)
                     0
                     (apply max (map string-length lap-images)))))
          (define top-bar (text (string-append "╭" (make-string cw #\─) "╮")))
          (define bot-bar (text (string-append "╰" (make-string cw #\─) "╯")))
          (define (row content)
            (define img (ensure-image content))
            (define rpad (max 0 (- cw 1 (image-w img))))
            (hcat 'top (text "│ ") img (text (string-append (make-string rpad #\space) "│"))))
          (define (row-multi img h)
            (define rpad (max 0 (- cw 1 (image-w img))))
            (hcat 'top (left-bar h) img (right-bar h rpad)))
          (define blank-row
            (text (string-append "│" (make-string cw #\space) "│")))

          ;; Assemble
          (apply vcat
                 'left
                 top-bar
                 (row-multi digits-image 5)
                 (row status)
                 blank-row
                 (append (if (null? lap-images)
                             '()
                             (cons (row (bold "Laps:"))
                                   (for/list ([li (in-list lap-images)])
                                     (row (text li)))))
                         (list blank-row
                               (row "spc go/stop l lap r rst q quit")
                               bot-bar)))))

(module+ main
  (define p (make-program (make-stopwatch) #:alt-screen #t))
  (program-run p))
