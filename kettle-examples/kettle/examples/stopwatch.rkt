#lang racket/base

;; stopwatch.rkt -- Subscription-driven stopwatch.
;; Demonstrates define-kettle-program, subscriptions (every), and tick-based updates.

(require racket/format
         racket/match
         racket/math
         kettle)

(provide (struct-out stopwatch)
         make-stopwatch)

(define-kettle-program
 stopwatch
 #:fields ([elapsed 0.0] ;; seconds
           [running? #f]
           [last-tick #f]) ;; timestamp of last tick
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

     ;; r resets
     [(key-msg #\r _ _) (struct-copy stopwatch self [elapsed 0.0] [running? #f] [last-tick #f])]

     ;; q quits
     [(key-msg #\q _ _) (cmd self (quit-cmd))]

     [_ self]))
 #:subscriptions (lambda (self)
                   (if (stopwatch-running? self)
                       (list (every 0.1 tick-msg))
                       '()))
 #:view (lambda (self)
          (define secs (stopwatch-elapsed self))
          (define mins (exact-floor (/ secs 60)))
          (define s (- secs (* mins 60)))
          (define status (if (stopwatch-running? self) "RUNNING" "STOPPED"))
          (vcat 'left
                (bold (format "~a:~a"
                              (~r mins #:min-width 2 #:pad-string "0")
                              (~r s #:min-width 5 #:precision '(= 1) #:pad-string "0")))
                ""
                (format "  [~a]" status)
                ""
                "  space  start/stop"
                "  r      reset"
                "  q      quit")))

(module+ main
  (define p (make-program (make-stopwatch) #:alt-screen #t))
  (program-run p))
