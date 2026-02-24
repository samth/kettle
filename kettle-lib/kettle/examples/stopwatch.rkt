#lang racket/base

;; stopwatch.rkt -- Subscription-driven stopwatch.
;; Demonstrates define-tea-program, subscriptions (every), and tick-based updates.

(require racket/format
         racket/math
         kettle)

(provide (struct-out stopwatch)
         make-stopwatch)

(define-tea-program
 stopwatch
 #:fields ([elapsed 0.0] ;; seconds
           [running? #f]
           [last-tick #f]) ;; timestamp of last tick
 #:update
 (lambda (self msg)
   (cond
     ;; Tick while running
     [(and (tick-msg? msg) (stopwatch-running? self))
      (define now (tick-msg-time msg))
      (define last (stopwatch-last-tick self))
      (define dt
        (if last
            (/ (- now last) 1000.0)
            0.0))
      (values (struct-copy stopwatch self [elapsed (+ (stopwatch-elapsed self) dt)] [last-tick now])
              #f)]

     ;; Space toggles start/stop
     [(and (key-msg? msg) (char? (key-msg-key msg)) (char=? (key-msg-key msg) #\space))
      (if (stopwatch-running? self)
          (values (struct-copy stopwatch self [running? #f] [last-tick #f]) #f)
          (values
           (struct-copy stopwatch self [running? #t] [last-tick (current-inexact-milliseconds)])
           #f))]

     ;; r resets
     [(and (key-msg? msg) (char? (key-msg-key msg)) (char=? (key-msg-key msg) #\r))
      (values (struct-copy stopwatch self [elapsed 0.0] [running? #f] [last-tick #f]) #f)]

     ;; q quits
     [(and (key-msg? msg) (char? (key-msg-key msg)) (char=? (key-msg-key msg) #\q))
      (values self (quit-cmd))]

     [else (values self #f)]))
 #:view (lambda (self)
          (define secs (stopwatch-elapsed self))
          (define mins (exact-floor (/ secs 60)))
          (define s (- secs (* mins 60)))
          (define status (if (stopwatch-running? self) "RUNNING" "STOPPED"))
          (vcat 'left
                (styled (make-style #:bold #t)
                        (text (format "~a:~a"
                                      (~r mins #:min-width 2 #:pad-string "0")
                                      (~r s #:min-width 5 #:precision '(= 1) #:pad-string "0"))))
                (text "")
                (text (format "  [~a]" status))
                (text "")
                (text "  space  start/stop")
                (text "  r      reset")
                (text "  q      quit"))))

(define (main)
  (define p (make-program (make-stopwatch) #:alt-screen #t))
  (program-run p))

(module+ main
  (main))
