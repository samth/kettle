#lang racket/base

;; timer.rkt -- Countdown timer with start/stop/reset.
;; Adapted from cl-tuition examples/timer.lisp
;; https://github.com/atgreen/cl-tuition/blob/master/examples/timer.lisp
;;
;; Key differences from cl-tuition:
;; - Uses Elm-style subscriptions (every 0.01 tick-msg) instead of manual
;;   (sleep 0.01) command loops that post tick-msg and timeout-msg instances.
;; - define-kettle-program with #:subscriptions replaces three defmethod
;;   specializations (key-msg, tick-msg, timeout-msg).
;; - Immutable struct updates (struct-copy) vs. CLOS setf mutation.
;; - Time tracking uses current-inexact-milliseconds instead of
;;   get-internal-real-time / internal-time-units-per-second.
;;
;; Press s to start/stop, r to reset, q to quit.

(require racket/match
         racket/format
         kettle)

(define timeout-secs 5)

(define-kettle-program timer-demo
  #:fields ([remaining (exact->inexact timeout-secs)]
            [running? #t]
            [last-tick #f])
  #:init (lambda (self)
    (struct-copy timer-demo self
                 [last-tick (current-inexact-milliseconds)]))
  #:update (lambda (self msg)
    (match msg
      ;; Key handling
      [(key-msg #\q _ _) (cmd self (quit-cmd))]
      [(key-msg _ _ #t)  (cmd self (quit-cmd))]
      [(key-msg #\s _ _)
       (if (timer-demo-running? self)
           ;; Pause
           (struct-copy timer-demo self [running? #f])
           ;; Resume
           (struct-copy timer-demo self
                        [running? #t]
                        [last-tick (current-inexact-milliseconds)]))]
      [(key-msg #\r _ _)
       (struct-copy timer-demo self
                    [remaining (exact->inexact timeout-secs)]
                    [last-tick (if (timer-demo-running? self)
                                   (current-inexact-milliseconds)
                                   (timer-demo-last-tick self))])]
      ;; Tick
      [(tick-msg now)
       #:when (timer-demo-running? self)
       (define dt (if (timer-demo-last-tick self)
                      (/ (- now (timer-demo-last-tick self)) 1000.0)
                      0.0))
       (define new-remaining (max 0.0 (- (timer-demo-remaining self) dt)))
       (define new-self
         (struct-copy timer-demo self
                      [remaining new-remaining]
                      [last-tick now]))
       (if (<= new-remaining 0.0)
           (cmd (struct-copy timer-demo new-self [running? #f]) (quit-cmd))
           new-self)]
      [_ self]))
  #:subscriptions (lambda (self)
    (if (timer-demo-running? self)
        (list (every 0.01 tick-msg))
        '()))
  #:view (lambda (self)
    (define rem (timer-demo-remaining self))
    (if (<= rem 0.0)
        (vcat 'left "" (styled (make-style #:foreground fg-bright-green #:bold #t) "All done!") "")
        (vcat 'left
              ""
              (hcat 'top (text "Exiting in ")
                         (styled (make-style #:foreground fg-bright-yellow #:bold #t)
                                 (format "~a" (~r rem #:precision '(= 2))))
                         (text " seconds"))
              ""
              "Controls:"
              (hcat 'top (text "  s    ")
                         (text (if (timer-demo-running? self) "Stop" "Start")))
              "  r    Reset"
              "  q    Quit"
              ""))))

(module+ main
  (program-run (make-program (make-timer-demo) #:alt-screen #t)))
