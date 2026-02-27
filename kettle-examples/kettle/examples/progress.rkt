#lang racket/base

;; progress.rkt -- Animated progress bar.
;; Adapted from cl-tuition examples/progress.lisp
;; https://github.com/atgreen/cl-tuition/blob/master/examples/progress.lisp
;;
;; Key differences from cl-tuition:
;; - Uses Kettle's built-in progress component (kettle/components/progress)
;;   instead of manually rendering filled/empty character bars.
;; - Tick-based advancement uses #:on-tick + #:tick-rate in the `run` API,
;;   replacing cl-tuition's (sleep 1.0) command loop.
;; - Auto-quits when progress reaches 100%, using #:stop-when predicate.
;;
;; Progress advances every second. Press any key to quit early.

(require racket/match
         kettle/run
         kettle/image
         kettle/program
         (prefix-in pb: kettle/components/progress))

(define (progress-on-key state km)
  (cmd state (quit-cmd)))

(define (progress-on-tick state)
  (pb:progress-increment state 0.25))

(define (progress-view state)
  (vcat 'left
        ""
        (hcat 'top (text "  ") (view state))
        ""
        "  Press any key to quit"))

(module+ main
  (run (pb:make-progress #:width 40 #:percent 0.0)
       #:on-key progress-on-key
       #:on-tick progress-on-tick
       #:tick-rate 1
       #:to-view progress-view
       #:stop-when (lambda (pb) (>= (pb:progress-percent pb) 1.0))))
