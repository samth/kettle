#lang racket/base

;; simple.rkt -- Countdown timer that exits automatically.
;; Adapted from cl-tuition examples/simple.lisp
;; https://github.com/atgreen/cl-tuition/blob/master/examples/simple.lisp
;;
;; Key differences from cl-tuition:
;; - Uses Kettle's `run` functional API with #:on-tick and #:tick-rate instead of
;;   a manual (sleep 1) command loop returning tick-msg instances.
;; - cl-tuition's defprogram macro + defmessage + defmethod dispatch is replaced
;;   by a plain on-key function and pattern matching.
;; - The subscription-based tick (every 1 second via #:tick-rate) is managed by
;;   the runtime, eliminating the need for explicit sleep/post-message threading.
;;
;; Counts down from 5 and exits. Press q or C-c to quit sooner.

(require racket/format
         racket/match
         kettle/run
         kettle/image
         kettle/program)

(define (simple-on-key secs km)
  (match km
    [(key-msg #\q _ _) (cmd secs (quit-cmd))]
    [(key-msg _ _ #t)  (cmd secs (quit-cmd))]  ;; ctrl+any
    [_ secs]))

(define (simple-on-tick secs)
  (define new (sub1 secs))
  (if (<= new 0)
      (cmd new (quit-cmd))
      new))

(define (simple-view secs)
  (vcat 'left
        (text (format "Hi. This program will exit in ~a seconds." secs))
        ""
        "To quit sooner press q or ctrl-c..."))

(module+ main
  (run 5
       #:on-key simple-on-key
       #:on-tick simple-on-tick
       #:tick-rate 1
       #:to-view simple-view))
