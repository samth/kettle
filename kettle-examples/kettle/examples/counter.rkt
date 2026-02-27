#lang at-exp racket/base

;; counter.rkt -- Hello-world Kettle program.
;; Adapted from cl-tuition examples/counter.lisp
;; https://github.com/atgreen/cl-tuition/blob/master/examples/counter.lisp
;;
;; Key differences from cl-tuition:
;; - Uses Kettle's `run` functional API instead of CLOS defclass/defmethod.
;; - View returns an algebraic image tree (vcat, text) instead of a format string.
;; - Pattern matching on key-msg structs replaces CLOS method dispatch.
;; - at-exp reader provides a lightweight template syntax for the view.
;;
;; Model is a plain number. Press +/= to increment, -/_ to decrement, q to quit.

(require racket/format
         racket/match
         kettle/run
         kettle/image
         kettle/program)

(provide counter-on-key
         counter-view)

(define (counter-on-key count km)
  (match km
    [(key-msg (or #\+ #\=) _ _) (add1 count)]
    [(key-msg (or #\- #\_) _ _) (sub1 count)]
    [(key-msg #\q _ _) (cmd count (quit-cmd))]
    [_ count]))

(define (counter-view count)
  @vcat['left]{@~a{Count: @count}
               @" "
               @"  "+/=  increment
               @"  "-/_  decrement
               @"  "q    quit})

(module+ main
  (run 0 #:on-key counter-on-key #:to-view counter-view #:alt-screen #t))
