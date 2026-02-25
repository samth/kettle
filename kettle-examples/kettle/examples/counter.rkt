#lang racket/base

;; counter.rkt -- Hello-world Kettle program.
;; Model is a plain number. Press +/= to increment, -/_ to decrement, q to quit.

(require racket/match
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
  (vcat 'left
        (text (format "Count: ~a" count))
        ""
        "  +/=  increment"
        "  -/_  decrement"
        "  q    quit"))

(module+ main
  (run 0 #:on-key counter-on-key #:to-view counter-view #:alt-screen #t))
