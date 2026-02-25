#lang racket/base

;; counter.rkt -- Hello-world Kettle program.
;; Model is a plain number. Press +/= to increment, -/_ to decrement, q to quit.

(require kettle/run
         kettle/image
         kettle/program)

(provide counter-on-key
         counter-view)

(define (counter-on-key count km)
  (define key (key-msg-key km))
  (cond
    [(and (char? key) (or (char=? key #\+) (char=? key #\=))) (add1 count)]
    [(and (char? key) (or (char=? key #\-) (char=? key #\_))) (sub1 count)]
    [(and (char? key) (char=? key #\q)) (cmd count (quit-cmd))]
    [else count]))

(define (counter-view count)
  (vcat 'left
        (text (format "Count: ~a" count))
        (text "")
        (text "  +/=  increment")
        (text "  -/_  decrement")
        (text "  q    quit")))

(module+ main
  (run 0 #:on-key counter-on-key #:to-view counter-view #:alt-screen #t))
