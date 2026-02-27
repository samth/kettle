#lang racket/base

;; window-size.rkt -- Terminal size display.
;; Adapted from cl-tuition examples/window-size.lisp
;; https://github.com/atgreen/cl-tuition/blob/master/examples/window-size.lisp
;;
;; Key differences from cl-tuition:
;; - Kettle's window-size-msg is a struct with width/height fields, delivered
;;   automatically when the terminal resizes (via SIGWINCH).
;; - cl-tuition's window-size-msg also has width/height accessors; the difference
;;   is primarily in API naming (window-size-msg-width vs. tui:window-size-msg-width).
;; - Uses define-kettle-program for a clean struct-based model.
;;
;; Resize your terminal to see the dimensions update. Press q to quit.

(require racket/match
         racket/format
         kettle)

(define-kettle-program window-size-demo
  #:fields ([width 0] [height 0])
  #:init (lambda (self)
    (define size (get-terminal-size))
    (if size
        (struct-copy window-size-demo self
                     [width (car size)]
                     [height (cdr size)])
        self))
  #:update (lambda (self msg)
    (match msg
      [(key-msg #\q _ _) (cmd self (quit-cmd))]
      [(key-msg _ _ #t)  (cmd self (quit-cmd))]
      [(window-size-msg w h)
       (struct-copy window-size-demo self [width w] [height h])]
      [_ self]))
  #:view (lambda (self)
    (vcat 'left
          ""
          (bold "  Terminal Size")
          ""
          (hcat 'top (text "  Width:  ")
                     (styled (make-style #:foreground fg-bright-cyan #:bold #t)
                             (format "~a" (window-size-demo-width self))))
          (hcat 'top (text "  Height: ")
                     (styled (make-style #:foreground fg-bright-cyan #:bold #t)
                             (format "~a" (window-size-demo-height self))))
          ""
          (styled (make-style #:foreground fg-bright-black)
                  "  Resize your terminal to see updates. Press q to quit."))))

(module+ main
  (program-run (make-program (make-window-size-demo) #:alt-screen #t)))
