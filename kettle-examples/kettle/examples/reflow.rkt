#lang racket/base

;; reflow.rkt -- Text wrapping, truncation, and indentation.
;; Adapted from cl-tuition examples/reflow.lisp
;; https://github.com/atgreen/cl-tuition/blob/master/examples/reflow.lisp
;;
;; Key differences from cl-tuition:
;; - Uses Kettle's `run` functional API instead of CLOS defclass/defmethod.
;; - wrap-text, truncate-text, and indent-lines have the same API as cl-tuition's
;;   tui:wrap-text, tui:truncate-text, tui:indent-lines -- these are among the
;;   most direct 1:1 correspondences between the two libraries.
;; - View returns an image tree (vcat, text, string->image) instead of format string.
;;
;; Left/right arrows adjust wrap width. Press q to quit.

(require racket/match
         racket/format
         kettle/run
         kettle/image
         kettle/style
         kettle/program)

(define sample-text
  "I went to the woods because I wished to live deliberately, to front only the essential facts of life, and see if I could not learn what it had to teach.")

(define (reflow-on-key width km)
  (match km
    [(key-msg #\q _ _)    (cmd width (quit-cmd))]
    [(key-msg _ _ #t)     (cmd width (quit-cmd))]
    [(key-msg 'left _ _)  (max 10 (sub1 width))]
    [(key-msg 'right _ _) (min 100 (add1 width))]
    [_ width]))

(define (reflow-view width)
  (define wrapped (wrap-text sample-text width))
  (define truncated (truncate-text (render-styled (make-style #:bold #t)
                                                  "Some styled text to truncate cleanly")
                                   20))
  (define indented (indent-lines "Line A\nLine B" 4))
  (vcat 'left
        ""
        (bold "Reflow Demo")
        ""
        (text (format "Wrap (width ~a):" width))
        (string->image wrapped)
        ""
        "Truncate (20):"
        (string->image truncated)
        ""
        "Indent:"
        (string->image indented)
        ""
        (styled (make-style #:foreground fg-bright-black)
                "Controls: left/right adjust width • q quit")))

(module+ main
  (run 40
       #:on-key reflow-on-key
       #:to-view reflow-view
       #:alt-screen #t))
