#lang racket/base

;; borders.rkt -- Demonstrates border styles.
;; Adapted from cl-tuition examples/borders.lisp
;; https://github.com/atgreen/cl-tuition/blob/master/examples/borders.lisp
;;
;; Key differences from cl-tuition:
;; - Kettle's render-border returns a string (legacy layout API), while the view
;;   assembles these into an image tree via vcat and text.
;; - Border constants are named border-normal, border-rounded, etc. instead of
;;   *border-normal*, *border-rounded* (Racket convention, no earmuffs).
;; - cl-tuition passes :fg-color as a keyword; Kettle uses #:fg-color.
;;
;; Press q to quit.

(require racket/match
         kettle/run
         kettle/image
         kettle/style
         kettle/border
         kettle/program)

(define (borders-on-key _ km)
  (match km
    [(key-msg #\q _ _) (cmd #f (quit-cmd))]
    [(key-msg _ _ #t)  (cmd #f (quit-cmd))]
    [_ #f]))

(define (borders-view _)
  (vcat 'left
        (bold "Border Styles Demo")
        ""
        (string->image (render-border "Normal Border" border-normal))
        ""
        (string->image (render-border "Rounded Border" border-rounded))
        ""
        (string->image (render-border "Thick Border" border-thick))
        ""
        (string->image (render-border "Double Border" border-double))
        ""
        (string->image (render-border "ASCII Border" border-ascii))
        ""
        (string->image (render-border "Colored Border" border-normal
                                      #:fg-color fg-bright-magenta))
        ""
        "Press q to quit"))

(module+ main
  (run #f #:on-key borders-on-key #:to-view borders-view #:alt-screen #t))
