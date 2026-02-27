#lang racket/base

;; styled.rkt -- Demonstrates the styling system.
;; Adapted from cl-tuition examples/styled.lisp
;; https://github.com/atgreen/cl-tuition/blob/master/examples/styled.lisp
;;
;; Key differences from cl-tuition:
;; - Kettle's `styled` wraps an algebraic image subtree (composable, diffable)
;;   rather than producing ANSI escape sequences inline in a format string.
;; - make-style returns an immutable style struct; the renderer resolves colors
;;   and attributes when painting the cell buffer.
;; - cl-tuition's render-styled/colored return decorated strings; Kettle's styled
;;   and colored return image nodes that participate in layout (hcat, vcat).
;; - Foreground/background colors are named constants (fg-red, bg-blue) rather
;;   than global variables (*fg-bright-red*, *bg-black*).
;;
;; Press q to quit.

(require racket/match
         kettle/run
         kettle/image
         kettle/style
         kettle/program)

(define (styled-on-key _ km)
  (match km
    [(key-msg #\q _ _) (cmd #f (quit-cmd))]
    [(key-msg _ _ #t)  (cmd #f (quit-cmd))]
    [_ #f]))

(define (styled-view _)
  (define title-style (make-style #:foreground fg-bright-magenta #:bold #t))
  (define heading-style (make-style #:foreground fg-bright-cyan #:bold #t))
  (define faint-style (make-style #:faint #t))

  (vcat 'left
        (styled title-style "Kettle Styling Demo")
        ""
        (styled heading-style "Text Formatting:")
        (bold "This is bold text")
        (italic "This is italic text")
        (underline "This is underlined text")
        (styled faint-style "This is faint text")
        ""
        (styled heading-style "Colors:")
        (styled (make-style #:foreground fg-bright-red) "Red text")
        (styled (make-style #:foreground fg-bright-green) "Green text")
        (styled (make-style #:foreground fg-bright-blue) "Blue text")
        (styled (make-style #:foreground fg-bright-yellow #:background bg-black)
                "Yellow on black")
        ""
        (styled heading-style "Combined Styles:")
        (styled (make-style #:foreground fg-bright-white #:background bg-blue
                            #:bold #t #:underline #t)
                "Bold underlined white on blue")
        (styled (make-style #:foreground fg-black #:background bg-bright-green
                            #:italic #t)
                "Italic black on bright green")
        ""
        "Press q to quit"))

(module+ main
  (run #f #:on-key styled-on-key #:to-view styled-view #:alt-screen #t))
