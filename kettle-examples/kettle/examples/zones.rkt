#lang racket/base

;; zones.rkt -- Mouse zone tracking with clickable buttons.
;; Adapted from cl-tuition examples/zones.lisp
;; https://github.com/atgreen/cl-tuition/blob/master/examples/zones.lisp
;;
;; Key differences from cl-tuition:
;; - cl-tuition uses invisible ANSI markers (zone-mark / zone-scan) embedded in
;;   rendered strings; Kettle's image-tree renderer makes that impractical, so
;;   this example uses an explicit zone-manager registry with known positions.
;; - Zone positions are calculated from the view layout geometry rather than
;;   being auto-discovered by scanning rendered output.
;; - Immutable struct-copy + zone-manager updates replace CLOS setf mutation
;;   on the global zone registry.
;; - Pattern matching on mouse-press-event replaces CLOS dispatch on mouse-press-event.
;;
;; Click buttons with the mouse. Press q to quit.

(require racket/match
         racket/format
         racket/string
         racket/list
         kettle/program
         kettle/image
         kettle/style
         (prefix-in zm: kettle/components/zone-manager))

(define buttons '("Button 1" "Button 2" "Button 3" "Reset"))
(define button-width 12)  ; visible width of each button cell
(define button-spacing 2)
(define buttons-row 4)    ; y offset of the button row in the view

(define-kettle-program zones-demo
  #:fields ([selected #f]
            [click-count 0]
            [zones (let ()
                     ;; Pre-register button zones at known positions
                     (define z (zm:make-zone-manager))
                     (for/fold ([z z])
                               ([btn (in-list buttons)]
                                [i (in-naturals)])
                       (define x (+ 1 (* i (+ button-width button-spacing))))
                       (zm:zone-register z btn x buttons-row button-width 1)))])
  #:update (lambda (self msg)
    (match msg
      [(key-msg #\q _ _) (cmd self (quit-cmd))]
      [(key-msg _ _ #t)  (cmd self (quit-cmd))]
      ;; Mouse click
      [(mouse-press-event x y _ _ _ 'left)
       (define zones (zones-demo-zones self))
       (define hit (zm:zone-hit zones x y))
       (cond
         [(and hit (string=? hit "Reset"))
          (struct-copy zones-demo self [selected #f] [click-count 0])]
         [hit
          (struct-copy zones-demo self
                       [selected hit]
                       [click-count (add1 (zones-demo-click-count self))])]
         [else self])]
      [_ self]))
  #:view (lambda (self)
    (define sel (zones-demo-selected self))
    ;; Build button row
    (define btn-images
      (for/list ([btn (in-list buttons)])
        (define sel? (and sel (string=? btn sel)))
        (styled (make-style #:foreground fg-white
                            #:background (if sel? bg-blue bg-black)
                            #:bold sel?)
                (format " ~a " (~a btn #:width (- button-width 2))))))
    (vcat 'left
          (bold "Zone Tracking Example")
          ""
          "Click on the buttons below:"
          ""
          (apply hcat 'top
                 (add-between btn-images (text "  ")))
          ""
          (text (format "Selected: ~a" (or sel "None")))
          (text (format "Total clicks: ~a" (zones-demo-click-count self)))
          ""
          "Press q to quit")))

(module+ main
  (program-run (make-program (make-zones-demo) #:alt-screen #t #:mouse 'all-motion)))
