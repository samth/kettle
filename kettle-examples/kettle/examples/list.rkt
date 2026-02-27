#lang racket/base

;; list.rkt -- Simple list selection menu.
;; Adapted from cl-tuition examples/list.lisp
;; https://github.com/atgreen/cl-tuition/blob/master/examples/list.lisp
;;
;; Key differences from cl-tuition:
;; - Uses Kettle's `run` functional API with a plain struct as model,
;;   instead of CLOS defclass + defmethod dispatch on key-msg.
;; - Pattern matching on key symbols ('up, 'down, 'enter) replaces
;;   cl-tuition's (or (eq key :up) (and (characterp key) (char= key #\k))).
;; - View returns an image tree (vcat, hcat, styled) rather than a format string.
;; - Immutable struct-copy vs. CLOS setf mutation.
;;
;; Use arrow keys or j/k to navigate, enter to select, q to quit.

(require racket/match
         racket/list
         racket/format
         kettle/run
         kettle/image
         kettle/style
         kettle/program)

(struct list-state (items selected choice quitting?) #:transparent)

(define initial-items
  '("Ramen" "Tomato Soup" "Hamburgers" "Cheeseburgers"
    "Currywurst" "Okonomiyaki" "Pasta" "Fillet Mignon"
    "Caviar" "Just Wine"))

(define (list-on-key st km)
  (match km
    [(key-msg #\q _ _)
     (cmd (struct-copy list-state st [quitting? #t]) (quit-cmd))]
    [(key-msg _ _ #t)
     (cmd st (quit-cmd))]
    [(key-msg 'enter _ _)
     (cmd (struct-copy list-state st
                       [choice (list-ref (list-state-items st)
                                         (list-state-selected st))])
          (quit-cmd))]
    [(key-msg (or 'up #\k) _ _)
     (define sel (list-state-selected st))
     (struct-copy list-state st [selected (max 0 (sub1 sel))])]
    [(key-msg (or 'down #\j) _ _)
     (define sel (list-state-selected st))
     (define len (length (list-state-items st)))
     (struct-copy list-state st [selected (min (sub1 len) (add1 sel))])]
    [_ st]))

(define (list-view st)
  (cond
    [(list-state-choice st)
     (vcat 'left "" ""
           (hcat 'top (text "  ") (text (list-state-choice st))
                      (text "? Sounds good to me."))
           "" "")]
    [(list-state-quitting? st)
     (vcat 'left "" "" "  Not hungry? That's cool." "" "")]
    [else
     (apply vcat 'left
            ""
            "  What do you want for dinner?"
            ""
            (append
             (for/list ([item (in-list (list-state-items st))]
                        [i (in-naturals)])
               (define sel? (= i (list-state-selected st)))
               (hcat 'top
                     (text (if sel? " > " "   "))
                     (styled (make-style #:foreground (if sel? fg-bright-cyan fg-white))
                             (format "~a. ~a" (add1 i) item))))
             (list ""
                   (styled (make-style #:foreground fg-bright-black)
                           "  up/k up • down/j down • enter select • q quit"))))]))

(module+ main
  (run (list-state initial-items 0 #f #f)
       #:on-key list-on-key
       #:to-view list-view))
