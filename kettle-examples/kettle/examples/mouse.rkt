#lang racket/base

;; mouse.rkt -- Mouse tracking demo with a circle that follows the cursor.
;; Adapted from cl-tuition examples/mouse.lisp
;; https://github.com/atgreen/cl-tuition/blob/master/examples/mouse.lisp
;;
;; Key differences from cl-tuition:
;; - Kettle uses a typed mouse-event hierarchy (mouse-press-event, mouse-release-event,
;;   mouse-move-event, mouse-scroll-event, mouse-drag-event) with pattern matching,
;;   replacing cl-tuition's generic mouse-msg with action slot (:press, :release, etc.)
;; - Mouse is enabled via #:mouse 'all-motion on make-program, vs. cl-tuition's
;;   :mouse :all-motion on make-program.
;; - Draws a circle that tracks the mouse position across the entire terminal.
;;
;; Move the mouse to see the circle follow. Press q to quit.

(require racket/match
         racket/format
         kettle/program
         kettle/image
         kettle/style)

(define circle-radius 3)
;; Terminal cells are ~2x taller than wide, so scale x for a round circle.
(define aspect-ratio 2.0)

(define-kettle-program
 mouse-demo
 #:fields ([mx 40] [my 12] [tw 80] [th 24] [button ""] [action ""] [scroll-dir ""])
 #:update
 (lambda (self msg)
   (match msg
     [(key-msg #\q _ _) (cmd self (quit-cmd))]
     [(key-msg _ _ #t) (cmd self (quit-cmd))]
     [(window-size-msg w h) (struct-copy mouse-demo self [tw w] [th h])]
     [(mouse-press-event x y _ _ _ button)
      (struct-copy mouse-demo
                   self
                   [mx x]
                   [my y]
                   [button (format "~a" button)]
                   [action "press"]
                   [scroll-dir ""])]
     [(mouse-release-event x y _ _ _ button)
      (struct-copy mouse-demo
                   self
                   [mx x]
                   [my y]
                   [button (format "~a" button)]
                   [action "release"]
                   [scroll-dir ""])]
     [(mouse-drag-event x y _ _ _ button)
      (struct-copy mouse-demo
                   self
                   [mx x]
                   [my y]
                   [button (format "~a" button)]
                   [action "drag"]
                   [scroll-dir ""])]
     [(mouse-move-event x y _ _ _)
      (struct-copy mouse-demo self [mx x] [my y] [action "move"] [button ""] [scroll-dir ""])]
     [(mouse-scroll-event x y _ _ _ dir _)
      (struct-copy mouse-demo
                   self
                   [mx x]
                   [my y]
                   [action "scroll"]
                   [scroll-dir (format "~a" dir)]
                   [button ""])]
     [_ self]))
 #:view
 (lambda (self)
   (define w (mouse-demo-tw self))
   (define h (mouse-demo-th self))
   (define cx (mouse-demo-mx self))
   (define cy (mouse-demo-my self))
   (define r circle-radius)

   ;; Build just the circle bounding box, then pad into position
   (define half-w (inexact->exact (ceiling (* (+ r 1) aspect-ratio))))
   (define half-h (inexact->exact (ceiling (+ r 1))))
   (define circle-img
     (apply vcat
            'left
            (for/list ([dy (in-range (- half-h) (add1 half-h))])
              (define line
                (apply string
                       (for/list ([col-off (in-range (- half-w) (add1 half-w))])
                         (define dx (/ col-off aspect-ratio))
                         (define d2 (+ (* dx dx) (* dy dy)))
                         (cond
                           [(and (>= d2 (* (- r 0.5) (- r 0.5)))
                                 (<= d2 (* (+ r 0.5) (+ r 0.5))))
                            #\u00B7]
                           [else #\space]))))
              (text line))))
   (define pad-left (max 0 (- cx half-w)))
   (define pad-top (max 0 (- cy half-h)))
   (define pad-right (max 0 (- w (+ cx half-w 1))))
   (define pad-bottom (max 0 (- h (+ cy half-h 1))))
   (define canvas (crop w h 0 0
                        (pad circle-img
                             #:left pad-left #:right pad-right
                             #:top pad-top #:bottom pad-bottom)))

   ;; Info overlay in top-left
   (define action-str (mouse-demo-action self))
   (define detail
     (cond
       [(string=? action-str "scroll") (format "scroll ~a" (mouse-demo-scroll-dir self))]
       [(string=? (mouse-demo-button self) "") action-str]
       [else (format "~a ~a" action-str (mouse-demo-button self))]))
   (define info
     (vcat 'left
           (bold " Mouse Tracking Demo")
           (hcat 'top
                 (text " Position: ")
                 (styled (make-style #:foreground fg-bright-cyan) (format "(~a, ~a)" cx cy)))
           (hcat 'top
                 (text " Action:   ")
                 (styled (make-style #:foreground fg-bright-yellow)
                         (if (string=? action-str "") "waiting..." detail)))))

   ;; Layer info on top of the circle canvas
   (zcat info canvas)))

(module+ main
  (program-run (make-program (make-mouse-demo) #:alt-screen #t #:mouse 'all-motion)))
