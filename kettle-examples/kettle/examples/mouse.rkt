#lang racket/base

;; mouse.rkt -- Mouse event tracking demo.
;; Adapted from cl-tuition examples/mouse.lisp
;; https://github.com/atgreen/cl-tuition/blob/master/examples/mouse.lisp
;;
;; Key differences from cl-tuition:
;; - Kettle uses a typed mouse-event hierarchy (mouse-press-event, mouse-release-event,
;;   mouse-move-event, mouse-scroll-event, mouse-drag-event) with pattern matching,
;;   replacing cl-tuition's generic mouse-msg with action slot (:press, :release, etc.)
;; - Mouse is enabled via #:mouse 'all-motion on make-program, vs. cl-tuition's
;;   :mouse :all-motion on make-program.
;; - The model uses immutable struct-copy updates.
;;
;; Move the mouse, click, scroll. Press q to quit.

(require racket/match
         racket/format
         kettle/program
         kettle/image
         kettle/style)

(struct mouse-state (x y button action scroll-dir)
  #:transparent)

(define-kettle-program mouse-demo
  #:fields ([x 0] [y 0] [button ""] [action ""] [scroll-dir ""])
  #:update (lambda (self msg)
    (match msg
      ;; Quit
      [(key-msg #\q _ _) (cmd self (quit-cmd))]
      [(key-msg _ _ #t)  (cmd self (quit-cmd))]
      ;; Mouse press
      [(mouse-press-event x y _ _ _ button)
       (struct-copy mouse-demo self
                    [x x] [y y]
                    [button (format "~a" button)]
                    [action "press"]
                    [scroll-dir ""])]
      ;; Mouse release
      [(mouse-release-event x y _ _ _ button)
       (struct-copy mouse-demo self
                    [x x] [y y]
                    [button (format "~a" button)]
                    [action "release"]
                    [scroll-dir ""])]
      ;; Mouse drag
      [(mouse-drag-event x y _ _ _ button)
       (struct-copy mouse-demo self
                    [x x] [y y]
                    [button (format "~a" button)]
                    [action "drag"]
                    [scroll-dir ""])]
      ;; Mouse move
      [(mouse-move-event x y _ _ _)
       (struct-copy mouse-demo self
                    [x x] [y y]
                    [action "move"]
                    [button ""]
                    [scroll-dir ""])]
      ;; Mouse scroll
      [(mouse-scroll-event x y _ _ _ dir _)
       (struct-copy mouse-demo self
                    [x x] [y y]
                    [action "scroll"]
                    [scroll-dir (format "~a" dir)]
                    [button ""])]
      [_ self]))
  #:view (lambda (self)
    (define action-str (mouse-demo-action self))
    (define detail
      (cond
        [(string=? action-str "scroll")
         (format "scroll ~a" (mouse-demo-scroll-dir self))]
        [(string=? (mouse-demo-button self) "")
         action-str]
        [else (format "~a ~a" action-str (mouse-demo-button self))]))

    (vcat 'left
          ""
          (bold "  Mouse Tracking Demo")
          ""
          (hcat 'top (text "  Position: ")
                     (styled (make-style #:foreground fg-bright-cyan)
                             (format "(~a, ~a)" (mouse-demo-x self) (mouse-demo-y self))))
          (hcat 'top (text "  Action:   ")
                     (styled (make-style #:foreground fg-bright-yellow)
                             (if (string=? action-str "") "waiting..." detail)))
          ""
          (styled (make-style #:foreground fg-bright-black)
                  "  Move/click/scroll the mouse. Press q to quit."))))

(module+ main
  (program-run (make-program (make-mouse-demo) #:alt-screen #t #:mouse 'all-motion)))
