#lang racket/base

;; spring-animation.rkt -- Physics-based spring animation.
;; Adapted from cl-tuition examples/spring-animation.lisp
;; https://github.com/atgreen/cl-tuition/blob/master/examples/spring-animation.lisp
;;
;; Key differences from cl-tuition:
;; - cl-tuition has built-in spring physics (make-spring-smooth, make-spring-bouncy,
;;   make-spring-gentle, spring-update). Kettle does not currently include a
;;   spring physics module.
;; - This example implements the spring physics inline using the damped harmonic
;;   oscillator equation, demonstrating that user-space physics is straightforward
;;   within Kettle's subscription-based animation model.
;; - Uses (every 0.016 tick-msg) for ~60 FPS animation instead of cl-tuition's
;;   manual (sleep 0.016) command loops.
;;
;; Press Space to change target, q to quit.

(require racket/match
         racket/format
         racket/math
         kettle/program
         kettle/image
         kettle/style)

;; Simple spring physics: critically-damped, under-damped, over-damped
(struct spring (stiffness damping) #:transparent)

(define spring-smooth (spring 120.0 22.0))   ;; critically damped
(define spring-bouncy (spring 120.0 8.0))    ;; under-damped (bouncy)
(define spring-gentle (spring 40.0 16.0))    ;; over-damped (slow)

(define (spring-step sp x vel target dt)
  (define stiff (spring-stiffness sp))
  (define damp (spring-damping sp))
  (define force (- (* stiff (- target x)) (* damp vel)))
  (define new-vel (+ vel (* force dt)))
  (define new-x (+ x (* new-vel dt)))
  (values new-x new-vel))

(define-kettle-program spring-demo
  #:fields ([x1 0.0] [v1 0.0]
            [x2 0.0] [v2 0.0]
            [x3 0.0] [v3 0.0]
            [target 50.0]
            [tick-count 0])
  #:update (lambda (self msg)
    (match msg
      [(key-msg #\q _ _) (cmd self (quit-cmd))]
      [(key-msg _ _ #t)  (cmd self (quit-cmd))]
      [(key-msg #\space _ _)
       (define new-target (if (> (spring-demo-target self) 25.0) 10.0 50.0))
       (struct-copy spring-demo self [target new-target])]
      [(tick-msg _)
       (define dt 0.016)
       (define tgt (spring-demo-target self))
       (define-values (nx1 nv1) (spring-step spring-smooth (spring-demo-x1 self)
                                             (spring-demo-v1 self) tgt dt))
       (define-values (nx2 nv2) (spring-step spring-bouncy (spring-demo-x2 self)
                                             (spring-demo-v2 self) tgt dt))
       (define-values (nx3 nv3) (spring-step spring-gentle (spring-demo-x3 self)
                                             (spring-demo-v3 self) tgt dt))
       (define tc (add1 (spring-demo-tick-count self)))
       ;; Auto-change target every ~3 sec (180 ticks)
       (define new-target
         (if (zero? (modulo tc 180))
             (if (> tgt 25.0) 10.0 50.0)
             tgt))
       (struct-copy spring-demo self
                    [x1 nx1] [v1 nv1]
                    [x2 nx2] [v2 nv2]
                    [x3 nx3] [v3 nv3]
                    [target new-target]
                    [tick-count tc])]
      [_ self]))
  #:subscriptions (lambda (self)
    (list (every 0.016 tick-msg)))
  #:view (lambda (self)
    (define (render-track pos label width)
      (define clamped (max 0 (min (exact-round pos) width)))
      (define before (make-string clamped #\-))
      (define after (make-string (- width clamped) #\-))
      (hcat 'top
            (text (format "~a: " label))
            (text before)
            (styled (make-style #:foreground fg-bright-cyan #:bold #t) "●")
            (text after)))
    (define width 50)
    (vcat 'left
          ""
          (bold "  Spring Animation Demo")
          ""
          (render-track (spring-demo-x1 self) "Smooth (critical)  " width)
          ""
          (render-track (spring-demo-x2 self) "Bouncy (under-damp)" width)
          ""
          (render-track (spring-demo-x3 self) "Gentle (over-damp) " width)
          ""
          (hcat 'top (text "  Target: ")
                     (styled (make-style #:foreground fg-bright-yellow)
                             (format "~a" (exact-round (spring-demo-target self)))))
          ""
          (styled (make-style #:foreground fg-bright-black)
                  "  Press SPACE to change target, 'q' to quit"))))

(module+ main
  (program-run (make-program (make-spring-demo) #:alt-screen #t)))
