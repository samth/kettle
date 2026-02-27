#lang racket/base

;; eyes.rkt -- Animated blinking eyes.
;; Adapted from cl-tuition examples/eyes.lisp
;; https://github.com/atgreen/cl-tuition/blob/master/examples/eyes.lisp
;;
;; Key differences from cl-tuition:
;; - Uses Kettle subscriptions (every 0.05 tick-msg) for animation instead of
;;   cl-tuition's (tui:tick 0.05) command chaining.
;; - Renders to a string vector "canvas" converted to an image via string->image,
;;   similar to cl-tuition's 2D array approach but using Racket vectors.
;; - Pattern matching on tick-msg and window-size-msg replaces cl-tuition's
;;   tick-msg-p / window-size-msg-p predicate dispatching.
;; - Immutable struct-copy updates vs. CLOS setf.
;;
;; Watch the eyes blink. Press q or Escape to quit.

(require racket/match
         racket/format
         racket/math
         racket/string
         kettle)

(define eye-width 15)
(define eye-height 12)
(define eye-spacing 40)
(define blink-frames 20)
(define open-time-min 1000)  ;; ms
(define open-time-max 4000)

(define eye-char "●")
(define bg-char " ")

(define (random-open-time)
  (+ open-time-min (random (- open-time-max open-time-min))))

(define-kettle-program eyes-demo
  #:fields ([width 80] [height 24]
            [eye-x1 0] [eye-x2 0] [eye-y 0]
            [blinking? #f] [blink-state 0]
            [last-blink 0] [open-time 0])
  #:init (lambda (self)
    (define ot (random-open-time))
    (define now (current-inexact-milliseconds))
    (define size (get-terminal-size))
    (define w (if size (car size) 80))
    (define h (if size (cdr size) 24))
    (define start-x (quotient (- w eye-spacing) 2))
    (struct-copy eyes-demo self
                 [width w] [height h]
                 [eye-x1 start-x]
                 [eye-x2 (+ start-x eye-spacing)]
                 [eye-y (quotient h 2)]
                 [open-time ot]
                 [last-blink now]))
  #:update (lambda (self msg)
    (match msg
      [(key-msg 'escape _ _) (cmd self (quit-cmd))]
      [(key-msg #\q _ _)     (cmd self (quit-cmd))]
      [(key-msg _ _ #t)      (cmd self (quit-cmd))]
      [(window-size-msg w h)
       (define start-x (quotient (- w eye-spacing) 2))
       (struct-copy eyes-demo self
                    [width w] [height h]
                    [eye-x1 start-x]
                    [eye-x2 (+ start-x eye-spacing)]
                    [eye-y (quotient h 2)])]
      [(tick-msg now)
       (define s self)
       ;; Check if should start blinking
       (define elapsed (- now (eyes-demo-last-blink s)))
       (when (and (not (eyes-demo-blinking? s))
                  (>= elapsed (eyes-demo-open-time s)))
         (set! s (struct-copy eyes-demo s [blinking? #t] [blink-state 0])))
       ;; Update blink animation
       (when (eyes-demo-blinking? s)
         (define new-bs (add1 (eyes-demo-blink-state s)))
         (set! s (struct-copy eyes-demo s [blink-state new-bs]))
         (when (>= new-bs blink-frames)
           (define new-ot
             (if (zero? (random 10))
                 300 ;; double blink
                 (random-open-time)))
           (set! s (struct-copy eyes-demo s
                                [blinking? #f]
                                [last-blink now]
                                [open-time new-ot]))))
       s]
      [_ self]))
  #:subscriptions (lambda (self)
    (list (every 0.05 tick-msg)))
  #:view (lambda (self)
    (define w (eyes-demo-width self))
    (define h (eyes-demo-height self))
    ;; Calculate current eye height
    (define current-height
      (if (not (eyes-demo-blinking? self))
          eye-height
          (let* ([bs (eyes-demo-blink-state self)]
                 [half (quotient blink-frames 2)]
                 [progress
                  (if (< bs half)
                      ;; Closing
                      (let ([p (/ (exact->inexact bs) (exact->inexact half))])
                        (- 1.0 (* p p)))
                      ;; Opening
                      (let ([p (/ (exact->inexact (- bs half)) (exact->inexact half))])
                        (* p (- 2.0 p))))])
            (max 1 (exact-floor (* eye-height progress))))))
    ;; Draw on canvas
    (define canvas (make-vector (* w h) bg-char))
    (define (draw-ellipse cx cy rx ry)
      (for ([dy (in-range (- ry) (add1 ry))])
        (define y-ratio (/ (exact->inexact dy) (exact->inexact ry)))
        (define half-width (exact-floor (* rx (sqrt (max 0.0 (- 1.0 (* y-ratio y-ratio)))))))
        (for ([dx (in-range (- half-width) (add1 half-width))])
          (define px (+ cx dx))
          (define py (+ cy dy))
          (when (and (>= px 0) (< px w) (>= py 0) (< py h))
            (vector-set! canvas (+ (* py w) px) eye-char)))))
    (draw-ellipse (eyes-demo-eye-x1 self) (eyes-demo-eye-y self) eye-width current-height)
    (draw-ellipse (eyes-demo-eye-x2 self) (eyes-demo-eye-y self) eye-width current-height)
    ;; Convert to image
    (define lines
      (for/list ([y (in-range h)])
        (apply string-append
               (for/list ([x (in-range w)])
                 (vector-ref canvas (+ (* y w) x))))))
    (styled (make-style #:foreground fg-white)
            (string-join lines "\n"))))

(module+ main
  (program-run (make-program (make-eyes-demo) #:alt-screen #t)))
