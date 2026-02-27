#lang racket/base

;; showcase.rkt -- Animated feature showcase.
;; Adapted from cl-tuition examples/showcase.lisp
;; https://github.com/atgreen/cl-tuition/blob/master/examples/showcase.lisp
;;
;; Key differences from cl-tuition:
;; - Uses define-kettle-program with #:subscriptions (every 0.1 tick-msg) for
;;   the animation loop, replacing cl-tuition's manual (sleep 0.1) command chain.
;; - View builds an algebraic image tree with vcat/hcat/styled/bold, which the
;;   renderer diffs into minimal terminal updates, vs. cl-tuition's join-vertical
;;   over rendered styled strings.
;; - Progress bar is built inline with make-string for filled/empty regions,
;;   rather than using a separate progress component.
;; - Auto-exits when progress reaches 100%.
;;
;; Watch the progress bar fill. Press q to quit early.

(require racket/match
         racket/format
         kettle/program
         kettle/image
         kettle/style
         kettle/border)

(define-kettle-program showcase
  #:fields ([progress 0.0] [tick-count 0] [status "Initializing..."] [running? #t])
  #:update (lambda (self msg)
    (match msg
      [(key-msg #\q _ _) (cmd self (quit-cmd))]
      [(key-msg _ _ #t)  (cmd self (quit-cmd))]
      [(tick-msg _)
       (define new-progress (min 1.0 (+ (showcase-progress self) 0.01)))
       (define new-status
         (cond
           [(< new-progress 0.25) "Loading resources..."]
           [(< new-progress 0.50) "Compiling modules..."]
           [(< new-progress 0.75) "Building world..."]
           [(< new-progress 1.0)  "Finalizing..."]
           [else "Complete!"]))
       (define new-self
         (struct-copy showcase self
                      [progress new-progress]
                      [tick-count (add1 (showcase-tick-count self))]
                      [status new-status]
                      [running? (< new-progress 1.0)]))
       (if (>= new-progress 1.0)
           (cmd new-self (quit-cmd))
           new-self)]
      [_ self]))
  #:subscriptions (lambda (self)
    (if (showcase-running? self)
        (list (every 0.1 tick-msg))
        '()))
  #:view (lambda (self)
    (define width 60)
    (define pct (showcase-progress self))
    (define filled (exact-floor (* pct width)))
    (define empty (- width filled))

    (define title
      (string->image
       (render-border
        (render-styled (make-style #:foreground fg-bright-magenta #:bold #t)
                       "KETTLE SHOWCASE")
        border-rounded
        #:fg-color fg-bright-magenta)))

    (define subtitle
      (styled (make-style #:foreground fg-bright-cyan #:italic #t)
              "A Racket library for building Terminal User Interfaces"))

    (define progress-label
      (styled (make-style #:foreground fg-bright-yellow #:bold #t)
              (format "Progress: ~a%" (exact-floor (* pct 100)))))

    (define bar
      (hcat 'top
            (text "  ")
            (styled (make-style #:background bg-bright-green)
                    (make-string filled #\space))
            (styled (make-style #:background bg-bright-black)
                    (make-string empty #\space))))

    (define status-text
      (styled (make-style #:foreground fg-bright-white) (showcase-status self)))

    (define features
      (apply vcat 'left
             (for/list ([f '("The Elm Architecture (TEA)"
                             "Algebraic image composition"
                             "ANSI styling & truecolor"
                             "Borders & layout constraints"
                             "Mouse & keyboard support"
                             "Reusable components")])
               (styled (make-style #:foreground fg-green)
                       (format "  ✓ ~a" f)))))

    (define stats
      (styled (make-style #:foreground fg-bright-black)
              (format "Ticks: ~a" (showcase-tick-count self))))

    (define footer
      (styled (make-style #:foreground fg-bright-black #:italic #t)
              "Press 'q' to quit"))

    (vcat 'left
          ""
          title
          subtitle
          ""
          progress-label
          bar
          status-text
          ""
          features
          ""
          stats
          ""
          footer
          "")))

(module+ main
  (program-run (make-program (make-showcase) #:alt-screen #t)))
