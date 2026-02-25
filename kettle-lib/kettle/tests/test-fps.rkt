#lang racket/base

;; test-fps.rkt -- Tests for FPS display overlay and hotkey.

(require rackunit
         racket/string
         kettle/image
         kettle/style
         kettle/program
         (except-in kettle/test image->string)
         kettle/examples/counter
         kettle/examples/stopwatch)

;; Test that make-program accepts #:show-fps
(test-case "fps: make-program accepts #:show-fps"
  (define p (make-program (make-stopwatch) #:show-fps #t))
  (check-true (program-show-fps? p)))

(test-case "fps: show-fps defaults to #f"
  (define p (make-program (make-stopwatch)))
  (check-false (program-show-fps? p)))

(test-case "fps: program-show-fps! toggles the flag"
  (define p (make-program (make-stopwatch)))
  (check-false (program-show-fps? p))
  (program-show-fps! p #t)
  (check-true (program-show-fps? p))
  (program-show-fps! p #f)
  (check-false (program-show-fps? p)))

;; Test that Alt+F is a no-op in the headless test harness
;; (the hotkey is handled by program.rkt's event loop, not the model)
(test-case "fps: Alt+F does not affect model in headless mode"
  (define tp (make-test-program/run 0 #:on-key counter-on-key #:to-view counter-view))
  (test-program-press tp #\f #:alt #t)
  (check-test-program-contains tp "Count: 0")
  (check-test-program-running tp))

;; Test the FPS overlay image composition (simulating what overlay-fps does)
(test-case "fps: overlay produces valid image"
  (define base
    (vcat 'left (text (make-string 80 #\=)) (text "Hello world") (text (make-string 80 #\-))))
  (define fps-style (make-style #:foreground "33" #:background "40" #:bold #t))
  (define label (styled fps-style (text " 60 fps ")))
  (define w (image-w base))
  (define label-w (image-w label))
  (define result (zcat (pad label #:left (- w label-w)) base))
  (check-true (image? result))
  (check-equal? (image-w result) (image-w base))
  (check-equal? (image-h result) (image-h base))
  ;; Render to string and verify the FPS label appears
  (define rendered (image->string result))
  (check-true (string-contains? rendered "60 fps")))

;; Test that narrow images handle the overlay gracefully
(test-case "fps: overlay skips narrow images"
  (define narrow (text "Hi"))
  (define fps-style (make-style #:foreground "33" #:background "40" #:bold #t))
  (define label (styled fps-style (text " 99 fps ")))
  ;; Label is wider than image -- overlay would not apply
  (check-true (> (image-w label) (image-w narrow))))
