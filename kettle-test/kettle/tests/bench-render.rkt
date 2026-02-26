#lang racket/base

;; bench-render.rkt -- Rendering pipeline benchmarks.
;; Measures paint!, cell-buffer->string, and full render cycle times
;; for increasingly complex image trees to verify the system can sustain
;; at least 15 fps (66ms per frame) for typical TUI layouts.
;;
;; Run: racket -y kettle-test/kettle/tests/bench-render.rkt

(require racket/format
         racket/string
         racket/list
         kettle
         (prefix-in vp: kettle/components/viewport))

;; Measure how long body takes in milliseconds, return (cons result ms)
(define-syntax-rule (time-ms body ...)
  (let ([start (current-inexact-milliseconds)])
    (define result
      (begin
        body ...))
    (cons result (- (current-inexact-milliseconds) start))))

;; Run body n times and return the median time in ms
(define-syntax-rule (bench n body ...)
  (let ([times (for/list ([_ (in-range n)])
                 (define t (time-ms body ...))
                 (cdr t))])
    (define sorted (sort times <))
    (list-ref sorted (quotient (length sorted) 2))))

(define WIDTH 120)
(define HEIGHT 40)
(define ITERS 50)

;; Build a simple text image: one text node per line
(define (make-simple-text-image w h)
  (apply vcat
         'left
         (for/list ([i (in-range h)])
           (text (make-string w (integer->char (+ 65 (modulo i 26))))))))

;; Build a styled text image: each line has a different style
(define (make-styled-text-image w h)
  (apply vcat
         'left
         (for/list ([i (in-range h)])
           (define sty
             (make-style #:bold (even? i)
                         #:foreground (list-ref (list fg-red fg-green fg-yellow fg-blue fg-magenta fg-cyan) (modulo i 6))
                         #:background (if (even? i) bg-black #f)))
           (styled sty (text (make-string w (integer->char (+ 65 (modulo i 26)))))))))

;; Build a complex nested layout: header + viewport + footer with borders
(define (make-complex-layout w h)
  (define header-style (make-style #:bold #t #:reverse #t))
  (define footer-style (make-style #:faint #t))
  (define content-lines
    (string-join (for/list ([i (in-range 200)])
                   (format "Line ~a: ~a" (add1 i) (make-string (- w 20) #\x)))
                 "\n"))
  (define vp (vp:make-viewport #:width w #:height (- h 3) #:content content-lines))
  (define vp-img (view vp))
  (vcat 'left
        (styled header-style (text (make-string w #\=)))
        vp-img
        (styled footer-style (text (make-string w #\-)))))

;; Build a deeply nested image tree (stress test for paint! recursion)
(define (make-deep-nested-image w h depth)
  (define base (text (make-string w #\A)))
  (for/fold ([img base]) ([_ (in-range depth)])
    (styled (make-style #:bold #t) (pad img #:left 0 #:top 0))))

;; Build a wide hcat (many columns)
(define (make-wide-hcat cols h)
  (define col-w (max 1 (quotient WIDTH cols)))
  (apply hcat
         'top
         (for/list ([c (in-range cols)])
           (apply vcat
                  'left
                  (for/list ([r (in-range h)])
                    (text (make-string col-w (integer->char (+ 65 (modulo (+ c r) 26))))))))))

;; Full render cycle: build image, paint, emit string
(define (full-render-cycle img w h)
  (define buf (make-cell-buffer w h))
  (paint! buf img 0 0)
  (cell-buffer->string buf))

(define (run-benchmarks)
  (printf "Kettle Rendering Benchmarks (~ax~a, ~a iterations, median time)\n" WIDTH HEIGHT ITERS)
  (printf "Target: <~ams per frame (>~a fps)\n\n" (quotient 1000 15) 15)

  ;; Simple text
  (define simple-img (make-simple-text-image WIDTH HEIGHT))
  (define simple-ms (bench ITERS (full-render-cycle simple-img WIDTH HEIGHT)))
  (printf "Simple text (~ax~a):         ~ams  (~a fps)\n"
          WIDTH
          HEIGHT
          (~r simple-ms #:precision '(= 1))
          (~r (/ 1000.0 simple-ms) #:precision '(= 0)))

  ;; Styled text
  (define styled-img (make-styled-text-image WIDTH HEIGHT))
  (define styled-ms (bench ITERS (full-render-cycle styled-img WIDTH HEIGHT)))
  (printf "Styled text (~ax~a):         ~ams  (~a fps)\n"
          WIDTH
          HEIGHT
          (~r styled-ms #:precision '(= 1))
          (~r (/ 1000.0 styled-ms) #:precision '(= 0)))

  ;; Complex layout (header + viewport + footer)
  (define complex-img (make-complex-layout WIDTH HEIGHT))
  (define complex-ms (bench ITERS (full-render-cycle complex-img WIDTH HEIGHT)))
  (printf "Complex layout (~ax~a):      ~ams  (~a fps)\n"
          WIDTH
          HEIGHT
          (~r complex-ms #:precision '(= 1))
          (~r (/ 1000.0 complex-ms) #:precision '(= 0)))

  ;; Deep nesting (50 levels)
  (define deep-img (make-deep-nested-image WIDTH HEIGHT 50))
  (define deep-ms (bench ITERS (full-render-cycle deep-img WIDTH HEIGHT)))
  (printf "Deep nesting (50 levels):    ~ams  (~a fps)\n"
          (~r deep-ms #:precision '(= 1))
          (~r (/ 1000.0 deep-ms) #:precision '(= 0)))

  ;; Wide hcat (10 columns)
  (define wide-img (make-wide-hcat 10 HEIGHT))
  (define wide-ms (bench ITERS (full-render-cycle wide-img WIDTH HEIGHT)))
  (printf "Wide hcat (10 cols):         ~ams  (~a fps)\n"
          (~r wide-ms #:precision '(= 1))
          (~r (/ 1000.0 wide-ms) #:precision '(= 0)))

  ;; Large terminal (200x60)
  (define large-w 200)
  (define large-h 60)
  (define large-img (make-styled-text-image large-w large-h))
  (define large-ms (bench ITERS (full-render-cycle large-img large-w large-h)))
  (printf "Large terminal (~ax~a):     ~ams  (~a fps)\n"
          large-w
          large-h
          (~r large-ms #:precision '(= 1))
          (~r (/ 1000.0 large-ms) #:precision '(= 0)))

  ;; Paint-only benchmark (no string emission)
  (define paint-buf (make-cell-buffer WIDTH HEIGHT))
  (define paint-ms (bench ITERS (paint! paint-buf complex-img 0 0)))
  (printf "\nPaint-only (complex):        ~ams\n" (~r paint-ms #:precision '(= 1)))

  ;; Emit-only benchmark (no painting)
  (define emit-buf (make-cell-buffer WIDTH HEIGHT))
  (paint! emit-buf styled-img 0 0)
  (define emit-ms (bench ITERS (cell-buffer->string emit-buf)))
  (printf "Emit-only (styled):          ~ams\n" (~r emit-ms #:precision '(= 1)))

  ;; image->string end-to-end
  (define e2e-ms (bench ITERS (image->string complex-img)))
  (printf "image->string (complex):     ~ams  (~a fps)\n"
          (~r e2e-ms #:precision '(= 1))
          (~r (/ 1000.0 e2e-ms) #:precision '(= 0)))

  (newline)

  ;; Verdict
  (define worst-ms (max simple-ms styled-ms complex-ms deep-ms wide-ms large-ms))
  (define worst-fps (/ 1000.0 worst-ms))
  (if (>= worst-fps 15.0)
      (printf "PASS: Worst case ~a fps (>= 15 fps target)\n" (~r worst-fps #:precision '(= 0)))
      (printf "FAIL: Worst case ~a fps (< 15 fps target)\n" (~r worst-fps #:precision '(= 0)))))

(module+ main
  (run-benchmarks))

(module+ test
  (require rackunit)
  (run-benchmarks)

  ;; Assert that all scenarios meet the 15 fps target
  (define (check-fps label img w h)
    (define ms (bench 20 (full-render-cycle img w h)))
    (define fps (/ 1000.0 ms))
    (check-true (>= fps 15.0) (format "~a: ~a fps (need >= 15)" label (~r fps #:precision '(= 1)))))

  (check-fps "simple text" (make-simple-text-image WIDTH HEIGHT) WIDTH HEIGHT)
  (check-fps "styled text" (make-styled-text-image WIDTH HEIGHT) WIDTH HEIGHT)
  (check-fps "complex layout" (make-complex-layout WIDTH HEIGHT) WIDTH HEIGHT)
  (check-fps "deep nesting" (make-deep-nested-image WIDTH HEIGHT 50) WIDTH HEIGHT)
  (check-fps "wide hcat" (make-wide-hcat 10 HEIGHT) WIDTH HEIGHT)
  (check-fps "large terminal" (make-styled-text-image 200 60) 200 60))
