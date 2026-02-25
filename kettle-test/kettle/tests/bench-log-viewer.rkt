#lang racket/base

;; bench-log-viewer.rkt -- Performance benchmarks for the log viewer.
;; Measures construction and scroll/render times at 100K and 1M lines.

(require rackunit
         racket/format
         kettle/test
         kettle/examples/log-viewer)

;;; Helpers

(define (generate-lines n)
  (define vec (make-vector n))
  (for ([i (in-range n)])
    (vector-set!
     vec
     i
     (format "2026-02-24T12:00:00.~a INFO  [worker-~a] Processing request #~a from 192.168.1.~a"
             (~r (modulo i 1000) #:min-width 3 #:pad-string "0")
             (modulo i 8)
             i
             (modulo i 256))))
  vec)

(define (median lst)
  (define sorted (sort lst <))
  (define n (length sorted))
  (list-ref sorted (quotient n 2)))

(define (bench name thunk #:iterations [iters 50])
  (collect-garbage)
  (collect-garbage)
  (define times
    (for/list ([_ (in-range iters)])
      (define start (current-inexact-milliseconds))
      (thunk)
      (define end (current-inexact-milliseconds))
      (- end start)))
  (define med (median times))
  (printf "  ~a: ~a ms (median of ~a)\n" name (~r med #:precision '(= 1)) iters)
  med)

;;; Benchmarks

(printf "\n=== Log Viewer Benchmarks ===\n\n")

;; 1. File load benchmarks
(printf "--- Construction ---\n")

(define load-100k-ms
  (bench "Load 100K lines"
         (lambda ()
           (define vec (generate-lines 100000))
           (make-log-viewer vec "bench.log" #:width 120 #:height 40))
         #:iterations 5))

(define load-1m-ms
  (bench "Load 1M lines"
         (lambda ()
           (define vec (generate-lines 1000000))
           (make-log-viewer vec "bench.log" #:width 120 #:height 40))
         #:iterations 3))

;; Pre-build viewers for scroll benchmarks
(define lines-100k (generate-lines 100000))
(define lines-1m (generate-lines 1000000))
(define lv-100k (make-log-viewer lines-100k "bench-100k.log" #:width 120 #:height 40))
(define lv-1m (make-log-viewer lines-1m "bench-1m.log" #:width 120 #:height 40))
(define lv-1m-large (make-log-viewer lines-1m "bench-1m.log" #:width 200 #:height 60))

;; 2. Scroll + render at 100K lines
(printf "\n--- Scroll render at 100K lines (120x40) ---\n")

(define scroll-100k-ms
  (bench "Scroll down + view"
         (lambda ()
           (define-values (lv2 _) (update lv-100k (key-msg #\j #f #f)))
           (view lv2))))

;; 3. Scroll + render at 1M lines
(printf "\n--- Scroll render at 1M lines (120x40) ---\n")

(define scroll-1m-ms
  (bench "Scroll down + view"
         (lambda ()
           (define-values (lv2 _) (update lv-1m (key-msg #\j #f #f)))
           (view lv2))))

;; 4. Page-down at 1M lines (200x60)
(printf "\n--- Page-down at 1M lines (200x60) ---\n")

(define page-1m-ms
  (bench "Page down + view"
         (lambda ()
           (define-values (lv2 _) (update lv-1m-large (key-msg #\space #f #f)))
           (view lv2))))

;; 5. Jump-to-bottom at 1M lines
(printf "\n--- Jump to bottom at 1M lines (120x40) ---\n")

(define jump-1m-ms
  (bench "Jump to bottom (G) + view"
         (lambda ()
           (define-values (lv2 _) (update lv-1m (key-msg #\G #f #f)))
           (view lv2))))

;; Assertions
(printf "\n--- Assertions ---\n")

(test-case "bench: load 100K lines < 2s"
  (check-true (< load-100k-ms 2000) (format "Load 100K took ~a ms, expected < 2000" load-100k-ms)))

(test-case "bench: scroll at 100K < 66ms (>15 fps)"
  (check-true (< scroll-100k-ms 66) (format "Scroll 100K took ~a ms, expected < 66" scroll-100k-ms)))

(test-case "bench: scroll at 1M < 66ms (>15 fps)"
  (check-true (< scroll-1m-ms 66) (format "Scroll 1M took ~a ms, expected < 66" scroll-1m-ms)))

(test-case "bench: page-down at 1M < 66ms"
  (check-true (< page-1m-ms 66) (format "Page-down 1M took ~a ms, expected < 66" page-1m-ms)))

(test-case "bench: jump-to-bottom at 1M < 66ms"
  (check-true (< jump-1m-ms 66) (format "Jump-to-bottom 1M took ~a ms, expected < 66" jump-1m-ms)))

(printf "\nAll benchmarks passed.\n")
