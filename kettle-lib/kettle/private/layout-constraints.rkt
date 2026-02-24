#lang racket/base

;; layout-constraints.rkt
;;
;; SPDX-License-Identifier: MIT
;;
;; Ratatui-inspired layout constraint solver for splitting terminal space.

(require racket/list
         racket/match
         racket/math)

;; Constraint types
(provide (struct-out constraint)
         fixed
         percent
         fill
         min-size
         max-size
         ratio

         ;; Solvers
         split-horizontal
         split-vertical)

;;; Constraint types

(struct constraint (type value) #:transparent)

(define (fixed n)
  (constraint 'fixed n))
(define (percent p)
  (constraint 'percent p))
(define (fill)
  (constraint 'fill 1))
(define (min-size n c)
  (constraint 'min (list n c)))
(define (max-size n c)
  (constraint 'max (list n c)))
(define (ratio n d)
  (constraint 'ratio (list n d)))

;;; Solver

(define (solve-constraints total constraints)
  ;; First pass: allocate fixed, percent, ratio constraints
  (define initial
    (for/list ([c (in-list constraints)])
      (match (constraint-type c)
        ['fixed (constraint-value c)]
        ['percent (exact-floor (* total (/ (constraint-value c) 100)))]
        ['ratio
         (match-define (list n d) (constraint-value c))
         (exact-floor (* total (/ n d)))]
        ['fill 0]
        ['min
         (match-define (list min-val inner) (constraint-value c))
         (define inner-size (car (solve-constraints total (list inner))))
         (max min-val inner-size)]
        ['max
         (match-define (list max-val inner) (constraint-value c))
         (define inner-size (car (solve-constraints total (list inner))))
         (min max-val inner-size)])))

  ;; Count fills
  (define fill-count (for/sum ([c (in-list constraints)]) (if (eq? (constraint-type c) 'fill) 1 0)))

  (define used (apply + initial))
  (define remaining (max 0 (- total used)))

  (if (zero? fill-count)
      initial
      (let ()
        (define per-fill (quotient remaining fill-count))
        (define extra (remainder remaining fill-count))
        (define fill-idx 0)
        (for/list ([c (in-list constraints)]
                   [size (in-list initial)])
          (if (eq? (constraint-type c) 'fill)
              (let ([result (+ per-fill (if (< fill-idx extra) 1 0))])
                (set! fill-idx (add1 fill-idx))
                result)
              size)))))

(define (split-horizontal total . constraints)
  (solve-constraints total constraints))

(define (split-vertical total . constraints)
  (solve-constraints total constraints))
