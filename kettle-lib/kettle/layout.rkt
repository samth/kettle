#lang racket/base

;; kettle/layout -- re-exports layout utilities and constraint solver

(require racket/contract
         racket/math
         "private/layout.rkt"
         "private/layout-constraints.rkt")

(provide (all-from-out "private/layout.rkt")

         ;; Constraint types
         (struct-out constraint)

         ;; Contracted constraint constructors
         (contract-out
          [fixed (-> natural-number/c constraint?)]
          [percent (-> (and/c real? (between/c 0 100)) constraint?)]
          [fill (-> constraint?)]
          [min-size (-> natural-number/c constraint? constraint?)]
          [max-size (-> natural-number/c constraint? constraint?)]
          [ratio (-> (and/c exact-integer? positive?) (and/c exact-integer? positive?) constraint?)]
          [split-horizontal
           (->* (natural-number/c) #:rest (listof constraint?) (listof natural-number/c))]
          [split-vertical
           (->* (natural-number/c) #:rest (listof constraint?) (listof natural-number/c))]))
