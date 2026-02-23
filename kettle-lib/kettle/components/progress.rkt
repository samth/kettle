#lang racket/base

;; components/progress.rkt
;;
;; SPDX-License-Identifier: MIT
;;
;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;; Racket port
;;
;; Progress bar component

(provide (struct-out progress)
         make-progress
         progress-init
         progress-update
         progress-view
         progress-set-percent!
         progress-increment!)

;;; Progress bar model
(struct progress
  ([percent #:mutable]
   width
   show-percentage?
   full-char
   empty-char)
  #:transparent)

(define (make-progress #:percent [percent 0.0]
                       #:width [width 40]
                       #:show-percentage [show-pct #t]
                       #:full-char [full-char #\█]
                       #:empty-char [empty-char #\░])
  (progress (max 0.0 (min 1.0 percent))
            width show-pct full-char empty-char))

;;; Component operations

(define (progress-init pb)
  (values pb #f))

(define (progress-update pb msg)
  (values pb #f))

(define (progress-view pb)
  (define pct (max 0.0 (min 1.0 (progress-percent pb))))
  (define w (progress-width pb))
  (define filled (exact-floor (* pct w)))
  (define empty (- w filled))
  (define bar
    (format "[~a~a]"
            (make-string filled (progress-full-char pb))
            (make-string empty (progress-empty-char pb))))
  (if (progress-show-percentage? pb)
      (format "~a ~a%" bar (exact-floor (* pct 100)))
      bar))

;;; Helper functions

(define (progress-set-percent! pb pct)
  (set-progress-percent! pb (max 0.0 (min 1.0 pct))))

(define (progress-increment! pb amount)
  (progress-set-percent! pb (+ (progress-percent pb) amount)))
