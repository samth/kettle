#lang racket/base

;; components/progress.rkt
;;
;; SPDX-License-Identifier: MIT
;;
;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;; Racket port
;;
;; Progress bar component

(require racket/math
         "../private/protocol.rkt"
         "../private/image.rkt")

(provide (struct-out progress)
         make-progress
         progress-set-percent
         progress-increment)

;;; Progress bar model -- implements gen:tea-model
(struct progress (percent width show-percentage? full-char empty-char)
  #:transparent
  #:methods gen:tea-model
  [(define (init pb)
     (values pb #f))
   (define (update pb msg)
     (values pb #f))
   (define (view pb)
     (define pct (max 0.0 (min 1.0 (progress-percent pb))))
     (define w (progress-width pb))
     (define filled (exact-floor (* pct w)))
     (define empty (- w filled))
     (define bar
       (format "[~a~a]"
               (make-string filled (progress-full-char pb))
               (make-string empty (progress-empty-char pb))))
     (if (progress-show-percentage? pb)
         (text (format "~a ~a%" bar (exact-floor (* pct 100))))
         (text bar)))])

(define (make-progress #:percent [percent 0.0]
                       #:width [width 40]
                       #:show-percentage [show-pct #t]
                       #:full-char [full-char #\█]
                       #:empty-char [empty-char #\░])
  (progress (max 0.0 (min 1.0 percent)) width show-pct full-char empty-char))

;;; Functional update helpers (return new progress)
(define (progress-set-percent pb pct)
  (struct-copy progress pb [percent (max 0.0 (min 1.0 pct))]))

(define (progress-increment pb amount)
  (progress-set-percent pb (+ (progress-percent pb) amount)))
