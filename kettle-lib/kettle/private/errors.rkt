#lang racket/base

;; errors.rkt
;;
;; SPDX-License-Identifier: MIT
;;
;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;; Racket port
;;
;; Condition types and error handling hooks

(provide (struct-out exn:fail:kettle)
         (struct-out exn:fail:kettle:terminal)
         (struct-out exn:fail:kettle:terminal:operation)
         (struct-out exn:fail:kettle:input)
         current-error-handler
         handle-kettle-error)

;; Exception hierarchy
(struct exn:fail:kettle exn:fail () #:transparent)
(struct exn:fail:kettle:terminal exn:fail:kettle (reason) #:transparent)
(struct exn:fail:kettle:terminal:operation exn:fail:kettle:terminal (operation) #:transparent)
(struct exn:fail:kettle:input exn:fail:kettle (reason) #:transparent)

;; Configurable error handler
;; Takes (where condition) where `where` is a symbol like 'event-loop, 'input-loop, etc.
(define current-error-handler
  (make-parameter
   (lambda (where condition)
     (log-error "kettle ~a: ~a" where (exn-message condition)))))

(define (handle-kettle-error where condition)
  ((current-error-handler) where condition))
