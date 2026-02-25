#lang racket/base

;; kettle/test -- headless TEA test harness
;;
;; Provides a synchronous test runner for Kettle programs.
;; No terminal or threading dependencies.

(require "private/test-program.rkt"
         "private/protocol.rkt"
         "private/image.rkt"
         "private/renderer.rkt")

;; Re-export the test harness API
;; Construction
(provide make-test-program
         make-test-program/run

         ;; Event injection
         test-program-send
         test-program-press
         test-program-type
         test-program-resize

         ;; State inspection
         test-program-model
         test-program-value
         test-program-view
         test-program-view-string
         test-program-done?
         test-program-history
         test-program-subscriptions

         ;; Rackunit checks
         check-test-program-contains
         check-test-program-done
         check-test-program-running

         ;; Re-export commonly needed protocol types for test authors
         (all-from-out "private/protocol.rkt")
         image?
         image->string)
