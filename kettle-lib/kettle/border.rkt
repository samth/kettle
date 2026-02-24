#lang racket/base

;; kettle/border -- re-exports border definitions and rendering

(require "private/borders.rkt")

(provide (all-from-out "private/borders.rkt"))
