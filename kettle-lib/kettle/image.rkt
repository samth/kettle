#lang racket/base

;; kettle/image -- re-exports the image type and composition functions

(require racket/contract
         "private/image.rkt"
         "private/renderer.rkt"
         "private/style.rkt")

;; Struct types and predicates (no contracts needed)
(provide (all-from-out "private/image.rkt")

         ;; Contracted functions
         (contract-out [image->string (-> image? string?)]
                       ;; Re-export contracted constructors for users importing kettle/image
                       ;; The smart constructors from image.rkt are re-exported directly above,
                       ;; so we only add contracts for renderer utilities here.
                       [render-image! (-> renderer? image? void?)]))
