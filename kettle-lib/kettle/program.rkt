#lang racket/base

;; kettle/program -- re-exports the TEA program runner, protocol, and macros

(require racket/contract
         "private/protocol.rkt"
         "private/program.rkt")

(provide (all-from-out "private/protocol.rkt")

         ;; Contracted exports from program.rkt
         (contract-out [make-program
                        (->* (tea-model?)
                             (#:alt-screen boolean?
                              #:mouse (or/c 'cell-motion 'all-motion #f)
                              #:show-fps boolean?)
                             program?)]
                       [program-run (-> program? void?)]
                       [program-send (-> program? msg? void?)]
                       [program-quit (-> program? void?)]
                       [program-kill (-> program? void?)]
                       [program-stop (-> program? void?)]
                       [program-show-fps! (-> program? boolean? void?)])

         ;; Non-contracted re-exports
         (struct-out program)
         current-program
         define-tea-program)
