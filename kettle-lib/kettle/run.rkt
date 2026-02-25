#lang racket/base

;; run.rkt
;;
;; SPDX-License-Identifier: MIT
;;
;; Simple big-bang-style entry point for Kettle TUI programs.
;; The model can be any value -- no need for gen:kettle-model.

(require racket/contract
         racket/match
         "private/protocol.rkt"
         "private/program.rkt"
         "private/image.rkt")

(provide (contract-out [run
                        (->* (any/c)
                             (#:on-key (or/c #f (-> any/c key-msg? any/c))
                                       #:on-msg (or/c #f (-> any/c msg? any/c))
                                       #:to-view (or/c #f (-> any/c image?))
                                       #:stop-when (or/c #f (-> any/c boolean?))
                                       #:alt-screen boolean?
                                       #:mouse (or/c 'cell-motion 'all-motion #f)
                                       #:show-fps boolean?
                                       #:kitty-keyboard boolean?)
                             void?)])
         on-key
         on-msg
         to-view
         stop-when)

;; Clause markers (used as syntax identifiers for matching in `run`)
(define-syntax on-key
  (syntax-rules ()
    [(_ handler) handler]))

(define-syntax on-msg
  (syntax-rules ()
    [(_ handler) handler]))

(define-syntax to-view
  (syntax-rules ()
    [(_ handler) handler]))

(define-syntax stop-when
  (syntax-rules ()
    [(_ pred) pred]))

;; Internal wrapper implementing gen:kettle-model around a plain value.
(struct run-model (value on-key-fn on-msg-fn view-fn stop-fn)
  #:methods gen:kettle-model
  [(define (init m) m)
   (define (update m msg)
     (cond
       ;; Check stop condition before processing
       [(and (run-model-stop-fn m) ((run-model-stop-fn m) (run-model-value m))) (cmd m (quit-cmd))]
       ;; Key messages go to on-key handler
       [(and (key-msg? msg) (run-model-on-key-fn m))
        (define-values (new-val cmd*) (extract-update-result ((run-model-on-key-fn m) (run-model-value m) msg)))
        (define new-model (struct-copy run-model m [value new-val]))
        ;; Check stop after update
        (if (and (run-model-stop-fn m) ((run-model-stop-fn m) new-val))
            (cmd new-model (quit-cmd))
            (if cmd* (cmd new-model cmd*) new-model))]
       ;; All other messages go to on-msg handler
       [(run-model-on-msg-fn m)
        (define-values (new-val cmd*) (extract-update-result ((run-model-on-msg-fn m) (run-model-value m) msg)))
        (define new-model (struct-copy run-model m [value new-val]))
        (if (and (run-model-stop-fn m) ((run-model-stop-fn m) new-val))
            (cmd new-model (quit-cmd))
            (if cmd* (cmd new-model cmd*) new-model))]
       [else m]))
   (define (view m)
     (if (run-model-view-fn m)
         ((run-model-view-fn m) (run-model-value m))
         (text (format "~a" (run-model-value m)))))])

;; Main entry point.
(define (run initial-value
             #:on-key [on-key-fn #f]
             #:on-msg [on-msg-fn #f]
             #:to-view [view-fn #f]
             #:stop-when [stop-fn #f]
             #:alt-screen [alt-screen #f]
             #:mouse [mouse #f]
             #:show-fps [show-fps #f]
             #:kitty-keyboard [kitty-keyboard #f])
  (define model (run-model initial-value on-key-fn on-msg-fn view-fn stop-fn))
  (define p (make-program model #:alt-screen alt-screen #:mouse mouse #:show-fps show-fps
                          #:kitty-keyboard kitty-keyboard))
  (program-run p))
