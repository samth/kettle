#lang racket/base

;; protocol.rkt
;;
;; SPDX-License-Identifier: MIT
;;
;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;; Racket port
;;
;; Core protocol for the Elm Architecture pattern

(require racket/generic
         racket/match)

(provide gen:tea-model
         tea-model?
         init
         update
         view

         ;; Messages
         (struct-out msg)
         (struct-out quit-msg)
         (struct-out key-msg)
         (struct-out paste-msg)
         (struct-out window-size-msg)
         (struct-out tick-msg)
         (struct-out suspend-msg)
         (struct-out resume-msg)
         (struct-out focus-in-msg)
         (struct-out focus-out-msg)

         ;; Mouse event hierarchy
         (struct-out mouse-event)
         (struct-out mouse-button-event)
         (struct-out mouse-press-event)
         (struct-out mouse-release-event)
         (struct-out mouse-drag-event)
         (struct-out mouse-move-event)
         (struct-out mouse-scroll-event)

         ;; Legacy mouse message
         (struct-out mouse-msg)

         ;; Command utilities
         quit-cmd
         batch
         cmd-sequence
         tick-cmd
         (struct-out exec-cmd)

         ;; Key string utility
         key-string)

;;; TEA Model generic interface
(define-generics tea-model
  ;; Initialize the model. Returns (values model cmd-or-#f).
  (init tea-model)
  ;; Handle a message. Returns (values new-model cmd-or-#f).
  (update tea-model msg)
  ;; Render the model to a string.
  (view tea-model)
  #:defaults
  ([init (lambda (m) (values m #f))]
   [update (lambda (m msg) (values m #f))]
   [view (lambda (m) "")]))

;;; Messages
(struct msg () #:transparent)
(struct quit-msg msg () #:transparent)
(struct key-msg msg (key [alt #:mutable] [ctrl #:mutable]) #:transparent)
(struct paste-msg msg (text) #:transparent)
(struct window-size-msg msg (width height) #:transparent)
(struct tick-msg msg (time) #:transparent)
(struct suspend-msg msg () #:transparent)
(struct resume-msg msg () #:transparent)
(struct focus-in-msg msg () #:transparent)
(struct focus-out-msg msg () #:transparent)

;;; Mouse event hierarchy
(struct mouse-event msg (x y shift alt ctrl) #:transparent)
(struct mouse-button-event mouse-event (button) #:transparent)
(struct mouse-press-event mouse-button-event () #:transparent)
(struct mouse-release-event mouse-button-event () #:transparent)
(struct mouse-drag-event mouse-button-event () #:transparent)
(struct mouse-move-event mouse-event () #:transparent)
(struct mouse-scroll-event mouse-event (direction [count #:mutable]) #:transparent)

;; Legacy mouse message
(struct mouse-msg msg (x y button shift alt ctrl action)
  #:transparent)

;;; Command utilities

(define (quit-cmd)
  (lambda () (quit-msg)))

(define (batch . cmds)
  (filter values cmds))

(define (cmd-sequence . cmds)
  (cons 'sequence (filter values cmds)))

(define (tick-cmd duration [fn #f])
  (lambda ()
    (sleep duration)
    (if fn
        (fn)
        (tick-msg (current-inexact-milliseconds)))))

;;; Exec command - run an external program with full TUI suspension
(struct exec-cmd (program args callback) #:transparent)

;;; Key string utility
(define (key-string km)
  (let ([key (key-msg-key km)])
    (cond
      [(char? key) (string key)]
      [(symbol? key) (symbol->string key)]
      [else (format "~a" key)])))
