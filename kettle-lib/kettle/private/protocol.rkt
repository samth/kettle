#lang racket/base

;; protocol.rkt
;;
;; SPDX-License-Identifier: MIT
;;
;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;; Racket port
;;
;; Core protocol for the Kettle architecture pattern

(require racket/generic
         racket/match)

(provide gen:kettle-model
         kettle-model?
         init
         update
         view
         subscriptions

         ;; Messages
         (struct-out msg)
         (struct-out quit-msg)
         (struct-out key-msg)
         (struct-out key-event-msg)
         (struct-out key-release-msg)
         (struct-out paste-msg)
         (struct-out window-size-msg)
         (struct-out tick-msg)
         (struct-out suspend-msg)
         (struct-out resume-msg)
         (struct-out focus-in-msg)
         (struct-out focus-out-msg)
         (struct-out kitty-query-response-msg)

         ;; Key event helpers
         key-msg-event-type
         key-press?
         key-repeat?
         key-release?

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

         ;; Update result wrapper
         (struct-out cmd)
         extract-update-result

         ;; Command utilities
         quit-cmd
         batch
         cmd-sequence
         tick-cmd
         (struct-out exec-cmd)

         ;; Key string utility
         key-string

         ;; Component composition
         delegate)

;;; Update result wrapper
;; Return (cmd model value) from update/init when a command is needed.
;; Return a bare model (no wrapper) when there is no command.
(struct cmd (model value) #:transparent)

;; Extract model and command from an update/init return value.
(define (extract-update-result v)
  (if (cmd? v)
      (values (cmd-model v) (cmd-value v))
      (values v #f)))

;;; Kettle Model generic interface
(define-generics kettle-model
  ;; Initialize the model. Returns model or (cmd model command).
  (init kettle-model)
  ;; Handle a message. Returns new-model or (cmd new-model command).
  (update kettle-model msg)
  ;; Render the model to an image (or string for legacy).
  (view kettle-model)
  ;; Return a list of subscription specs. Evaluated after each update.
  (subscriptions kettle-model)
  #:defaults
  ([init (lambda (m) m)]
   [update (lambda (m msg) m)]
   [view (lambda (m) "")]
   [subscriptions (lambda (m) '())]))

;;; Messages
(struct msg () #:transparent)
(struct quit-msg msg () #:transparent)
(struct key-msg msg (key alt ctrl) #:transparent)

;; Extended key message with Kitty keyboard protocol fields.
;; Subtype of key-msg: existing (key-msg? m) checks and
;; (key-msg key alt ctrl) patterns match press/repeat events.
(struct key-event-msg key-msg (shift event-type) #:transparent)
;; event-type: 'press or 'repeat

;; Key release event. NOT a subtype of key-msg, so existing code
;; that matches on key-msg won't accidentally handle releases.
(struct key-release-msg msg (key alt ctrl shift) #:transparent)

;; Internal: Kitty keyboard protocol query response (ESC[?flags u).
(struct kitty-query-response-msg msg (flags) #:transparent)

;; Get the event type from a key message.
;; Returns 'press, 'repeat, 'release, or #f (for plain key-msg).
(define (key-msg-event-type km)
  (cond
    [(key-event-msg? km) (key-event-msg-event-type km)]
    [(key-release-msg? km) 'release]
    [else #f]))

;; True if this is a key press (or a legacy key-msg, which is always press).
(define (key-press? km)
  (or (and (key-msg? km) (not (key-event-msg? km)))
      (and (key-event-msg? km)
           (let ([et (key-event-msg-event-type km)])
             (or (eq? et 'press) (not et))))))

;; True if this is a key repeat event.
(define (key-repeat? km)
  (and (key-event-msg? km) (eq? (key-event-msg-event-type km) 'repeat)))

;; True if this is a key release event.
(define (key-release? km)
  (key-release-msg? km))

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
(struct mouse-scroll-event mouse-event (direction count) #:transparent)

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

;;; Component composition helper
(define (delegate parent getter setter msg)
  (define result (update (getter parent) msg))
  (if (cmd? result)
      (cmd (setter parent (cmd-model result)) (cmd-value result))
      (setter parent result)))
