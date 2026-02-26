#lang racket/base

;; input.rkt
;;
;; SPDX-License-Identifier: MIT
;;
;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;; Racket port
;;
;; Input handling and key event processing.
;; Uses ansi/lcd-terminal for input parsing, translating ansi events
;; into Kettle protocol messages.

(require racket/set
         (prefix-in ansi: ansi/lcd-terminal)
         "protocol.rkt")

(provide read-key
         read-all-available-events
         current-input-stream)

;; The stream to read input from.
(define current-input-stream (make-parameter (current-input-port)))

;;; ============================================================
;;; Translation: ansi event → Kettle protocol message
;;; ============================================================

;; Convert ansi key to Kettle key-msg
(define (ansi-key->kettle-msg k)
  (define val (ansi:key-value k))
  (define mods (ansi:key-modifiers k))
  (define shift? (set-member? mods 'shift))
  (define alt? (set-member? mods 'meta))
  (define ctrl? (set-member? mods 'control))

  (cond
    [(ansi:unknown-escape-sequence? val)
     (key-msg 'unknown #f #f)]
    [else
     ;; Map shifted navigation keys to Kettle's shift-* symbols
     (define final-key
       (if shift?
           (case val
             [(up) 'shift-up] [(down) 'shift-down]
             [(left) 'shift-left] [(right) 'shift-right]
             [(home) 'shift-home] [(end) 'shift-end]
             [else val])
           val))
     ;; ansi returns (key #\A (set 'control)) for Ctrl+A,
     ;; but Kettle expects (key-msg #\a #f #t) with lowercase char
     (define normalized-key
       (if (and (char? final-key) ctrl?)
           (char-downcase final-key)
           final-key))
     (key-msg normalized-key alt? ctrl?)]))

;; Convert ansi kitty-key-event to Kettle key-event-msg or key-release-msg
(define (ansi-kitty->kettle-msg evt)
  (define val (ansi:kitty-key-event-value evt))
  (define mods (ansi:kitty-key-event-modifiers evt))
  (define event-type (ansi:kitty-key-event-event-type evt))
  (define shift? (set-member? mods 'shift))
  (define alt? (set-member? mods 'meta))
  (define ctrl? (set-member? mods 'control))

  (cond
    ;; Query response: event-type field holds the flags integer
    [(eq? val 'query-response)
     (kitty-query-response-msg event-type)]
    ;; Key release
    [(eq? event-type 'release)
     (key-release-msg val alt? ctrl? shift?)]
    ;; Key press or repeat
    [else
     (key-event-msg val alt? ctrl? shift? event-type)]))

;; Convert ansi mouse-event to Kettle mouse event
(define (ansi-mouse->kettle-msg evt)
  (define type (ansi:mouse-event-type evt))
  (define btn (ansi:mouse-event-button evt))
  (define row (ansi:mouse-event-row evt))
  (define col (ansi:mouse-event-column evt))
  (define mods (ansi:mouse-event-modifiers evt))
  (define shift? (set-member? mods 'shift))
  (define alt? (set-member? mods 'meta))
  (define ctrl? (set-member? mods 'control))

  ;; Convert 1-based button number to symbol
  (define button-sym
    (case btn
      [(1) 'left] [(2) 'middle] [(3) 'right]
      [else #f]))

  (case type
    [(press)
     (mouse-press-event col row shift? alt? ctrl? button-sym)]
    [(release release-all)
     (mouse-release-event col row shift? alt? ctrl? button-sym)]
    [(motion)
     (if button-sym
         (mouse-drag-event col row shift? alt? ctrl? button-sym)
         (mouse-move-event col row shift? alt? ctrl?))]
    [(scroll)
     (define direction (case btn [(4) 'up] [(5) 'down] [else 'up]))
     (mouse-scroll-event col row shift? alt? ctrl? direction 1)]
    [else #f]))

;; Convert ansi focus event to Kettle focus message
(define (ansi-focus->kettle-msg evt)
  (if (ansi:mouse-focus-event-focus-in? evt)
      (focus-in-msg)
      (focus-out-msg)))

;;; ============================================================
;;; Main read-key: non-blocking, uses lex-lcd-input
;;; ============================================================

(define (read-key)
  "Read a single key from input and return a key-msg or mouse event.
   Returns #f if no input is available."
  (define port (current-input-stream))
  ;; Check if input is available (non-blocking)
  (and (byte-ready? port)
       (let ([evt (ansi:lex-lcd-input port)])
         (cond
           [(eof-object? evt) #f]
           [(ansi:key? evt) (ansi-key->kettle-msg evt)]
           [(ansi:kitty-key-event? evt) (ansi-kitty->kettle-msg evt)]
           [(ansi:mouse-event? evt) (ansi-mouse->kettle-msg evt)]
           [(ansi:mouse-focus-event? evt) (ansi-focus->kettle-msg evt)]
           [(ansi:bracketed-paste-event? evt)
            (paste-msg (ansi:bracketed-paste-event-content evt))]
           [else #f]))))

(define (read-all-available-events)
  "Read all available input events and return them as a list."
  (let loop ([events '()])
    (define evt (read-key))
    (if evt
        (loop (append events (list evt)))
        events)))
