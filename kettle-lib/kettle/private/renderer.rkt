#lang racket/base

;; renderer.rkt
;;
;; SPDX-License-Identifier: MIT
;;
;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;; Racket port
;;
;; Rendering engine for efficient terminal output

(provide (struct-out renderer)
         make-renderer
         render!
         move-cursor-home
         clear-to-end-of-screen
         move-cursor)

(define ESC "\e")

(struct renderer ([last-output #:mutable] output-stream) #:transparent)

(define (make-renderer [output-stream (current-output-port)])
  (renderer "" output-stream))

(define (render! r view-string)
  "Render the view string to the terminal.
   Only redraws if the output has changed."
  (unless (string=? view-string (renderer-last-output r))
    (set-renderer-last-output! r view-string)
    (define stream (renderer-output-stream r))
    (move-cursor-home stream)
    (display view-string stream)
    (clear-to-end-of-screen stream)
    (flush-output stream)))

(define (move-cursor-home [stream (current-output-port)])
  "Move cursor to home position (1,1)."
  (display (format "~a[H" ESC) stream))

(define (clear-to-end-of-screen [stream (current-output-port)])
  "Clear from cursor to end of screen."
  (display (format "~a[J" ESC) stream))

(define (move-cursor row col [stream (current-output-port)])
  "Move cursor to specific position (1-indexed)."
  (display (format "~a[~a;~aH" ESC row col) stream))
