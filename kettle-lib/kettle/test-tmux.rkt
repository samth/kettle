#lang racket/base

;; kettle/test-tmux -- tmux-based end-to-end test harness
;;
;; Provides tools for running Kettle programs in a real pseudo-terminal
;; via tmux, injecting keystrokes, and capturing screen output.

(require "private/test-tmux.rkt")

;; Session management
(provide tmux-start
         tmux-kill
         with-tmux-session

         ;; Key injection
         tmux-send-keys
         tmux-type

         ;; Screen capture
         tmux-capture

         ;; Waiting / polling
         tmux-wait-for

         ;; Rackunit checks
         check-tmux-contains
         check-tmux-matches
         check-tmux-not-contains

         ;; DSL helpers
         send+wait
         check-quit-exits
         with-e2e-sessions

         ;; Predicate
         tmux-available?

         ;; Struct
         tmux-session
         tmux-session-name
         tmux-session-width
         tmux-session-height)
