#lang racket/base

;; components/spinner.rkt
;;
;; SPDX-License-Identifier: MIT
;;
;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;; Racket port
;;
;; Spinner component - reusable animated spinner

(require racket/list
         "../private/protocol.rkt")

(provide (struct-out spinner)
         make-spinner
         (struct-out spinner-tick-msg)

         ;; Predefined spinners
         spinner-line
         spinner-dot
         spinner-minidot
         spinner-jump
         spinner-pulse
         spinner-points
         spinner-globe
         spinner-moon
         spinner-monkey
         spinner-meter
         spinner-hamburger
         spinner-ellipsis

         ;; Operations
         spinner-init
         spinner-update
         spinner-view)

;;; Predefined spinner frame lists
(define spinner-line '("|" "/" "-" "\\"))
(define spinner-dot '("⣾ " "⣽ " "⣻ " "⢿ " "⡿ " "⣟ " "⣯ " "⣷ "))
(define spinner-minidot '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"))
(define spinner-jump '("⢄" "⢂" "⢁" "⡁" "⡈" "⡐" "⡠"))
(define spinner-pulse '("█" "▓" "▒" "░"))
(define spinner-points '("∙∙∙" "●∙∙" "∙●∙" "∙∙●"))
(define spinner-globe '("🌍" "🌎" "🌏"))
(define spinner-moon '("🌑" "🌒" "🌓" "🌔" "🌕" "🌖" "🌗" "🌘"))
(define spinner-monkey '("🙈" "🙉" "🙊"))
(define spinner-meter '("▱▱▱" "▰▱▱" "▰▰▱" "▰▰▰" "▰▰▱" "▰▱▱" "▱▱▱"))
(define spinner-hamburger '("☱" "☲" "☴" "☲"))
(define spinner-ellipsis '("" "." ".." "..."))

;;; Tick message
(struct spinner-tick-msg msg (id) #:transparent)

;;; Spinner model
(struct spinner
  (frames fps [frame-index #:mutable] id)
  #:transparent)

(define (make-spinner #:frames [frames spinner-line]
                      #:fps [fps 0.1])
  (spinner frames fps 0 (current-inexact-milliseconds)))

;;; Component operations

(define (spinner-init s)
  "Initialize the spinner and return a tick command."
  (lambda ()
    (sleep (spinner-fps s))
    (spinner-tick-msg (spinner-id s))))

(define (spinner-update s msg)
  "Update the spinner. Returns (values new-spinner cmd)."
  (cond
    [(and (spinner-tick-msg? msg)
          (= (spinner-tick-msg-id msg) (spinner-id s)))
     (set-spinner-frame-index! s
                               (modulo (add1 (spinner-frame-index s))
                                       (length (spinner-frames s))))
     (values s
             (lambda ()
               (sleep (spinner-fps s))
               (spinner-tick-msg (spinner-id s))))]
    [else (values s #f)]))

(define (spinner-view s)
  "Render the current spinner frame."
  (define frames (spinner-frames s))
  (define index (spinner-frame-index s))
  (if (< index (length frames))
      (list-ref frames index)
      ""))
