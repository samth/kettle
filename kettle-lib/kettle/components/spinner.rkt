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
         "../private/protocol.rkt"
         "../private/image.rkt")

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
         spinner-ellipsis)

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

;;; Spinner model -- implements gen:tea-model
(struct spinner (frames fps frame-index id)
  #:transparent
  #:methods gen:tea-model
  [(define (init s)
     (values s
             (lambda ()
               (sleep (spinner-fps s))
               (spinner-tick-msg (spinner-id s)))))
   (define (update s msg)
     (cond
       [(and (spinner-tick-msg? msg) (= (spinner-tick-msg-id msg) (spinner-id s)))
        (define new-index (modulo (add1 (spinner-frame-index s)) (length (spinner-frames s))))
        (values (struct-copy spinner s [frame-index new-index])
                (lambda ()
                  (sleep (spinner-fps s))
                  (spinner-tick-msg (spinner-id s))))]
       [else (values s #f)]))
   (define (view s)
     (define frames (spinner-frames s))
     (define index (spinner-frame-index s))
     (if (< index (length frames))
         (text (list-ref frames index))
         empty-image))])

(define (make-spinner #:frames [frames spinner-line] #:fps [fps 0.1])
  (spinner frames fps 0 (current-inexact-milliseconds)))
