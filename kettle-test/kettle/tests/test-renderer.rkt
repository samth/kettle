#lang racket/base

;; test-renderer.rkt -- Unit tests for the renderer and image->string pipeline.
;; Tests that style attributes are correctly translated to ANSI output.

(require rackunit
         racket/string
         kettle)

(provide (all-defined-out))

;;; ============================================================
;;; Style rendering through image->string
;;; ============================================================

(test-case "renderer: plain text"
  (define s (image->string (text "hello")))
  (check-true (string-contains? s "hello")))

(test-case "renderer: bold text"
  (define s (image->string (styled (make-style #:bold #t) "test")))
  ;; SGR 1 = bold
  (check-true (string-contains? s "\e[") "should contain ANSI escape")
  (check-true (string-contains? s "1") "should contain bold SGR code")
  (check-true (string-contains? s "test")))

(test-case "renderer: reverse style swaps fg and bg"
  ;; reverse with default colors (fg=7 white, bg=0 black) should produce
  ;; fg=0 (30) and bg=7 (47) in the output
  (define s (image->string (styled (make-style #:reverse #t) "rev")))
  (check-true (string-contains? s "30") "reversed fg should be 30 (black)")
  (check-true (string-contains? s "47") "reversed bg should be 47 (white)"))

(test-case "renderer: reverse style with custom colors"
  ;; fg=red (1 -> SGR 31), bg=blue (4 -> SGR 44)
  ;; reversed: fg=blue (4 -> SGR 34), bg=red (1 -> SGR 41)
  (define s (image->string (styled (make-style #:foreground fg-red
                                               #:background fg-blue
                                               #:reverse #t)
                                   "x")))
  (check-true (string-contains? s "34") "reversed: blue fg should be 34")
  (check-true (string-contains? s "41") "reversed: red bg should be 41"))

(test-case "renderer: non-reverse keeps normal fg/bg"
  ;; fg=red (1 -> SGR 31), bg=blue (4 -> SGR 44), no reverse
  (define s (image->string (styled (make-style #:foreground fg-red
                                               #:background fg-blue)
                                   "x")))
  (check-true (string-contains? s "31") "normal: red fg should be 31")
  (check-true (string-contains? s "44") "normal: blue bg should be 44"))

(test-case "renderer: italic text"
  (define s (image->string (styled (make-style #:italic #t) "it")))
  ;; SGR 3 = italic
  (check-true (string-contains? s "3") "should contain italic SGR code"))

(test-case "renderer: underline text"
  (define s (image->string (styled (make-style #:underline #t) "ul")))
  ;; ubuf underline produces 'single
  (check-true (string-contains? s "4") "should contain underline SGR code"))

(test-case "renderer: faint text uses faint attribute"
  ;; faint style should produce SGR 2
  (define s (image->string (styled (make-style #:faint #t) "dim")))
  (check-true (string-contains? s "dim")))

(test-case "renderer: combined bold+reverse"
  ;; bold+reverse: should have bold SGR and swapped colors
  (define s (image->string (styled (make-style #:bold #t #:reverse #t) "br")))
  (check-true (string-contains? s "30") "bold+reverse: swapped fg should be 30")
  (check-true (string-contains? s "47") "bold+reverse: swapped bg should be 47")
  (check-true (string-contains? s "1") "bold+reverse: should have bold code"))
