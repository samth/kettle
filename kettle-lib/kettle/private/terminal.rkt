#lang racket/base

;; terminal.rkt
;;
;; SPDX-License-Identifier: MIT
;;
;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;; Racket port
;;
;; Terminal control and raw mode handling.
;; Uses the built-in #%terminal API (ioctl-based) for raw mode and
;; terminal size, with escape sequences for TUI features.

(require racket/list
         (only-in '#%terminal terminal-init terminal-get-screen-size terminal-raw-mode)
         "errors.rkt")

(provide enter-raw-mode
         exit-raw-mode
         get-terminal-size
         set-terminal-title
         clear-screen
         hide-cursor
         show-cursor
         enter-alt-screen
         exit-alt-screen
         enable-mouse-cell-motion
         enable-mouse-all-motion
         disable-mouse
         enable-bracketed-paste
         disable-bracketed-paste
         enable-focus-events
         disable-focus-events
         enable-kitty-keyboard
         disable-kitty-keyboard
         suspend-terminal
         resume-terminal
         with-raw-terminal
         open-tty)

(define ESC "\e")

(define tty-port (make-parameter #f))
(define terminal-initialized? (make-parameter #f))

(define (open-tty)
  (or (tty-port)
      (let ()
        (define in (open-input-file "/dev/tty"))
        (define out (open-output-file "/dev/tty" #:exists 'append))
        ;; Disable buffering so byte-ready? reflects actual OS state
        (file-stream-buffer-mode in 'none)
        (tty-port (list in out))
        (tty-port))))

(define (tty-input)
  (define p (tty-port))
  (if p
      (first p)
      (current-input-port)))

(define (tty-output)
  (define p (tty-port))
  (if p
      (second p)
      (current-output-port)))

(define (close-tty)
  (define p (tty-port))
  (when p
    (close-input-port (first p))
    (close-output-port (second p))
    (tty-port #f)))

;; Initialize the #%terminal subsystem if not already done.
;; Uses fd -1 to indicate "use inherited stdin/stdout".
(define (ensure-terminal-init!)
  (unless (terminal-initialized?)
    (when (terminal-init -1 -1)
      (terminal-initialized? #t))))

(define (enter-raw-mode)
  (with-handlers ([exn:fail? (lambda (e)
                               (raise (exn:fail:kettle:terminal:operation
                                       (format "enter-raw-mode failed: ~a" (exn-message e))
                                       (current-continuation-marks)
                                       e
                                       'enter-raw-mode)))])
    (ensure-terminal-init!)
    (terminal-raw-mode #t)))

(define (exit-raw-mode)
  (with-handlers ([exn:fail? (lambda (e)
                               (raise (exn:fail:kettle:terminal:operation
                                       (format "exit-raw-mode failed: ~a" (exn-message e))
                                       (current-continuation-marks)
                                       e
                                       'exit-raw-mode)))])
    (terminal-raw-mode #f)))

;; Returns (cons width height). Uses ioctl TIOCGWINSZ via #%terminal.
(define (get-terminal-size)
  (with-handlers ([exn:fail? (lambda (e) (cons 80 24))])
    (ensure-terminal-init!)
    (define size (terminal-get-screen-size))
    (cons (cdr size) (car size))))

(define (set-terminal-title title)
  (display (format "~a]0;~a\a" ESC title) (tty-output))
  (flush-output (tty-output)))

(define (clear-screen [port (tty-output)])
  (display (format "~a[2J~a[H" ESC ESC) port)
  (flush-output port))

(define (hide-cursor [port (tty-output)])
  (display (format "~a[?25l" ESC) port)
  (flush-output port))

(define (show-cursor [port (tty-output)])
  (display (format "~a[?25h" ESC) port)
  (flush-output port))

(define (enter-alt-screen [port (tty-output)])
  (display (format "~a[?1049h" ESC) port)
  (flush-output port))

(define (exit-alt-screen [port (tty-output)])
  (display (format "~a[?1049l" ESC) port)
  (flush-output port))

(define (enable-mouse-cell-motion [port (tty-output)])
  (display (format "~a[?1002h~a[?1006h" ESC ESC) port)
  (flush-output port))

(define (enable-mouse-all-motion [port (tty-output)])
  (display (format "~a[?1003h~a[?1006h" ESC ESC) port)
  (flush-output port))

(define (disable-mouse [port (tty-output)])
  (display (format "~a[?1002l~a[?1003l~a[?1006l" ESC ESC ESC) port)
  (flush-output port))

(define (enable-bracketed-paste [port (tty-output)])
  (display (format "~a[?2004h" ESC) port)
  (flush-output port))

(define (disable-bracketed-paste [port (tty-output)])
  (display (format "~a[?2004l" ESC) port)
  (flush-output port))

(define (enable-focus-events [port (tty-output)])
  (display (format "~a[?1004h" ESC) port)
  (flush-output port))

(define (disable-focus-events [port (tty-output)])
  (display (format "~a[?1004l" ESC) port)
  (flush-output port))

;; Kitty keyboard protocol: push enhanced mode
;; flags bitmask: 0x1=disambiguate, 0x2=event types, 0x4=alternate keys
(define (enable-kitty-keyboard [port (tty-output)] #:flags [flags 3])
  (display (format "~a[>~au" ESC flags) port)
  (flush-output port))

;; Kitty keyboard protocol: pop mode (restore previous)
(define (disable-kitty-keyboard [port (tty-output)])
  (display (format "~a[<u" ESC) port)
  (flush-output port))

;;; Suspend/resume support

(define (suspend-terminal #:alt-screen [alt-screen #f]
                          #:mouse [mouse #f]
                          #:focus-events [focus-events #f]
                          #:kitty-keyboard [kitty-keyboard #f])
  (when kitty-keyboard
    (disable-kitty-keyboard))
  (when mouse
    (disable-mouse))
  (disable-bracketed-paste)
  (when focus-events
    (disable-focus-events))
  (when alt-screen
    (exit-alt-screen))
  (show-cursor)
  (exit-raw-mode)
  (flush-output (tty-output))
  ;; Return restore function
  (lambda ()
    (enter-raw-mode)
    (hide-cursor)
    (clear-screen)
    (when alt-screen
      (enter-alt-screen))
    (enable-bracketed-paste)
    (when focus-events
      (enable-focus-events))
    (when kitty-keyboard
      (enable-kitty-keyboard))
    (case mouse
      [(cell-motion) (enable-mouse-cell-motion)]
      [(all-motion) (enable-mouse-all-motion)]
      [else (void)])
    (flush-output (tty-output))))

(define (resume-terminal restore-fn)
  (when restore-fn
    (restore-fn)))

;;; High-level terminal lifecycle
(define (with-raw-terminal thunk
                           #:alt-screen [alt-screen #f]
                           #:mouse [mouse #f]
                           #:focus-events [focus-events #t]
                           #:kitty-keyboard [kitty-keyboard #f])
  (define raw-ok #f)
  (dynamic-wind (lambda ()
                  (enter-raw-mode)
                  (set! raw-ok #t)
                  (when raw-ok
                    (hide-cursor)
                    (clear-screen))
                  (when alt-screen
                    (enter-alt-screen))
                  (when raw-ok
                    (enable-bracketed-paste))
                  (when focus-events
                    (enable-focus-events))
                  (when kitty-keyboard
                    (enable-kitty-keyboard))
                  (case mouse
                    [(cell-motion) (enable-mouse-cell-motion)]
                    [(all-motion) (enable-mouse-all-motion)]
                    [else (void)]))
                thunk
                (lambda ()
                  (with-handlers ([exn:fail? void])
                    (when kitty-keyboard
                      (disable-kitty-keyboard))
                    (when alt-screen
                      (exit-alt-screen))
                    (when mouse
                      (disable-mouse))
                    (when focus-events
                      (disable-focus-events))
                    (when raw-ok
                      (disable-bracketed-paste))
                    (when raw-ok
                      (show-cursor))
                    (when raw-ok
                      (exit-raw-mode))))))
