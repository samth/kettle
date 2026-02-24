#lang racket/base

;; terminal.rkt
;;
;; SPDX-License-Identifier: MIT
;;
;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;; Racket port
;;
;; Terminal control and raw mode handling

(require racket/system
         racket/port
         racket/list
         racket/string
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
         suspend-terminal
         resume-terminal
         with-raw-terminal
         open-tty)

(define ESC "\e")

;; Store original stty settings for restoration
(define original-stty-settings (make-parameter #f))
(define tty-port (make-parameter #f))

(define (open-tty)
  "Open /dev/tty for direct terminal I/O."
  (or (tty-port)
      (let ()
        (define-values (in out) (values (open-input-file "/dev/tty")
                                        (open-output-file "/dev/tty" #:exists 'append)))
        (define combined (make-input-port
                          "tty-in"
                          (lambda (buf start end)
                            (let ([b (read-bytes-avail! buf in start end)])
                              (if (eof-object? b) 0 b)))
                          #f
                          (lambda () (close-input-port in))))
        (tty-port (list combined out in))
        (tty-port))))

(define (tty-input)
  (define p (tty-port))
  (if p (first p) (current-input-port)))

(define (tty-output)
  (define p (tty-port))
  (if p (second p) (current-output-port)))

(define (close-tty)
  (define p (tty-port))
  (when p
    (close-input-port (third p))
    (close-output-port (second p))
    (tty-port #f)))

(define (enter-raw-mode)
  "Put the terminal in raw mode for TUI applications."
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (raise (exn:fail:kettle:terminal:operation
                             (format "enter-raw-mode failed: ~a" (exn-message e))
                             (current-continuation-marks)
                             e
                             'enter-raw-mode)))])
    ;; Save original settings
    (unless (original-stty-settings)
      (define saved
        (string-trim
         (with-output-to-string
           (lambda ()
             (parameterize ([current-input-port (open-input-file "/dev/tty")])
               (system* "/bin/stty" "-g"))))))
      (original-stty-settings saved))
    ;; Enter raw mode via stty
    (parameterize ([current-input-port (open-input-file "/dev/tty")])
      (system* "/bin/stty" "raw" "-echo" "-icanon" "-isig" "-iexten"
               "-ixon" "-brkint" "-inlcr" "-igncr" "-icrnl" "-opost"
               "min" "1" "time" "0"))))

(define (exit-raw-mode)
  "Restore the terminal to its original state."
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (raise (exn:fail:kettle:terminal:operation
                             (format "exit-raw-mode failed: ~a" (exn-message e))
                             (current-continuation-marks)
                             e
                             'exit-raw-mode)))])
    (define saved (original-stty-settings))
    (when saved
      (parameterize ([current-input-port (open-input-file "/dev/tty")])
        (system* "/bin/stty" saved))
      (original-stty-settings #f))))

(define (get-terminal-size)
  "Get the current terminal size as (cons width height)."
  (with-handlers ([exn:fail? (lambda (e) (cons 80 24))])
    (define output
      (string-trim
       (with-output-to-string
         (lambda ()
           (parameterize ([current-input-port (open-input-file "/dev/tty")])
             (system* "/bin/stty" "size"))))))
    (define parts (string-split output))
    (if (= (length parts) 2)
        (cons (string->number (second parts))
              (string->number (first parts)))
        (cons 80 24))))

(define (set-terminal-title title)
  "Set the terminal window title."
  (display (format "~a]0;~a\a" ESC title) (tty-output))
  (flush-output (tty-output)))

(define (clear-screen [port (tty-output)])
  "Clear the terminal screen."
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

;;; Suspend/resume support

(define (suspend-terminal #:alt-screen [alt-screen #f]
                          #:mouse [mouse #f]
                          #:focus-events [focus-events #f])
  "Suspend the terminal - restore original state for backgrounding.
   Returns a thunk to restore the TUI state."
  (when mouse (disable-mouse))
  (disable-bracketed-paste)
  (when focus-events (disable-focus-events))
  (when alt-screen (exit-alt-screen))
  (show-cursor)
  (exit-raw-mode)
  (flush-output (tty-output))
  ;; Return restore function
  (lambda ()
    (enter-raw-mode)
    (hide-cursor)
    (clear-screen)
    (when alt-screen (enter-alt-screen))
    (enable-bracketed-paste)
    (when focus-events (enable-focus-events))
    (case mouse
      [(cell-motion) (enable-mouse-cell-motion)]
      [(all-motion) (enable-mouse-all-motion)]
      [else (void)])
    (flush-output (tty-output))))

(define (resume-terminal restore-fn)
  "Resume the terminal using the restore function."
  (when restore-fn
    (restore-fn)))

;;; High-level terminal lifecycle
(define (with-raw-terminal thunk
                           #:alt-screen [alt-screen #f]
                           #:mouse [mouse #f]
                           #:focus-events [focus-events #t])
  "Execute thunk with the terminal in raw TUI mode."
  (define raw-ok #f)
  (dynamic-wind
    (lambda ()
      (enter-raw-mode)
      (set! raw-ok #t)
      (when raw-ok
        (hide-cursor)
        (clear-screen))
      (when alt-screen (enter-alt-screen))
      (when raw-ok (enable-bracketed-paste))
      (when focus-events (enable-focus-events))
      (case mouse
        [(cell-motion) (enable-mouse-cell-motion)]
        [(all-motion) (enable-mouse-all-motion)]
        [else (void)]))
    thunk
    (lambda ()
      (with-handlers ([exn:fail? void])
        (when alt-screen (exit-alt-screen))
        (when mouse (disable-mouse))
        (when focus-events (disable-focus-events))
        (when raw-ok (disable-bracketed-paste))
        (when raw-ok (show-cursor))
        (when raw-ok (exit-raw-mode))))))
