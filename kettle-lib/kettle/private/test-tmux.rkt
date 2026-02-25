#lang racket/base

;; test-tmux.rkt
;;
;; SPDX-License-Identifier: MIT
;;
;; tmux-based end-to-end test harness for Kettle TUI programs.
;; Runs programs in a real pseudo-terminal via tmux, providing
;; key injection and screen capture for integration testing.

(require racket/port
         racket/string
         racket/format
         rackunit
         (for-syntax racket/base
                     syntax/parse))

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

;;; ============================================================
;;; Session struct
;;; ============================================================

(struct tmux-session (name width height) #:transparent)

;;; ============================================================
;;; Utilities
;;; ============================================================

;; Use a dedicated tmux socket so test sessions get the exact
;; dimensions requested, unaffected by any user tmux server.
(define tmux-socket "kettle-test")

;; Check if tmux is available on this system.
(define (tmux-available?)
  (define-values (proc stdout stdin stderr) (subprocess #f #f #f (find-executable-path "tmux") "-V"))
  (subprocess-wait proc)
  (close-input-port stdout)
  (close-output-port stdin)
  (close-input-port stderr)
  (zero? (subprocess-status proc)))

;; Run a tmux command and return its stdout as a string.
(define (tmux-run . args)
  (define tmux-path (find-executable-path "tmux"))
  (unless tmux-path
    (error 'tmux-run "tmux not found on PATH"))
  (define full-args (list* "-L" tmux-socket args))
  (define-values (proc stdout stdin stderr) (apply subprocess #f #f #f tmux-path full-args))
  (define output (port->string stdout))
  (close-input-port stdout)
  (close-output-port stdin)
  (close-input-port stderr)
  (subprocess-wait proc)
  output)

;; Run a tmux command, discarding output. Raises on non-zero exit.
(define (tmux-run! . args)
  (define tmux-path (find-executable-path "tmux"))
  (unless tmux-path
    (error 'tmux-run! "tmux not found on PATH"))
  (define full-args (list* "-L" tmux-socket args))
  (define-values (proc stdout stdin stderr) (apply subprocess #f #f #f tmux-path full-args))
  (define err-output (port->string stderr))
  (close-input-port stdout)
  (close-output-port stdin)
  (close-input-port stderr)
  (subprocess-wait proc)
  (define code (subprocess-status proc))
  (unless (zero? code)
    (error 'tmux-run! "tmux ~a failed (exit ~a): ~a" (car args) code err-output)))

;; Generate a unique session name.
(define session-counter 0)
(define (make-session-name [prefix "kettle-test"])
  (set! session-counter (add1 session-counter))
  ;; Avoid dots in session names -- tmux interprets dots as target separators
  (format "~a-~a_~a-~a" prefix (inexact->exact (truncate (current-inexact-milliseconds)))
          (random 10000) session-counter))

;;; ============================================================
;;; Session management
;;; ============================================================

;; Start a Kettle program in a tmux session.
;; module-path should be a path to a Racket file with a main submodule.
;; extra-args are additional command-line arguments passed to the Racket program.
(define (tmux-start module-path
                    #:width [width 80]
                    #:height [height 24]
                    #:session-name [session-name #f]
                    #:startup-delay [startup-delay 0.5]
                    #:args [extra-args '()])
  (define name (or session-name (make-session-name)))
  ;; Create a bash session first, then send the racket command.
  ;; This ensures the session persists even if the program exits or errors,
  ;; and lets us capture stderr from the Racket process.
  (tmux-run! "new-session"
             "-d"
             "-s"
             name
             "-x"
             (number->string width)
             "-y"
             (number->string height)
             "bash")
  ;; Launch the Racket program inside the session
  (define args-str
    (if (null? extra-args)
        ""
        (string-append " " (string-join (map ~a extra-args) " "))))
  (define cmd (format "racket -y ~a~a" module-path args-str))
  (tmux-run! "send-keys" "-t" name cmd "Enter")
  ;; Wait for the program to start
  (sleep startup-delay)
  (tmux-session name width height))

;; Kill a tmux session.
(define (tmux-kill session)
  (with-handlers ([exn:fail? void])
    (tmux-run! "kill-session" "-t" (tmux-session-name session))))

;; Convenience macro for automatic cleanup.
(define-syntax-rule (with-tmux-session ([var module-path kw-args ...] ...) body ...)
  (let ([var (tmux-start module-path kw-args ...)] ...)
    (dynamic-wind void
                  (lambda ()
                    body ...)
                  (lambda ()
                    (tmux-kill var) ...))))

;;; ============================================================
;;; Key injection
;;; ============================================================

;; Send keys to a tmux session.
;; key-spec is a tmux key string: "a", "C-q", "Enter", "Up", "Down", "Space", "Tab", etc.
(define (tmux-send-keys session key-spec)
  (tmux-run! "send-keys" "-t" (tmux-session-name session) key-spec))

;; Type a string character by character.
;; Each character is sent individually with a small delay between them.
(define (tmux-type session
                   str
                   #:delay (delay
                             0.05))
  (for ([ch (in-string str)])
    (tmux-send-keys session (string ch))
    (sleep delay)))

;;; ============================================================
;;; Screen capture
;;; ============================================================

;; Capture the contents of the tmux pane.
(define (tmux-capture session #:trim [trim #f])
  (define output (tmux-run "capture-pane" "-t" (tmux-session-name session) "-p"))
  (if trim
      (string-trim output)
      output))

;;; ============================================================
;;; Waiting / polling
;;; ============================================================

;; Wait until the captured pane contains pattern (string or regexp).
;; Returns the captured pane contents on success, or #f on timeout.
(define (tmux-wait-for session pattern #:timeout [timeout 5] #:interval [interval 0.1])
  (define rx
    (if (regexp? pattern)
        pattern
        (regexp-quote pattern)))
  (define deadline (+ (current-inexact-milliseconds) (* timeout 1000)))
  (let loop ()
    (define captured (tmux-capture session #:trim #t))
    (cond
      [(regexp-match? rx captured) captured]
      [(> (current-inexact-milliseconds) deadline) #f]
      [else
       (sleep interval)
       (loop)])))

;;; ============================================================
;;; Rackunit checks
;;; ============================================================

(define-check (check-tmux-contains session expected-text)
  (define captured (tmux-capture session #:trim #t))
  (unless (regexp-match? (regexp-quote expected-text) captured)
    (with-check-info (['expected expected-text] ['captured-pane captured])
                     (fail-check (format "Pane does not contain ~s" expected-text)))))

(define-check (check-tmux-matches session pattern)
  (define captured (tmux-capture session #:trim #t))
  (define rx
    (if (regexp? pattern)
        pattern
        (regexp pattern)))
  (unless (regexp-match? rx captured)
    (with-check-info (['pattern (format "~a" pattern)] ['captured-pane captured])
                     (fail-check (format "Pane does not match pattern ~s" pattern)))))

(define-check (check-tmux-not-contains session text)
  (define captured (tmux-capture session #:trim #t))
  (when (regexp-match? (regexp-quote text) captured)
    (with-check-info (['unexpected text] ['captured-pane captured])
                     (fail-check (format "Pane should not contain ~s" text)))))

;;; ============================================================
;;; DSL helpers for concise e2e tests
;;; ============================================================

;; Send a key and wait for expected text to appear.
(define (send+wait session key expected #:timeout [timeout 5])
  (tmux-send-keys session key)
  (tmux-wait-for session expected #:timeout timeout))

;; Check that sending a quit key causes the program to exit.
;; marker is text that should disappear when the TUI is no longer running.
(define-check (check-quit-exits session quit-key marker)
  (tmux-send-keys session quit-key)
  (sleep 0.3)
  (define captured (tmux-capture session #:trim #t))
  (when (regexp-match? (regexp-quote marker) captured)
    (with-check-info (['quit-key quit-key] ['marker marker] ['captured-pane captured])
                     (fail-check (format "TUI should no longer be visible after ~s, still contains ~s"
                                         quit-key marker)))))

;; Start multiple tmux sessions in parallel and call body with them.
;; Automatically kills all sessions on exit.
;; Each spec is (id module-path keyword-arg ...)
(define-syntax (with-e2e-sessions stx)
  (syntax-parse stx
    [(_ ([id:id path:expr . kw-args] ...) body:expr ...+)
     #:with (id-box ...) (generate-temporaries #'(id ...))
     #'(let ()
         (define id-box (box #f)) ...
         (define threads
           (list (thread (lambda () (set-box! id-box (tmux-start path . kw-args)))) ...))
         (for-each thread-wait threads)
         (define id (unbox id-box)) ...
         (dynamic-wind
          void
          (lambda () body ...)
          (lambda ()
            (with-handlers ([exn:fail? void]) (tmux-kill id)) ...)))]))
