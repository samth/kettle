#lang racket/base

;; test-program.rkt
;;
;; SPDX-License-Identifier: MIT
;;
;; Headless, synchronous Kettle test harness.
;; Reimplements the Kettle loop without threading or terminal dependencies.
;; Useful for deterministic integration testing of Kettle programs.

(require racket/match
         rackunit
         kettle/private/protocol
         kettle/private/renderer
         kettle/private/image
         kettle/private/subscriptions)

;; Construction
(provide make-test-program
         make-test-program/run

         ;; Event injection
         test-program-send
         test-program-press
         test-program-release
         test-program-type
         test-program-resize

         ;; State inspection
         test-program-model
         test-program-value
         test-program-view
         test-program-view-string
         test-program-done?
         test-program-history
         test-program-subscriptions

         ;; Rackunit checks
         check-test-program-contains
         check-test-program-done
         check-test-program-running)

;;; ============================================================
;;; Internal test-program struct
;;; ============================================================

(struct test-program
        ([model #:mutable] ; current kettle-model
         width
         height ; simulated terminal size
         [done? #:mutable] ; #t after quit-msg
         [history #:mutable] ; list of (cons model view), reverse chronological
         [active-subs #:mutable]) ; tracked subscription specs (not started)
  #:transparent)

;;; ============================================================
;;; Helpers
;;; ============================================================

;; Safely get subscriptions, returning '() if not implemented.
;; Many kettle-model structs don't define subscriptions; the #:defaults
;; mechanism in define-generics doesn't always fire.
(define (safe-subscriptions m)
  (with-handlers ([exn:fail? (lambda (_) '())])
    (subscriptions m)))

;;; ============================================================
;;; Command processing
;;; ============================================================

;; Process a command synchronously. Returns a list of messages to feed back.
(define (collect-command-messages cmd)
  (cond
    [(not cmd) '()]

    ;; Procedure (e.g. quit-cmd, tick-cmd) -- call it, collect result
    [(procedure? cmd)
     (define m (cmd))
     (if m
         (list m)
         '())]

    ;; Exec command -- skip the external program, call callback if present
    [(exec-cmd? cmd)
     (if (exec-cmd-callback cmd)
         (let ([m ((exec-cmd-callback cmd))])
           (if m
               (list m)
               '()))
         '())]

    ;; Batch or sequence list
    [(list? cmd)
     (cond
       ;; Sequence: (cons 'sequence thunks)
       [(and (not (null? cmd)) (eq? (car cmd) 'sequence))
        (apply append
               (for/list ([thunk (in-list (cdr cmd))])
                 (if thunk
                     (let ([m (thunk)])
                       (if m
                           (list m)
                           '()))
                     '())))]
       ;; Batch: list of commands
       [else
        (apply append
               (for/list ([c (in-list cmd)])
                 (collect-command-messages c)))])]

    [else '()]))

;; Process a single message through the TEA loop.
;; Handles update, command processing (recursively), view, and history.
(define (process-message! tp msg)
  (when (test-program-done? tp)
    (return))
  (cond
    [(quit-msg? msg) (set-test-program-done?! tp #t)]
    [else
     (define-values (new-model cmd) (extract-update-result (update (test-program-model tp) msg)))
     (set-test-program-model! tp new-model)
     ;; Process command and feed result messages back
     (define result-msgs (collect-command-messages cmd))
     (for ([m (in-list result-msgs)])
       (process-message! tp m))
     ;; Update subscriptions
     (unless (test-program-done? tp)
       (set-test-program-active-subs! tp (safe-subscriptions new-model)))
     ;; Record view and history
     (unless (test-program-done? tp)
       (define v (view (test-program-model tp)))
       (set-test-program-history! tp
                                  (cons (cons (test-program-model tp) v)
                                        (test-program-history tp))))]))

;; A non-local escape for process-message! when done
(define return void)

;;; ============================================================
;;; Construction
;;; ============================================================

;; Create a test-program from a kettle-model value.
(define (make-test-program model #:width [width 80] #:height [height 24])
  (define tp (test-program model width height #f '() '()))
  ;; Run init
  (define-values (m cmd) (extract-update-result (init model)))
  (set-test-program-model! tp m)
  ;; Process init command
  (define init-msgs (collect-command-messages cmd))
  (for ([msg (in-list init-msgs)])
    (process-message! tp msg))
  ;; Record initial view
  (unless (test-program-done? tp)
    (define v (view (test-program-model tp)))
    (set-test-program-history! tp (list (cons (test-program-model tp) v))))
  ;; Send initial window-size-msg (matching program-run behavior)
  (unless (test-program-done? tp)
    (test-program-send tp (window-size-msg width height)))
  ;; Initialize subscriptions
  (unless (test-program-done? tp)
    (set-test-program-active-subs! tp (safe-subscriptions (test-program-model tp))))
  tp)

;;; ============================================================
;;; run-style adapter
;;; ============================================================

;; Internal kettle-model wrapper for run-style programs (mirrors run-model from run.rkt)
(struct test-run-model (value on-key-fn on-msg-fn view-fn stop-fn on-tick-fn tick-rate)
  #:methods gen:kettle-model
  [(define (init m)
     m)
   (define (update m msg)
     (cond
       ;; Check stop condition before processing
       [(and (test-run-model-stop-fn m) ((test-run-model-stop-fn m) (test-run-model-value m)))
        (cmd m (quit-cmd))]
       ;; Key messages go to on-key handler
       [(and (key-msg? msg) (test-run-model-on-key-fn m))
        (define-values (new-val cmd*)
          (extract-update-result ((test-run-model-on-key-fn m) (test-run-model-value m) msg)))
        (define new-model (struct-copy test-run-model m [value new-val]))
        (if (and (test-run-model-stop-fn m) ((test-run-model-stop-fn m) new-val))
            (cmd new-model (quit-cmd))
            (if cmd*
                (cmd new-model cmd*)
                new-model))]
       ;; Tick messages go to on-tick handler (takes state only, no message)
       [(and (tick-msg? msg) (test-run-model-on-tick-fn m))
        (define-values (new-val cmd*)
          (extract-update-result ((test-run-model-on-tick-fn m) (test-run-model-value m))))
        (define new-model (struct-copy test-run-model m [value new-val]))
        (if (and (test-run-model-stop-fn m) ((test-run-model-stop-fn m) new-val))
            (cmd new-model (quit-cmd))
            (if cmd*
                (cmd new-model cmd*)
                new-model))]
       ;; All other messages go to on-msg handler
       [(test-run-model-on-msg-fn m)
        (define-values (new-val cmd*)
          (extract-update-result ((test-run-model-on-msg-fn m) (test-run-model-value m) msg)))
        (define new-model (struct-copy test-run-model m [value new-val]))
        (if (and (test-run-model-stop-fn m) ((test-run-model-stop-fn m) new-val))
            (cmd new-model (quit-cmd))
            (if cmd*
                (cmd new-model cmd*)
                new-model))]
       [else m]))
   (define (view m)
     (if (test-run-model-view-fn m)
         ((test-run-model-view-fn m) (test-run-model-value m))
         (text (format "~a" (test-run-model-value m)))))
   (define (subscriptions m)
     (if (test-run-model-on-tick-fn m)
         (list (every (test-run-model-tick-rate m) tick-msg))
         '()))])

;; Create a test-program from run-style arguments.
(define (make-test-program/run initial-value
                               #:on-key [on-key-fn #f]
                               #:on-tick [on-tick-fn #f]
                               #:tick-rate [tick-rate 1]
                               #:on-msg [on-msg-fn #f]
                               #:to-view [view-fn #f]
                               #:stop-when [stop-fn #f]
                               #:width [width 80]
                               #:height [height 24])
  (define model
    (test-run-model initial-value on-key-fn on-msg-fn view-fn stop-fn on-tick-fn tick-rate))
  (make-test-program model #:width width #:height height))

;;; ============================================================
;;; Event injection
;;; ============================================================

;; Send any message to the test program.
(define (test-program-send tp msg)
  (unless (test-program-done? tp)
    (process-message! tp msg)))

;; Press a key. key can be a character or symbol.
;; #:alt and #:ctrl are booleans for modifier keys.
(define (test-program-press tp key #:alt [alt #f] #:ctrl [ctrl #f])
  (test-program-send tp (key-msg key alt ctrl)))

;; Release a key (Kitty keyboard protocol).
(define (test-program-release tp key #:alt [alt #f] #:ctrl [ctrl #f] #:shift [shift #f])
  (test-program-send tp (key-release-msg key alt ctrl shift)))

;; Type a string character by character.
(define (test-program-type tp str)
  (for ([ch (in-string str)])
    (test-program-press tp ch)))

;; Send a window resize event.
(define (test-program-resize tp width height)
  (test-program-send tp (window-size-msg width height)))

;;; ============================================================
;;; State inspection
;;; ============================================================

;; test-program-model is provided by the struct accessor

;; Get the unwrapped value for run-style programs.
(define (test-program-value tp)
  (define m (test-program-model tp))
  (if (test-run-model? m)
      (test-run-model-value m)
      m))

;; Get the current view as an image (or string).
(define (test-program-view tp)
  (view (test-program-model tp)))

;; Get the current view rendered as a plain string.
(define (test-program-view-string tp)
  (define v (test-program-view tp))
  (cond
    [(image? v) (image->string v)]
    [(string? v) v]
    [else ""]))

;; test-program-done? is provided by the struct accessor
;; test-program-history is provided by the struct accessor

;; Get current subscription specs.
(define (test-program-subscriptions tp)
  (test-program-active-subs tp))

;;; ============================================================
;;; Rackunit checks
;;; ============================================================

(define-check (check-test-program-contains tp expected-text)
  (define rendered (test-program-view-string tp))
  (unless (regexp-match? (regexp-quote expected-text) rendered)
    (with-check-info (['expected expected-text] ['rendered-view rendered])
                     (fail-check (format "View does not contain ~s" expected-text)))))

(define-check (check-test-program-done tp)
  (unless (test-program-done? tp)
    (define rendered (test-program-view-string tp))
    (with-check-info (['rendered-view rendered])
                     (fail-check "Expected program to be done, but it is still running"))))

(define-check (check-test-program-running tp)
  (when (test-program-done? tp)
    (fail-check "Expected program to be running, but it is done")))
