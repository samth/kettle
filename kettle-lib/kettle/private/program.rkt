#lang racket/base

;; program.rkt
;;
;; SPDX-License-Identifier: MIT
;;
;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;; Racket port
;;
;; Main program loop and runtime

(require racket/match
         racket/list
         racket/string
         racket/async-channel
         (only-in racket/system system)
         "protocol.rkt"
         "terminal.rkt"
         "input.rkt"
         "renderer.rkt"
         "errors.rkt")

(provide (struct-out program)
         make-program
         program-send
         program-quit
         program-kill
         program-stop
         program-run
         current-program
         defprogram)

(define current-program (make-parameter #f))

(struct program
  ([model #:mutable]
   [renderer* #:mutable]
   msg-channel
   [running? #:mutable]
   [input-paused? #:mutable]
   options
   [tty-stream #:mutable]
   [input-thread #:mutable])
  #:transparent)

(define (make-program model
                      #:alt-screen [alt-screen #f]
                      #:mouse [mouse #f])
  "Create a new program with the given initial model."
  (when (and mouse (not (memq mouse '(cell-motion all-motion))))
    (error 'make-program "Invalid :mouse option ~s; expected 'cell-motion, 'all-motion, or #f" mouse))
  (define opts (list 'alt-screen (and alt-screen #t)
                     'mouse mouse))
  (program model
           (make-renderer)
           (make-async-channel)
           #f  ; running?
           #f  ; input-paused?
           opts
           #f  ; tty-stream
           #f  ; input-thread
           ))

(define (program-send p m)
  "Send a message to the program's update loop."
  (when (program-running? p)
    (async-channel-put (program-msg-channel p) m)))

(define (program-quit p)
  "Quit the program gracefully."
  (program-send p (quit-msg)))

(define (program-kill p)
  "Kill the program immediately."
  (set-program-running?! p #f))

(define (program-stop p)
  "Request the program to stop."
  (set-program-running?! p #f))

(define (opts-ref opts key [default #f])
  (let loop ([l opts])
    (cond
      [(null? l) default]
      [(null? (cdr l)) default]
      [(eq? (car l) key) (cadr l)]
      [else (loop (cddr l))])))

(define (program-run p)
  "Run the program's main loop. Blocks until the program exits."
  (define opts (program-options p))
  (define alt (opts-ref opts 'alt-screen))
  (define mouse (opts-ref opts 'mouse))

  (parameterize ([current-program p])
    ;; Open /dev/tty
    (define tty (open-tty))
    (define tty-in (first tty))
    (define tty-out (second tty))

    (with-raw-terminal
     (lambda ()
       (set-program-running?! p #t)

       ;; Set up tty streams
       (set-program-tty-stream! p tty)
       (set-program-renderer*! p (renderer "" tty-out))

       ;; Start input thread
       (define input-thd
         (thread (lambda () (input-loop p tty-in))))
       (set-program-input-thread! p input-thd)

       ;; Send initial window size
       (define size (get-terminal-size))
       (program-send p (window-size-msg (car size) (cdr size)))

       ;; Run initial command
       (define init-cmd
         (let-values ([(m cmd) (init (program-model p))])
           (set-program-model! p m)
           cmd))
       (when init-cmd
         (run-command p init-cmd))

       ;; Render initial view
       (render! (program-renderer* p) (view (program-model p)))

       ;; Main event loop
       (event-loop p)

       ;; Shutdown
       (set-program-running?! p #f)
       (kill-thread input-thd))

     #:alt-screen alt
     #:mouse mouse)))

(define (event-loop p)
  "Main event processing loop with batched message processing."
  (let loop ()
    (when (program-running? p)
      (with-handlers ([exn:fail?
                       (lambda (e)
                         (handle-kettle-error 'event-loop e))])
        ;; Block with timeout
        (define first-msg
          (sync/timeout 0.1 (program-msg-channel p)))
        (when first-msg
          ;; Drain all pending messages
          (define messages
            (let drain ([msgs (list first-msg)])
              (define next (async-channel-try-get (program-msg-channel p)))
              (if next
                  (drain (append msgs (list next)))
                  msgs)))
          ;; Process batch
          (handle-messages-batch p messages)))
      (loop))))

(define (coalesce-scroll-events messages)
  "Combine consecutive scroll events in the same direction."
  (if (null? messages)
      '()
      (let ([result '()]
            [prev-scroll #f])
        (for ([m (in-list messages)])
          (cond
            [(mouse-scroll-event? m)
             (define dir (mouse-scroll-event-direction m))
             (if (and prev-scroll (eq? dir (mouse-scroll-event-direction prev-scroll)))
                 (set-mouse-scroll-event-count! prev-scroll
                                                (add1 (mouse-scroll-event-count prev-scroll)))
                 (begin
                   (set! result (append result (list m)))
                   (set! prev-scroll m)))]
            [else
             (set! result (append result (list m)))
             (set! prev-scroll #f)]))
        result)))

(define (handle-messages-batch p messages)
  "Process multiple messages, rendering only once at the end."
  (define coalesced (coalesce-scroll-events messages))
  (define should-render #f)
  (define pending-cmds '())

  (let/ec return
    (for ([m (in-list coalesced)])
      (cond
        ;; Quit message
        [(quit-msg? m)
         (set-program-running?! p #f)
         (return (void))]
        ;; All other messages
        [else
         (define-values (new-model cmd)
           (update (program-model p) m))
         (set-program-model! p new-model)
         (set! should-render #t)
         (when cmd
           (set! pending-cmds (append pending-cmds (list cmd))))])))

  ;; Run accumulated commands
  (for ([cmd (in-list pending-cmds)])
    (run-command p cmd))

  ;; Render once
  (when should-render
    (render! (program-renderer* p) (view (program-model p)))))

(define (run-command p cmd)
  "Execute a command."
  (cond
    ;; Nil command
    [(not cmd) (void)]

    ;; Exec command
    [(exec-cmd? cmd)
     (run-exec-command p cmd)]

    ;; Batch commands
    [(list? cmd)
     (if (and (not (null? cmd)) (eq? (first cmd) 'sequence))
         (run-sequence p (rest cmd))
         (run-batch p cmd))]

    ;; Single command function
    [(procedure? cmd)
     (thread
      (lambda ()
        (with-handlers ([exn:fail?
                         (lambda (e)
                           (handle-kettle-error 'command e))])
          (define m (cmd))
          (when m
            (program-send p m)))))]

    [else (void)]))

(define (run-exec-command p cmd)
  "Run an external program with full TUI suspension."
  (define opts (program-options p))
  (define alt (opts-ref opts 'alt-screen))
  (define mouse (opts-ref opts 'mouse))
  (define exec-prog (exec-cmd-program cmd))
  (define exec-args (exec-cmd-args cmd))
  (define callback (exec-cmd-callback cmd))

  ;; Pause input
  (set-program-input-paused?! p #t)

  ;; Suspend terminal
  (define restore-fn (suspend-terminal #:alt-screen alt #:mouse mouse))

  (dynamic-wind
    void
    (lambda ()
      (with-handlers ([exn:fail?
                       (lambda (e)
                         (handle-kettle-error 'exec-command e))])
        (define cmd-line (if (null? exec-args)
                             exec-prog
                             (string-append exec-prog " "
                                            (string-join exec-args " "))))
        (system cmd-line)))
    (lambda ()
      ;; Always restore
      (when restore-fn
        (resume-terminal restore-fn))
      ;; Resume input
      (set-program-input-paused?! p #f)
      ;; Send window size to trigger redraw
      (define size (get-terminal-size))
      (program-send p (window-size-msg (car size) (cdr size)))
      ;; Callback
      (when callback
        (define m (callback))
        (when m
          (program-send p m))))))

(define (run-batch p cmds)
  "Run multiple commands concurrently."
  (for ([cmd (in-list cmds)])
    (when cmd
      (run-command p cmd))))

(define (run-sequence p cmds)
  "Run multiple commands in sequence."
  (thread
   (lambda ()
     (for ([cmd (in-list cmds)])
       (when (and cmd (program-running? p))
         (define m (cmd))
         (when m
           (program-send p m)))))))

(define (input-loop p tty-in)
  "Read input and send messages to the program."
  (parameterize ([current-input-stream tty-in])
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (handle-kettle-error 'input-loop e))])
      (let loop ()
        (when (program-running? p)
          (with-handlers ([exn:fail?
                           (lambda (e)
                             (handle-kettle-error 'input-loop e))])
            (if (program-input-paused? p)
                (sleep 0.05)
                (let ([events (read-all-available-events)])
                  (if (not (null? events))
                      (for ([evt (in-list events)])
                        (program-send p evt))
                      (sleep 0.001)))))
          (loop))))))

;;; Convenience macro
(define-syntax-rule (defprogram name
                      #:fields ([field-name field-default] ...)
                      #:init init-body
                      #:update update-body
                      #:view view-body)
  (begin
    (struct name (field-name ...)
      #:transparent
      #:mutable
      #:methods gen:tea-model
      [(define (init model) init-body)
       (define (update model msg) update-body)
       (define (view model) view-body)])
    (void)))
