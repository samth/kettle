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
         racket/format
         racket/async-channel
         (for-syntax racket/base racket/list racket/syntax)
         (only-in racket/system system)
         "protocol.rkt"
         "terminal.rkt"
         "input.rkt"
         "renderer.rkt"
         "image.rkt"
         "style.rkt"
         "errors.rkt"
         "subscriptions.rkt")

(provide (struct-out program)
         make-program
         program-send
         program-quit
         program-kill
         program-stop
         program-run
         program-show-fps!
         current-program
         define-kettle-program
         coalesce-mouse-events)

(define current-program (make-parameter #f))

(struct program
  ([model #:mutable]
   [renderer* #:mutable]
   msg-channel
   [running? #:mutable]
   [input-paused? #:mutable]
   options
   [tty-stream #:mutable]
   [input-thread #:mutable]
   [active-subs #:mutable]      ;; list of current subscription specs
   [sub-stoppers #:mutable]     ;; hash: subscription-key -> stopper thunk
   [show-fps? #:mutable]        ;; #t to overlay FPS counter
   [last-render-time #:mutable] ;; inexact milliseconds of last render
   [fps-value #:mutable])       ;; current smoothed FPS value
  #:transparent)

;; Create a new program with the given initial model.
(define (make-program model
                      #:alt-screen [alt-screen #f]
                      #:mouse [mouse #f]
                      #:show-fps [show-fps #f]
                      #:kitty-keyboard [kitty-keyboard #f])
  (when (and mouse (not (memq mouse '(cell-motion all-motion))))
    (error 'make-program "Invalid :mouse option ~s; expected 'cell-motion, 'all-motion, or #f" mouse))
  (define opts (list 'alt-screen (and alt-screen #t)
                     'mouse mouse
                     'kitty-keyboard (and kitty-keyboard #t)))
  (program model
           (make-renderer)
           (make-async-channel)
           #f  ; running?
           #f  ; input-paused?
           opts
           #f  ; tty-stream
           #f  ; input-thread
           '() ; active-subs
           (make-hash) ; sub-stoppers
           (and show-fps #t)  ; show-fps?
           #f  ; last-render-time
           0.0 ; fps-value
           ))

;; Send a message to the program's update loop.
(define (program-send p m)
  (when (program-running? p)
    (async-channel-put (program-msg-channel p) m)))

;; Quit the program gracefully.
(define (program-quit p)
  (program-send p (quit-msg)))

;; Kill the program immediately.
(define (program-kill p)
  (set-program-running?! p #f))

;; Request the program to stop.
(define (program-stop p)
  (set-program-running?! p #f))

;; Toggle FPS display at runtime.
(define (program-show-fps! p show?)
  (set-program-show-fps?! p (and show? #t)))

;; Overlay an FPS counter in the bottom-right corner of the terminal.
;; Uses exponential moving average for smooth display.
(define fps-label-style (make-style #:foreground fg-yellow #:background bg-black #:bold #t))
(define (overlay-fps img p)
  (define now (current-inexact-milliseconds))
  (define last (program-last-render-time p))
  (set-program-last-render-time! p now)
  (when last
    (define dt (- now last))
    (when (> dt 0)
      (define instant-fps (/ 1000.0 dt))
      (define old-fps (program-fps-value p))
      ;; Exponential moving average with alpha=0.2
      (define alpha 0.2)
      (define new-fps (+ (* alpha instant-fps) (* (- 1 alpha) old-fps)))
      (set-program-fps-value! p new-fps)))
  (define fps-str (format " ~a fps " (~r (program-fps-value p) #:precision '(= 0))))
  (define label (styled fps-label-style (text fps-str)))
  ;; Use terminal size so the label stays pinned regardless of image size
  (define size (get-terminal-size))
  (define tw (car size))
  (define th (cdr size))
  (define label-w (image-w label))
  (define label-h (image-h label))
  ;; Place label at bottom-right using zcat + pad
  (if (and (> tw label-w) (> th label-h))
      (zcat (pad label #:left (- tw label-w) #:top (- th label-h)) img)
      img))

(define (opts-ref opts key [default #f])
  (let loop ([l opts])
    (cond
      [(null? l) default]
      [(null? (cdr l)) default]
      [(eq? (car l) key) (cadr l)]
      [else (loop (cddr l))])))

;; Run the program's main loop. Blocks until the program exits.
(define (program-run p)
  (define opts (program-options p))
  (define alt (opts-ref opts 'alt-screen))
  (define mouse (opts-ref opts 'mouse))
  (define kitty (opts-ref opts 'kitty-keyboard))

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
       (set-program-renderer*! p (make-renderer tty-out))

       ;; Start input thread
       (define input-thd
         (thread (lambda () (input-loop p tty-in))))
       (set-program-input-thread! p input-thd)

       ;; Send initial window size
       (define size (get-terminal-size))
       (program-send p (window-size-msg (car size) (cdr size)))

       ;; Run initial command
       (define init-cmd
         (let-values ([(m cmd) (extract-update-result (init (program-model p)))])
           (set-program-model! p m)
           cmd))
       (when init-cmd
         (run-command p init-cmd))

       ;; Render initial view
       (let ([img (view (program-model p))])
         (render! (program-renderer* p)
                  (if (and (program-show-fps? p) (image? img))
                      (overlay-fps img p)
                      img)))

       ;; Start initial subscriptions
       (sync-subscriptions! p)

       ;; Enable incremental GC to reduce pause times during rendering
       (collect-garbage 'incremental)

       ;; Main event loop
       (event-loop p)

       ;; Shutdown
       (stop-all-subscriptions! p)
       (set-program-running?! p #f)
       (kill-thread input-thd))

     #:alt-screen alt
     #:mouse mouse
     #:kitty-keyboard kitty)))

;; Main event processing loop with batched message processing.
(define (event-loop p)
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

;; Coalesce consecutive mouse events in a batch:
;; - Consecutive scroll events in the same direction are merged (count incremented).
;; - Consecutive move events are collapsed to just the last one.
(define (coalesce-mouse-events messages)
  (if (null? messages)
      '()
      (let loop ([msgs messages] [result '()] [prev-scroll #f] [prev-move #f])
        (cond
          [(null? msgs)
           (reverse result)]
          [else
           (define m (car msgs))
           (define rest-msgs (cdr msgs))
           (cond
             [(mouse-scroll-event? m)
              (define dir (mouse-scroll-event-direction m))
              (if (and prev-scroll (eq? dir (mouse-scroll-event-direction prev-scroll)))
                  ;; Replace the previous scroll event with an updated count
                  (let ([updated (mouse-scroll-event
                                  (mouse-event-x prev-scroll)
                                  (mouse-event-y prev-scroll)
                                  (mouse-event-shift prev-scroll)
                                  (mouse-event-alt prev-scroll)
                                  (mouse-event-ctrl prev-scroll)
                                  dir
                                  (add1 (mouse-scroll-event-count prev-scroll)))])
                    (loop rest-msgs (cons updated (cdr result)) updated #f))
                  (loop rest-msgs (cons m result) m #f))]
             [(mouse-move-event? m)
              (if prev-move
                  ;; Replace the previous move event with this one
                  (loop rest-msgs (cons m (cdr result)) #f m)
                  (loop rest-msgs (cons m result) #f m))]
             [else
              (loop rest-msgs (cons m result) #f #f)])]))))

;; Evaluate the model's subscriptions and start/stop as needed.
(define (sync-subscriptions! p)
  (define new-subs
    (with-handlers ([exn:fail? (lambda (_) '())])
      (subscriptions (program-model p))))
  (define old-subs (program-active-subs p))
  (unless (equal? new-subs old-subs)
    (define-values (added removed _kept) (diff-subscriptions old-subs new-subs))
    ;; Stop removed subscriptions
    (for ([sub (in-list removed)])
      (define key (subscription-key sub))
      (define stopper (hash-ref (program-sub-stoppers p) key #f))
      (when stopper (stop-subscription stopper))
      (hash-remove! (program-sub-stoppers p) key))
    ;; Start added subscriptions
    (for ([sub (in-list added)])
      (define key (subscription-key sub))
      (define stopper (start-subscription sub (lambda (m) (program-send p m))))
      (when (procedure? stopper)
        (hash-set! (program-sub-stoppers p) key stopper)))
    (set-program-active-subs! p new-subs)))

;; Stop all active subscriptions.
(define (stop-all-subscriptions! p)
  (for ([(key stopper) (in-hash (program-sub-stoppers p))])
    (stop-subscription stopper))
  (hash-clear! (program-sub-stoppers p))
  (set-program-active-subs! p '()))

;; Process multiple messages, rendering only once at the end.
(define (handle-messages-batch p messages)
  (define coalesced (coalesce-mouse-events messages))
  (define should-render #f)
  (define pending-cmds '())

  (let/ec return
    (for ([m (in-list coalesced)])
      (cond
        ;; Quit message
        [(quit-msg? m)
         (set-program-running?! p #f)
         (return (void))]
        ;; Internal: Kitty keyboard protocol query response (discard)
        [(kitty-query-response-msg? m) (void)]
        ;; FPS toggle hotkey: Alt+F
        [(and (key-msg? m)
              (key-msg-alt m)
              (let ([k (key-msg-key m)])
                (and (char? k) (char=? k #\f))))
         (set-program-show-fps?! p (not (program-show-fps? p)))
         (set! should-render #t)]
        ;; All other messages
        [else
         (define-values (new-model cmd)
           (extract-update-result (update (program-model p) m)))
         (set-program-model! p new-model)
         (set! should-render #t)
         (when cmd
           (set! pending-cmds (append pending-cmds (list cmd))))])))

  ;; Run accumulated commands
  (for ([cmd (in-list pending-cmds)])
    (run-command p cmd))

  ;; Render once
  (when should-render
    (define img (view (program-model p)))
    (define final-img
      (if (and (program-show-fps? p) (image? img))
          (overlay-fps img p)
          img))
    (render! (program-renderer* p) final-img)
    ;; Re-evaluate subscriptions after model changes
    (sync-subscriptions! p)))

;; Execute a command.
(define (run-command p cmd)
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

;; Run an external program with full TUI suspension.
(define (run-exec-command p cmd)
  (define opts (program-options p))
  (define alt (opts-ref opts 'alt-screen))
  (define mouse (opts-ref opts 'mouse))
  (define kitty (opts-ref opts 'kitty-keyboard))
  (define exec-prog (exec-cmd-program cmd))
  (define exec-args (exec-cmd-args cmd))
  (define callback (exec-cmd-callback cmd))

  ;; Pause input
  (set-program-input-paused?! p #t)

  ;; Suspend terminal
  (define restore-fn (suspend-terminal #:alt-screen alt #:mouse mouse #:kitty-keyboard kitty))

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

;; Run multiple commands concurrently.
(define (run-batch p cmds)
  (for ([cmd (in-list cmds)])
    (when cmd
      (run-command p cmd))))

;; Run multiple commands in sequence.
(define (run-sequence p cmds)
  (thread
   (lambda ()
     (for ([cmd (in-list cmds)])
       (when (and cmd (program-running? p))
         (define m (cmd))
         (when m
           (program-send p m)))))))

;; Read input and send messages to the program.
(define (input-loop p tty-in)
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

;;; Convenience macro -- generates immutable structs with optional clauses.
;;
;; Usage:
;;   (define-kettle-program counter
;;     #:fields ([count 0])
;;     #:view
;;     (lambda (self) (text (format "Count: ~a" (counter-count self)))))
;;
;; Only #:view is required. Defaults:
;;   #:init     -> (lambda (self) (values self #f))
;;   #:update   -> (lambda (self msg) (values self #f))
(define-syntax (define-kettle-program stx)
  (define (keyword-stx? s kw)
    (and (keyword? (syntax-e s)) (equal? (syntax-e s) kw)))
  (syntax-case stx ()
    [(_ name clause ...)
     (let ()
       (define clauses (syntax->list #'(clause ...)))
       ;; Parse keyword clauses
       (define fields-stx #f)
       (define init-stx #f)
       (define update-stx #f)
       (define view-stx #f)
       (define subs-stx #f)
       (let loop ([cs clauses])
         (cond
           [(null? cs) (void)]
           [(keyword-stx? (car cs) '#:fields)
            (set! fields-stx (cadr cs))
            (loop (cddr cs))]
           [(keyword-stx? (car cs) '#:init)
            (set! init-stx (cadr cs))
            (loop (cddr cs))]
           [(keyword-stx? (car cs) '#:update)
            (set! update-stx (cadr cs))
            (loop (cddr cs))]
           [(keyword-stx? (car cs) '#:view)
            (set! view-stx (cadr cs))
            (loop (cddr cs))]
           [(keyword-stx? (car cs) '#:subscriptions)
            (set! subs-stx (cadr cs))
            (loop (cddr cs))]
           [else
            (raise-syntax-error 'define-kettle-program
                                "unknown clause"
                                (car cs))]))
       (unless view-stx
         (raise-syntax-error 'define-kettle-program
                             "missing required #:view clause" stx))
       ;; Parse field specs
       (define field-pairs
         (if fields-stx
             (map (lambda (fp)
                    (syntax-case fp ()
                      [(fname fdefault) (list #'fname #'fdefault)]))
                  (syntax->list fields-stx))
             '()))
       (define field-names (map car field-pairs))
       (define field-defaults (map cadr field-pairs))
       (with-syntax ([(fname ...) field-names]
                     [(fdefault ...) field-defaults]
                     [make-name (format-id #'name "make-~a" #'name)]
                     [init-expr (or init-stx #'(lambda (self) self))]
                     [update-expr (or update-stx #'(lambda (self msg) self))]
                     [view-expr view-stx]
                     [subs-expr (or subs-stx #'(lambda (self) '()))])
         #'(begin
             (struct name (fname ...)
               #:transparent
               #:methods gen:kettle-model
               [(define (init model) (init-expr model))
                (define (update model msg) (update-expr model msg))
                (define (view model) (view-expr model))
                (define (subscriptions model) (subs-expr model))])
             (define (make-name #:fname [fname fdefault] ...)
               (name fname ...)))))]))
