#lang racket/base

;; subscriptions.rkt
;;
;; SPDX-License-Identifier: MIT
;;
;; Elm-style subscriptions for Kettle.
;; Subscriptions produce messages at specified intervals or from external sources.

(require racket/match
         "protocol.rkt"
         "terminal.rkt")

;; Subscription constructors
(provide every
         on-resize
         watch-port

         ;; Subscription types
         (struct-out sub:every)
         (struct-out sub:resize)
         (struct-out sub:port)
         subscription?
         subscription-key

         ;; Runner
         start-subscription
         stop-subscription
         diff-subscriptions)

;;; Subscription types

(struct subscription (key) #:transparent)
(struct sub:every subscription (interval make-msg) #:transparent)
(struct sub:resize subscription (make-msg) #:transparent)
(struct sub:port subscription (port make-msg) #:transparent)

;;; Smart constructors

(define (every interval make-msg #:key [key #f])
  (sub:every (or key (list 'every interval)) interval make-msg))

(define (on-resize make-msg #:key [key 'resize])
  (sub:resize key make-msg))

(define (watch-port port make-msg #:key [key #f])
  (sub:port (or key (list 'port port)) port make-msg))

;;; Running subscriptions
;; Each active subscription has a thread. Returns a stopper thunk.

(define (start-subscription sub send-msg!)
  (match sub
    [(sub:every _ interval make-msg)
     (define thd
       (thread (lambda ()
                 (let loop ()
                   (sleep interval)
                   (send-msg! (make-msg (current-inexact-milliseconds)))
                   (loop)))))
     (lambda () (kill-thread thd))]

    [(sub:resize _ make-msg)
     (define thd
       (thread (lambda ()
                 (define last-size (get-terminal-size))
                 (let loop ()
                   (sleep 0.25)
                   (define size (get-terminal-size))
                   (unless (equal? size last-size)
                     (set! last-size size)
                     (send-msg! (make-msg (car size) (cdr size))))
                   (loop)))))
     (lambda () (kill-thread thd))]

    [(sub:port _ port make-msg)
     (define thd
       (thread (lambda ()
                 (define buf (make-bytes 4096))
                 (let loop ()
                   (sync port)
                   (define count (read-bytes-avail!* buf port))
                   (when (and count (not (eof-object? count)) (> count 0))
                     (send-msg! (make-msg (subbytes buf 0 count)))
                     (loop))))))
     (lambda () (kill-thread thd))]))

(define (stop-subscription stopper)
  (when (procedure? stopper)
    (stopper)))

;; Diff two lists of subscriptions by key, returning (added removed kept)
(define (diff-subscriptions old-subs new-subs)
  (define old-keys (map subscription-key old-subs))
  (define new-keys (map subscription-key new-subs))
  (define added (filter (lambda (s) (not (member (subscription-key s) old-keys))) new-subs))
  (define removed (filter (lambda (s) (not (member (subscription-key s) new-keys))) old-subs))
  (define kept (filter (lambda (s) (member (subscription-key s) old-keys)) new-subs))
  (values added removed kept))
