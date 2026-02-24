#lang racket/base

;; components/viewport.rkt
;;
;; SPDX-License-Identifier: MIT
;;
;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;; Racket port
;;
;; Viewport component for scrollable content

(require racket/string
         racket/list
         "../private/protocol.rkt"
         "../private/style.rkt"
         "../private/image.rkt")

(provide (struct-out viewport)
         make-viewport
         viewport-set-content
         viewport-content
         viewport-scroll-down
         viewport-scroll-up
         viewport-page-down
         viewport-page-up
         viewport-half-page-down
         viewport-half-page-up
         viewport-goto-top
         viewport-goto-bottom
         viewport-scroll-left
         viewport-scroll-right
         viewport-at-top?
         viewport-at-bottom?
         viewport-scroll-percent
         viewport-total-lines
         viewport-visible-lines-count)

;;; Viewport model
(struct viewport
        (width height y-offset x-offset lines mouse-wheel-enabled? mouse-wheel-delta horizontal-step)
  #:transparent
  #:methods gen:tea-model
  [(define (init vp)
     (values vp #f))
   (define (update vp msg)
     (viewport-update vp msg))
   (define (view vp)
     (viewport-view vp))])

(define (make-viewport #:width [width 80]
                       #:height [height 24]
                       #:content [content #f]
                       #:mouse-wheel-enabled [mwe #t]
                       #:mouse-wheel-delta [mwd 3]
                       #:horizontal-step [hs 4])
  (define vp (viewport width height 0 0 '() mwe mwd hs))
  (if content
      (viewport-set-content vp content)
      vp))

;;; Content management

(define (viewport-set-content vp content)
  (define new-lines (split-string-by-newline content))
  (define vp2 (struct-copy viewport vp [lines new-lines]))
  (if (> (viewport-y-offset vp2) (viewport-max-y-offset vp2))
      (viewport-goto-bottom vp2)
      vp2))

(define (viewport-content vp)
  (string-join (viewport-lines vp) "\n"))

;;; Helpers

(define (viewport-max-y-offset vp)
  (max 0 (- (length (viewport-lines vp)) (viewport-height vp))))

(define (viewport-longest-line-width vp)
  (define lines (viewport-lines vp))
  (if (null? lines)
      0
      (apply max (map visible-length lines))))

(define (viewport-visible-lines-list vp)
  (define lines (viewport-lines vp))
  (define h (viewport-height vp))
  (define y (viewport-y-offset vp))
  (define x (viewport-x-offset vp))
  (define w (viewport-width vp))
  (define longest (viewport-longest-line-width vp))

  (if (and (not (null? lines)) (> (length lines) 0))
      (let* ([top (max 0 y)]
             [bottom (min (+ y h) (length lines))]
             [visible (take (drop lines top) (- bottom top))])
        (if (and (> longest w) (> x 0))
            (map (lambda (line)
                   (define len (visible-length line))
                   (define start (min x len))
                   (define end (min (+ start w) len))
                   (if (>= start len)
                       ""
                       (substring line start end)))
                 visible)
            visible))
      '()))

;;; Status checks

(define (viewport-at-top? vp)
  (<= (viewport-y-offset vp) 0))

(define (viewport-at-bottom? vp)
  (>= (viewport-y-offset vp) (viewport-max-y-offset vp)))

(define (viewport-scroll-percent vp)
  (define lines-count (length (viewport-lines vp)))
  (define h (viewport-height vp))
  (define off (viewport-y-offset vp))
  (if (>= h lines-count)
      1.0
      (let ([max-offset (- lines-count h)])
        (if (<= max-offset 0)
            1.0
            (max 0.0 (min 1.0 (/ (exact->inexact off) max-offset)))))))

(define (viewport-total-lines vp)
  (length (viewport-lines vp)))

(define (viewport-visible-lines-count vp)
  (length (viewport-visible-lines-list vp)))

;;; Scrolling operations (all return new viewport)

(define (viewport-clamp-y-offset vp offset)
  (struct-copy viewport vp [y-offset (max 0 (min offset (viewport-max-y-offset vp)))]))

(define (viewport-clamp-x-offset vp offset)
  (struct-copy
   viewport
   vp
   [x-offset (max 0 (min offset (max 0 (- (viewport-longest-line-width vp) (viewport-width vp)))))]))

(define (viewport-scroll-down vp [n 1])
  (if (viewport-at-bottom? vp)
      vp
      (viewport-clamp-y-offset vp (+ (viewport-y-offset vp) n))))

(define (viewport-scroll-up vp [n 1])
  (if (viewport-at-top? vp)
      vp
      (viewport-clamp-y-offset vp (- (viewport-y-offset vp) n))))

(define (viewport-page-down vp)
  (viewport-scroll-down vp (viewport-height vp)))

(define (viewport-page-up vp)
  (viewport-scroll-up vp (viewport-height vp)))

(define (viewport-half-page-down vp)
  (viewport-scroll-down vp (quotient (viewport-height vp) 2)))

(define (viewport-half-page-up vp)
  (viewport-scroll-up vp (quotient (viewport-height vp) 2)))

(define (viewport-goto-top vp)
  (struct-copy viewport vp [y-offset 0]))

(define (viewport-goto-bottom vp)
  (viewport-clamp-y-offset vp (viewport-max-y-offset vp)))

(define (viewport-scroll-left vp [n #f])
  (define step (or n (viewport-horizontal-step vp)))
  (viewport-clamp-x-offset vp (- (viewport-x-offset vp) step)))

(define (viewport-scroll-right vp [n #f])
  (define step (or n (viewport-horizontal-step vp)))
  (viewport-clamp-x-offset vp (+ (viewport-x-offset vp) step)))

;;; TEA protocol

(define (viewport-update vp msg)
  (cond
    [(key-msg? msg)
     (define key (key-msg-key msg))
     (define ctrl? (key-msg-ctrl msg))
     (cond
       ;; Page down
       [(or (and (char? key) (char=? key #\space))
            (eq? key 'page-down)
            (and ctrl? (char? key) (char=? key #\f)))
        (values (viewport-page-down vp) #f)]
       ;; Page up
       [(or (eq? key 'page-up) (and ctrl? (char? key) (char=? key #\b)))
        (values (viewport-page-up vp) #f)]
       ;; Half page down
       [(and ctrl? (char? key) (char=? key #\d)) (values (viewport-half-page-down vp) #f)]
       ;; Half page up
       [(and ctrl? (char? key) (char=? key #\u)) (values (viewport-half-page-up vp) #f)]
       ;; Down/j
       [(or (eq? key 'down) (and (char? key) (char=? key #\j))) (values (viewport-scroll-down vp) #f)]
       ;; Up/k
       [(or (eq? key 'up) (and (char? key) (char=? key #\k))) (values (viewport-scroll-up vp) #f)]
       ;; Left/h
       [(or (eq? key 'left) (and (char? key) (char=? key #\h))) (values (viewport-scroll-left vp) #f)]
       ;; Right/l
       [(or (eq? key 'right) (and (char? key) (char=? key #\l)))
        (values (viewport-scroll-right vp) #f)]
       ;; Home/g
       [(or (eq? key 'home) (and (char? key) (char=? key #\g))) (values (viewport-goto-top vp) #f)]
       ;; End/G
       [(or (eq? key 'end) (and (char? key) (char=? key #\G))) (values (viewport-goto-bottom vp) #f)]
       [else (values vp #f)])]

    ;; Mouse wheel
    [(and (mouse-scroll-event? msg) (viewport-mouse-wheel-enabled? vp))
     (define dir (mouse-scroll-event-direction msg))
     (cond
       [(eq? dir 'up) (values (viewport-scroll-up vp (viewport-mouse-wheel-delta vp)) #f)]
       [(eq? dir 'down) (values (viewport-scroll-down vp (viewport-mouse-wheel-delta vp)) #f)]
       [else (values vp #f)])]

    [else (values vp #f)]))

(define (viewport-view vp)
  (define visible (viewport-visible-lines-list vp))
  (define h (viewport-height vp))
  (define actual-lines (length visible))
  (define padding-needed (max 0 (- h actual-lines)))
  (define line-imgs (map text visible))
  (define pad-imgs (make-list padding-needed (text "")))
  (apply vcat 'left (append line-imgs pad-imgs)))
