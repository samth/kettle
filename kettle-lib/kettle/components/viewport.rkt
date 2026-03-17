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
  #:methods gen:kettle-model
  [(define (init vp)
     vp)
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
        (map (lambda (line)
               (define shifted
                 (if (> x 0)
                     (let ([len (string-length line)])
                       (if (>= x len) "" (substring line x)))
                     line))
               (truncate-text shifted w #:ellipsis ""))
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
        (viewport-page-down vp)]
       ;; Page up
       [(or (eq? key 'page-up) (and ctrl? (char? key) (char=? key #\b)))
        (viewport-page-up vp)]
       ;; Half page down
       [(and ctrl? (char? key) (char=? key #\d)) (viewport-half-page-down vp)]
       ;; Half page up
       [(and ctrl? (char? key) (char=? key #\u)) (viewport-half-page-up vp)]
       ;; Down/j
       [(or (eq? key 'down) (and (char? key) (char=? key #\j))) (viewport-scroll-down vp)]
       ;; Up/k
       [(or (eq? key 'up) (and (char? key) (char=? key #\k))) (viewport-scroll-up vp)]
       ;; Left/h
       [(or (eq? key 'left) (and (char? key) (char=? key #\h))) (viewport-scroll-left vp)]
       ;; Right/l
       [(or (eq? key 'right) (and (char? key) (char=? key #\l)))
        (viewport-scroll-right vp)]
       ;; Home/g
       [(or (eq? key 'home) (and (char? key) (char=? key #\g))) (viewport-goto-top vp)]
       ;; End/G
       [(or (eq? key 'end) (and (char? key) (char=? key #\G))) (viewport-goto-bottom vp)]
       [else vp])]

    ;; Mouse wheel
    [(and (mouse-scroll-event? msg) (viewport-mouse-wheel-enabled? vp))
     (define dir (mouse-scroll-event-direction msg))
     (cond
       [(eq? dir 'up) (viewport-scroll-up vp (viewport-mouse-wheel-delta vp))]
       [(eq? dir 'down) (viewport-scroll-down vp (viewport-mouse-wheel-delta vp))]
       [else vp])]

    [else vp]))

(define (viewport-view vp)
  (define visible (viewport-visible-lines-list vp))
  (define h (viewport-height vp))
  (define actual-lines (length visible))
  (define padding-needed (max 0 (- h actual-lines)))
  ;; Empty strings must produce 1-height images (not 0-height blank)
  ;; so that blank lines in content are preserved as visual spacing
  (define line-imgs (map (lambda (l) (if (string=? l "") (text " ") (text l))) visible))
  (define pad-imgs (make-list padding-needed (text "")))
  (apply vcat 'left (append line-imgs pad-imgs)))
