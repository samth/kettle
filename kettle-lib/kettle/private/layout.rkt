#lang racket/base

;; layout.rkt
;;
;; SPDX-License-Identifier: MIT
;;
;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;; Racket port
;;
;; Layout utilities - joining and positioning text blocks

(require racket/string
         racket/list
         racket/math
         "style.rkt")

(provide ;; Position constants
         top middle bottom
         left center right

         ;; Layout functions
         join-horizontal
         join-vertical
         place-horizontal
         place-vertical
         place
         block-width
         block-height)

;;; Position constants
(define top 'top)
(define middle 'middle)
(define bottom 'bottom)
(define left 'left)
(define center 'center)
(define right 'right)

(define (calculate-offset position current-idx max-size content-size)
  (define space (- max-size content-size))
  (cond
    [(eq? position 'top) 0]
    [(eq? position 'middle) (quotient space 2)]
    [(eq? position 'bottom) space]
    [(eq? position 'left) 0]
    [(eq? position 'center) (quotient space 2)]
    [(eq? position 'right) space]
    [(number? position)
     (define pos (max 0 (min 1 position)))
     (exact-floor (* space pos))]
    [else 0]))

(define (join-horizontal position . blocks)
  "Join text blocks horizontally at the given vertical position."
  (when (null? blocks)
    (return ""))
  (define block-lines (map split-string-by-newline blocks))
  (define block-widths
    (map (lambda (ls)
           (if (null? ls) 0 (apply max (map visible-length ls))))
         block-lines))
  (define max-height (apply max (map length block-lines)))
  (define result '())

  (for ([i (in-range max-height)])
    (define line "")
    (for ([lines (in-list block-lines)]
          [bw (in-list block-widths)])
      (define h (length lines))
      (define offset (calculate-offset position i max-height h))
      (define idx (- i offset))
      (if (and (>= idx 0) (< idx h))
          (let* ([current-line (list-ref lines idx)]
                 [line-width (visible-length current-line)]
                 [padding (make-string (max 0 (- bw line-width)) #\space)])
            (set! line (string-append line current-line padding)))
          (set! line (string-append line (make-string bw #\space)))))
    (set! result (append result (list line))))

  (string-join result "\n"))

(define (return val) val)  ; just returns

(define (join-vertical position . blocks)
  "Join text blocks vertically at the given horizontal position."
  (when (null? blocks)
    (return ""))
  (define block-lines (map split-string-by-newline blocks))
  (define max-width
    (apply max
           (map (lambda (lines)
                  (if (null? lines) 0 (apply max (map visible-length lines))))
                block-lines)))
  (define result '())

  (for ([lines (in-list block-lines)])
    (for ([line (in-list lines)])
      (set! result (append result (list (align-text line max-width position))))))

  (string-join result "\n"))

(define (align-text text width position
                    #:whitespace-char [ws-char #f]
                    #:whitespace-fg [ws-fg #f])
  (define vlen (visible-length text))
  (define padding-cols (max 0 (- width vlen)))
  (define actual-char (or ws-char #\space))
  (define cw (char-display-width actual-char))
  (define padding-chars (if (> cw 0) (quotient padding-cols cw) 0))
  (define (make-pad n)
    (define s (make-string n actual-char))
    (if ws-fg (colored s #:fg ws-fg) s))

  (cond
    [(or (eq? position 'left) (eq? position 'top))
     (string-append text (make-pad padding-chars))]
    [(or (eq? position 'right) (eq? position 'bottom))
     (string-append (make-pad padding-chars) text)]
    [(or (eq? position 'center) (eq? position 'middle))
     (define left-cols (quotient padding-cols 2))
     (define right-cols (- padding-cols left-cols))
     (define left-chars (if (> cw 0) (quotient left-cols cw) 0))
     (define right-chars (if (> cw 0) (quotient right-cols cw) 0))
     (string-append (make-pad left-chars) text (make-pad right-chars))]
    [(number? position)
     (define left-cols (exact-floor (* padding-cols position)))
     (define right-cols (- padding-cols left-cols))
     (define left-chars (if (> cw 0) (quotient left-cols cw) 0))
     (define right-chars (if (> cw 0) (quotient right-cols cw) 0))
     (string-append (make-pad left-chars) text (make-pad right-chars))]
    [else text]))

(define (place-horizontal width position text
                          #:whitespace-char [ws-char #f]
                          #:whitespace-fg [ws-fg #f])
  "Place text horizontally in a space of given width."
  (string-join
   (map (lambda (line)
          (align-text line width position
                      #:whitespace-char ws-char
                      #:whitespace-fg ws-fg))
        (split-string-by-newline text))
   "\n"))

(define (place-vertical height position text)
  "Place text vertically in a space of given height."
  (define lines (split-string-by-newline text))
  (define text-height (length lines))
  (cond
    [(<= height 0) ""]
    [(<= text-height height)
     (define padding (- height text-height))
     (define offset (calculate-offset position 0 height text-height))
     (define top-pad (make-list offset ""))
     (define bottom-pad (make-list (- padding offset) ""))
     (string-join (append top-pad lines bottom-pad) "\n")]
    [else
     (define extra (- text-height height))
     (define start
       (cond
         [(or (eq? position 'top) (eq? position 'left)) 0]
         [(or (eq? position 'bottom) (eq? position 'right)) extra]
         [(or (eq? position 'middle) (eq? position 'center)) (quotient extra 2)]
         [(number? position) (exact-floor (* extra (max 0 (min 1 position))))]
         [else 0]))
     (string-join (take (drop lines start) height) "\n")]))

(define (place width height h-pos v-pos text
               #:whitespace-char [ws-char #f]
               #:whitespace-fg [ws-fg #f])
  "Place text in a width x height space."
  (place-horizontal width h-pos
                    (place-vertical height v-pos text)
                    #:whitespace-char ws-char
                    #:whitespace-fg ws-fg))

(define (block-width text)
  (if (string? text)
      (let ([lines (split-string-by-newline text)])
        (if (null? lines) 0 (apply max (map visible-length lines))))
      0))

(define (block-height text)
  (length (split-string-by-newline text)))
