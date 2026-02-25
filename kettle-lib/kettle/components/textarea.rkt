#lang racket/base

;; components/textarea.rkt
;;
;; SPDX-License-Identifier: MIT
;;
;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;; Racket port
;;
;; Textarea component for multi-line text editing

(require racket/string
         racket/list
         racket/vector
         "../private/protocol.rkt"
         "../private/style.rkt"
         "../private/image.rkt")

(provide (struct-out textarea)
         make-textarea
         textarea-value
         textarea-set-value
         textarea-focus
         textarea-blur
         textarea-reset
         textarea-insert-string
         textarea-insert-char
         textarea-cursor-position
         textarea-line-count
         textarea-total-length)

;;; Textarea model (immutable struct, mutable vector inside for line buffer)
(struct textarea
        (width height
               lines ;; a vector of strings (internally mutable for efficiency)
               row
               col
               focused?
               placeholder
               show-line-numbers?
               prompt
               char-limit
               max-lines)
  #:transparent
  #:methods gen:kettle-model
  [(define (init ta)
     ta)
   (define (update ta msg)
     (textarea-update ta msg))
   (define (view ta)
     (textarea-view ta))])

(define (make-textarea #:width [width 40]
                       #:height [height 6]
                       #:placeholder [placeholder ""]
                       #:show-line-numbers [show-ln #t]
                       #:prompt [prompt "> "]
                       #:char-limit [char-limit 0]
                       #:max-lines [max-lines 1000])
  (textarea width height (vector "") 0 0 #f placeholder show-ln prompt char-limit max-lines))

;; Copy a textarea, cloning its line buffer so the original is unaffected
(define (ta-copy ta #:lines [lines #f] #:row [row #f] #:col [col #f] #:focused? [focused? #f])
  (textarea (textarea-width ta)
            (textarea-height ta)
            (or lines (vector-copy (textarea-lines ta)))
            (or row (textarea-row ta))
            (or col (textarea-col ta))
            (if focused?
                focused?
                (textarea-focused? ta))
            (textarea-placeholder ta)
            (textarea-show-line-numbers? ta)
            (textarea-prompt ta)
            (textarea-char-limit ta)
            (textarea-max-lines ta)))

;;; Content management

(define (textarea-set-value ta text)
  (ta-copy ta #:lines (list->vector (split-string-by-newline text)) #:row 0 #:col 0))

(define (textarea-value ta)
  (define lines (textarea-lines ta))
  (if (zero? (vector-length lines))
      ""
      (string-join (vector->list lines) "\n")))

(define (textarea-insert-string ta str)
  (define ta2 (ta-copy ta))
  (define lines (textarea-lines ta2))
  (define row (textarea-row ta2))
  (define col (textarea-col ta2))
  (define current-line (vector-ref lines row))
  (define new-lines (split-string-by-newline str))

  (if (= (length new-lines) 1)
      ;; Single line insert
      (let ([new-line (string-append (substring current-line 0 col)
                                     (first new-lines)
                                     (substring current-line col))])
        (vector-set! lines row new-line)
        (textarea (textarea-width ta2)
                  (textarea-height ta2)
                  lines
                  row
                  (+ col (string-length (first new-lines)))
                  (textarea-focused? ta2)
                  (textarea-placeholder ta2)
                  (textarea-show-line-numbers? ta2)
                  (textarea-prompt ta2)
                  (textarea-char-limit ta2)
                  (textarea-max-lines ta2)))

      ;; Multi-line insert
      (let* ([first-part (string-append (substring current-line 0 col) (first new-lines))]
             [last-part (string-append (last new-lines) (substring current-line col))]
             [middle-parts (take (drop new-lines 1) (- (length new-lines) 2))]
             [total-new (sub1 (length new-lines))]
             [old-len (vector-length lines)]
             [new-vec (make-vector (+ old-len total-new) "")])
        (for ([i (in-range row)])
          (vector-set! new-vec i (vector-ref lines i)))
        (vector-set! new-vec row first-part)
        (for ([line (in-list middle-parts)]
              [i (in-naturals (add1 row))])
          (vector-set! new-vec i line))
        (vector-set! new-vec (+ row total-new) last-part)
        (for ([i (in-range (add1 row) old-len)])
          (vector-set! new-vec (+ i total-new) (vector-ref lines i)))
        (textarea (textarea-width ta2)
                  (textarea-height ta2)
                  new-vec
                  (+ row total-new)
                  (string-length (last new-lines))
                  (textarea-focused? ta2)
                  (textarea-placeholder ta2)
                  (textarea-show-line-numbers? ta2)
                  (textarea-prompt ta2)
                  (textarea-char-limit ta2)
                  (textarea-max-lines ta2)))))

(define (textarea-insert-char ta ch)
  (textarea-insert-string ta (string ch)))

(define (textarea-total-length ta)
  (for/sum ([line (in-vector (textarea-lines ta))]) (string-length line)))

(define (textarea-line-count ta)
  (vector-length (textarea-lines ta)))

(define (textarea-cursor-position ta)
  (values (textarea-row ta) (textarea-col ta)))

(define (textarea-reset ta)
  (ta-copy ta #:lines (vector "") #:row 0 #:col 0))

(define (textarea-focus ta)
  (ta-copy ta #:focused? #t))

(define (textarea-blur ta)
  (ta-copy ta #:focused? #f))

;;; Internal helpers (all return new textarea, never mutate input)

(define (ta-current-line ta)
  (vector-ref (textarea-lines ta) (textarea-row ta)))

(define (ta-with-col ta col)
  (define line (ta-current-line ta))
  (define new-col (max 0 (min col (string-length line))))
  (ta-copy ta #:col new-col))

(define (ta-cursor-start ta)
  (ta-copy ta #:col 0))

(define (ta-cursor-end ta)
  (ta-with-col ta (string-length (ta-current-line ta))))

(define (ta-cursor-up ta)
  (if (> (textarea-row ta) 0)
      (let* ([ta2 (ta-copy ta #:row (sub1 (textarea-row ta)))]) (ta-with-col ta2 (textarea-col ta)))
      ta))

(define (ta-cursor-down ta)
  (if (< (textarea-row ta) (sub1 (textarea-line-count ta)))
      (let* ([ta2 (ta-copy ta #:row (add1 (textarea-row ta)))]) (ta-with-col ta2 (textarea-col ta)))
      ta))

(define (ta-cursor-left ta)
  (if (> (textarea-col ta) 0)
      (ta-copy ta #:col (sub1 (textarea-col ta)))
      (if (> (textarea-row ta) 0)
          (let ([ta2 (ta-copy ta #:row (sub1 (textarea-row ta)))]) (ta-cursor-end ta2))
          ta)))

(define (ta-cursor-right ta)
  (define line-len (string-length (ta-current-line ta)))
  (if (< (textarea-col ta) line-len)
      (ta-copy ta #:col (add1 (textarea-col ta)))
      (if (< (textarea-row ta) (sub1 (textarea-line-count ta)))
          (ta-copy ta #:row (add1 (textarea-row ta)) #:col 0)
          ta)))

(define (ta-delete-char-backward ta)
  (define ta2 (ta-copy ta))
  (define lines (textarea-lines ta2))
  (define row (textarea-row ta2))
  (define col (textarea-col ta2))
  (cond
    ;; At start of line - merge with previous
    [(and (zero? col) (> row 0))
     (define prev-line (vector-ref lines (sub1 row)))
     (define curr-line (vector-ref lines row))
     (define merged (string-append prev-line curr-line))
     (define new-col (string-length prev-line))
     (define new-vec (make-vector (sub1 (vector-length lines)) ""))
     (for ([i (in-range (sub1 row))])
       (vector-set! new-vec i (vector-ref lines i)))
     (vector-set! new-vec (sub1 row) merged)
     (for ([i (in-range (add1 row) (vector-length lines))])
       (vector-set! new-vec (sub1 i) (vector-ref lines i)))
     (textarea (textarea-width ta2)
               (textarea-height ta2)
               new-vec
               (sub1 row)
               new-col
               (textarea-focused? ta2)
               (textarea-placeholder ta2)
               (textarea-show-line-numbers? ta2)
               (textarea-prompt ta2)
               (textarea-char-limit ta2)
               (textarea-max-lines ta2))]
    ;; In middle of line
    [(> col 0)
     (define line (vector-ref lines row))
     (define new-line (string-append (substring line 0 (sub1 col)) (substring line col)))
     (vector-set! lines row new-line)
     (textarea (textarea-width ta2)
               (textarea-height ta2)
               lines
               row
               (sub1 col)
               (textarea-focused? ta2)
               (textarea-placeholder ta2)
               (textarea-show-line-numbers? ta2)
               (textarea-prompt ta2)
               (textarea-char-limit ta2)
               (textarea-max-lines ta2))]
    [else ta]))

(define (ta-delete-char-forward ta)
  (define ta2 (ta-copy ta))
  (define lines (textarea-lines ta2))
  (define row (textarea-row ta2))
  (define col (textarea-col ta2))
  (cond
    ;; At end of line - merge with next
    [(and (= col (string-length (vector-ref lines row))) (< row (sub1 (vector-length lines))))
     (define curr-line (vector-ref lines row))
     (define next-line (vector-ref lines (add1 row)))
     (define merged (string-append curr-line next-line))
     (define new-vec (make-vector (sub1 (vector-length lines)) ""))
     (for ([i (in-range row)])
       (vector-set! new-vec i (vector-ref lines i)))
     (vector-set! new-vec row merged)
     (for ([i (in-range (+ row 2) (vector-length lines))])
       (vector-set! new-vec (sub1 i) (vector-ref lines i)))
     (textarea (textarea-width ta2)
               (textarea-height ta2)
               new-vec
               row
               col
               (textarea-focused? ta2)
               (textarea-placeholder ta2)
               (textarea-show-line-numbers? ta2)
               (textarea-prompt ta2)
               (textarea-char-limit ta2)
               (textarea-max-lines ta2))]
    ;; In middle of line
    [(< col (string-length (vector-ref lines row)))
     (define line (vector-ref lines row))
     (define new-line (string-append (substring line 0 col) (substring line (add1 col))))
     (vector-set! lines row new-line)
     (textarea (textarea-width ta2)
               (textarea-height ta2)
               lines
               row
               col
               (textarea-focused? ta2)
               (textarea-placeholder ta2)
               (textarea-show-line-numbers? ta2)
               (textarea-prompt ta2)
               (textarea-char-limit ta2)
               (textarea-max-lines ta2))]
    [else ta]))

(define (ta-newline ta)
  (define ta2 (ta-copy ta))
  (define lines (textarea-lines ta2))
  (define row (textarea-row ta2))
  (define col (textarea-col ta2))
  (define line (vector-ref lines row))
  (define before (substring line 0 col))
  (define after (substring line col))
  (define new-vec (make-vector (add1 (vector-length lines)) ""))
  (for ([i (in-range row)])
    (vector-set! new-vec i (vector-ref lines i)))
  (vector-set! new-vec row before)
  (vector-set! new-vec (add1 row) after)
  (for ([i (in-range (add1 row) (vector-length lines))])
    (vector-set! new-vec (add1 i) (vector-ref lines i)))
  (textarea (textarea-width ta2)
            (textarea-height ta2)
            new-vec
            (add1 row)
            0
            (textarea-focused? ta2)
            (textarea-placeholder ta2)
            (textarea-show-line-numbers? ta2)
            (textarea-prompt ta2)
            (textarea-char-limit ta2)
            (textarea-max-lines ta2)))

(define (ta-kill-to-end ta)
  (define ta2 (ta-copy ta))
  (define lines (textarea-lines ta2))
  (define row (textarea-row ta2))
  (define col (textarea-col ta2))
  (define line (vector-ref lines row))
  (vector-set! lines row (substring line 0 col))
  (textarea (textarea-width ta2)
            (textarea-height ta2)
            lines
            row
            col
            (textarea-focused? ta2)
            (textarea-placeholder ta2)
            (textarea-show-line-numbers? ta2)
            (textarea-prompt ta2)
            (textarea-char-limit ta2)
            (textarea-max-lines ta2)))

(define (ta-kill-to-start ta)
  (define ta2 (ta-copy ta))
  (define lines (textarea-lines ta2))
  (define row (textarea-row ta2))
  (define col (textarea-col ta2))
  (define line (vector-ref lines row))
  (vector-set! lines row (substring line col))
  (textarea (textarea-width ta2)
            (textarea-height ta2)
            lines
            row
            0
            (textarea-focused? ta2)
            (textarea-placeholder ta2)
            (textarea-show-line-numbers? ta2)
            (textarea-prompt ta2)
            (textarea-char-limit ta2)
            (textarea-max-lines ta2)))

;;; TEA protocol

(define (textarea-update ta msg)
  (if (not (textarea-focused? ta))
      ta
      (cond
        [(key-msg? msg)
         (define key (key-msg-key msg))
         (define ctrl? (key-msg-ctrl msg))
         (cond
           [(eq? key 'up) (ta-cursor-up ta)]
           [(eq? key 'down) (ta-cursor-down ta)]
           [(eq? key 'left) (ta-cursor-left ta)]
           [(eq? key 'right) (ta-cursor-right ta)]
           [(or (eq? key 'home) (and ctrl? (char? key) (char=? key #\a)))
            (ta-cursor-start ta)]
           [(or (eq? key 'end) (and ctrl? (char? key) (char=? key #\e)))
            (ta-cursor-end ta)]
           [(eq? key 'backspace) (ta-delete-char-backward ta)]
           [(eq? key 'delete) (ta-delete-char-forward ta)]
           [(or (eq? key 'enter) (and ctrl? (char? key) (char=? key #\m)))
            (ta-newline ta)]
           [(and ctrl? (char? key) (char=? key #\k)) (ta-kill-to-end ta)]
           [(and ctrl? (char? key) (char=? key #\u)) (ta-kill-to-start ta)]
           [(and (char? key) (char-graphic? key)) (textarea-insert-char ta key)]
           [else ta])]
        [else ta])))

(define (textarea-view ta)
  (define lines (textarea-lines ta))
  (define height (textarea-height ta))
  (define show-ln (textarea-show-line-numbers? ta))
  (define prompt-str (textarea-prompt ta))
  (define row (textarea-row ta))
  (define col (textarea-col ta))
  (define focused? (textarea-focused? ta))
  (define placeholder (textarea-placeholder ta))

  (define ln-width
    (if show-ln
        (+ 2 (string-length (number->string (textarea-line-count ta))))
        0))

  (define reverse-style (make-style #:reverse #t))

  ;; If empty and has placeholder
  (if (and (string=? (textarea-value ta) "") (not (string=? placeholder "")))
      (hcat 'top
            (text prompt-str)
            (if show-ln
                (text (format "~a " (string-pad-left "1" ln-width)))
                empty-image)
            (text placeholder))
      (apply
       vcat
       'left
       (for/list ([i (in-range height)])
         (define line-text
           (if (< i (vector-length lines))
               (vector-ref lines i)
               ""))
         (define ln-img
           (cond
             [(not show-ln) empty-image]
             [(< i (vector-length lines))
              (text (format "~a " (string-pad-left (number->string (add1 i)) ln-width)))]
             [else (text (format "~a " (make-string ln-width #\space)))]))
         (define content-img
           (if (and focused? (= i row) (< i (vector-length lines)))
               ;; Line with cursor
               (let* ([before (substring line-text 0 (min col (string-length line-text)))]
                      [cursor-char (if (< col (string-length line-text))
                                       (string (string-ref line-text col))
                                       " ")]
                      [after (if (< col (string-length line-text))
                                 (substring line-text (min (add1 col) (string-length line-text)))
                                 "")])
                 (hcat 'top (text before) (styled reverse-style (text cursor-char)) (text after)))
               (text line-text)))
         (hcat 'top (text prompt-str) ln-img content-img)))))

;;; Helpers

(define (char-graphic? ch)
  (and (char? ch) (>= (char->integer ch) 32) (not (= (char->integer ch) 127))))

(define (string-pad-left str width [pad-char #\space])
  (define len (string-length str))
  (if (>= len width)
      str
      (string-append (make-string (- width len) pad-char) str)))
