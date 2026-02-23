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
         "../private/style.rkt")

(provide (struct-out textarea)
         make-textarea
         textarea-init
         textarea-update
         textarea-view
         textarea-value
         textarea-set-value!
         textarea-focus!
         textarea-blur!
         textarea-reset!
         textarea-insert-string!
         textarea-insert-char!
         textarea-cursor-position
         textarea-line-count
         textarea-total-length)

;;; Textarea model
(struct textarea
  (width
   height
   [lines #:mutable]
   [row #:mutable]
   [col #:mutable]
   [focused? #:mutable]
   placeholder
   show-line-numbers?
   prompt
   char-limit
   max-lines)
  #:transparent)

(define (make-textarea #:width [width 40]
                       #:height [height 6]
                       #:placeholder [placeholder ""]
                       #:show-line-numbers [show-ln #t]
                       #:prompt [prompt "> "]
                       #:char-limit [char-limit 0]
                       #:max-lines [max-lines 1000])
  (textarea width height (vector "") 0 0 #f placeholder show-ln prompt char-limit max-lines))

;;; Content management

(define (textarea-set-value! ta text)
  (set-textarea-lines! ta (list->vector (split-string-by-newline text)))
  (set-textarea-row! ta 0)
  (set-textarea-col! ta 0)
  ta)

(define (textarea-value ta)
  (define lines (textarea-lines ta))
  (if (zero? (vector-length lines))
      ""
      (string-join (vector->list lines) "\n")))

(define (textarea-insert-string! ta str)
  (define lines (textarea-lines ta))
  (define row (textarea-row ta))
  (define col (textarea-col ta))
  (define current-line (vector-ref lines row))
  (define new-lines (split-string-by-newline str))

  (if (= (length new-lines) 1)
      ;; Single line insert
      (let ([new-line (string-append (substring current-line 0 col)
                                     (first new-lines)
                                     (substring current-line col))])
        (vector-set! lines row new-line)
        (set-textarea-col! ta (+ col (string-length (first new-lines)))))

      ;; Multi-line insert
      (let* ([first-part (string-append (substring current-line 0 col)
                                         (first new-lines))]
             [last-part (string-append (last new-lines)
                                       (substring current-line col))]
             [middle-parts (take (drop new-lines 1) (- (length new-lines) 2))]
             [total-new (sub1 (length new-lines))]
             [old-len (vector-length lines)]
             [new-vec (make-vector (+ old-len total-new) "")])
        ;; Copy lines before
        (for ([i (in-range row)])
          (vector-set! new-vec i (vector-ref lines i)))
        ;; First part
        (vector-set! new-vec row first-part)
        ;; Middle parts
        (for ([line (in-list middle-parts)]
              [i (in-naturals (add1 row))])
          (vector-set! new-vec i line))
        ;; Last part
        (vector-set! new-vec (+ row total-new) last-part)
        ;; Copy remaining
        (for ([i (in-range (add1 row) old-len)])
          (vector-set! new-vec (+ i total-new) (vector-ref lines i)))
        (set-textarea-lines! ta new-vec)
        (set-textarea-row! ta (+ row total-new))
        (set-textarea-col! ta (string-length (last new-lines)))))
  ta)

(define (textarea-insert-char! ta ch)
  (textarea-insert-string! ta (string ch)))

(define (textarea-total-length ta)
  (for/sum ([line (in-vector (textarea-lines ta))])
    (string-length line)))

(define (textarea-line-count ta)
  (vector-length (textarea-lines ta)))

(define (textarea-cursor-position ta)
  (values (textarea-row ta) (textarea-col ta)))

(define (textarea-reset! ta)
  (set-textarea-lines! ta (vector ""))
  (set-textarea-row! ta 0)
  (set-textarea-col! ta 0)
  ta)

(define (textarea-focus! ta)
  (set-textarea-focused?! ta #t)
  ta)

(define (textarea-blur! ta)
  (set-textarea-focused?! ta #f)
  ta)

;;; Internal helpers

(define (ta-current-line ta)
  (vector-ref (textarea-lines ta) (textarea-row ta)))

(define (ta-set-cursor! ta col)
  (define line (ta-current-line ta))
  (set-textarea-col! ta (max 0 (min col (string-length line)))))

(define (ta-cursor-start! ta)
  (set-textarea-col! ta 0) ta)

(define (ta-cursor-end! ta)
  (ta-set-cursor! ta (string-length (ta-current-line ta))) ta)

(define (ta-cursor-up! ta)
  (when (> (textarea-row ta) 0)
    (set-textarea-row! ta (sub1 (textarea-row ta)))
    (ta-set-cursor! ta (textarea-col ta)))
  ta)

(define (ta-cursor-down! ta)
  (when (< (textarea-row ta) (sub1 (textarea-line-count ta)))
    (set-textarea-row! ta (add1 (textarea-row ta)))
    (ta-set-cursor! ta (textarea-col ta)))
  ta)

(define (ta-cursor-left! ta)
  (if (> (textarea-col ta) 0)
      (set-textarea-col! ta (sub1 (textarea-col ta)))
      (when (> (textarea-row ta) 0)
        (set-textarea-row! ta (sub1 (textarea-row ta)))
        (ta-cursor-end! ta)))
  ta)

(define (ta-cursor-right! ta)
  (define line-len (string-length (ta-current-line ta)))
  (if (< (textarea-col ta) line-len)
      (set-textarea-col! ta (add1 (textarea-col ta)))
      (when (< (textarea-row ta) (sub1 (textarea-line-count ta)))
        (set-textarea-row! ta (add1 (textarea-row ta)))
        (set-textarea-col! ta 0)))
  ta)

(define (ta-delete-char-backward! ta)
  (define lines (textarea-lines ta))
  (define row (textarea-row ta))
  (define col (textarea-col ta))
  (cond
    ;; At start of line - merge with previous
    [(and (zero? col) (> row 0))
     (define prev-line (vector-ref lines (sub1 row)))
     (define curr-line (vector-ref lines row))
     (define merged (string-append prev-line curr-line))
     (define new-col (string-length prev-line))
     ;; Build new vector without current line
     (define new-vec (make-vector (sub1 (vector-length lines)) ""))
     (for ([i (in-range (sub1 row))])
       (vector-set! new-vec i (vector-ref lines i)))
     (vector-set! new-vec (sub1 row) merged)
     (for ([i (in-range (add1 row) (vector-length lines))])
       (vector-set! new-vec (sub1 i) (vector-ref lines i)))
     (set-textarea-lines! ta new-vec)
     (set-textarea-row! ta (sub1 row))
     (set-textarea-col! ta new-col)]
    ;; In middle of line
    [(> col 0)
     (define line (vector-ref lines row))
     (define new-line (string-append (substring line 0 (sub1 col))
                                     (substring line col)))
     (vector-set! lines row new-line)
     (set-textarea-col! ta (sub1 col))])
  ta)

(define (ta-delete-char-forward! ta)
  (define lines (textarea-lines ta))
  (define row (textarea-row ta))
  (define col (textarea-col ta))
  (cond
    ;; At end of line - merge with next
    [(and (= col (string-length (vector-ref lines row)))
          (< row (sub1 (vector-length lines))))
     (define curr-line (vector-ref lines row))
     (define next-line (vector-ref lines (add1 row)))
     (define merged (string-append curr-line next-line))
     (define new-vec (make-vector (sub1 (vector-length lines)) ""))
     (for ([i (in-range row)])
       (vector-set! new-vec i (vector-ref lines i)))
     (vector-set! new-vec row merged)
     (for ([i (in-range (+ row 2) (vector-length lines))])
       (vector-set! new-vec (sub1 i) (vector-ref lines i)))
     (set-textarea-lines! ta new-vec)]
    ;; In middle of line
    [(< col (string-length (vector-ref lines row)))
     (define line (vector-ref lines row))
     (define new-line (string-append (substring line 0 col)
                                     (substring line (add1 col))))
     (vector-set! lines row new-line)])
  ta)

(define (ta-newline! ta)
  (define lines (textarea-lines ta))
  (define row (textarea-row ta))
  (define col (textarea-col ta))
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
  (set-textarea-lines! ta new-vec)
  (set-textarea-row! ta (add1 row))
  (set-textarea-col! ta 0)
  ta)

;;; TEA protocol

(define (textarea-init ta)
  (values ta #f))

(define (textarea-update ta msg)
  (unless (textarea-focused? ta)
    (values ta #f))

  (cond
    [(key-msg? msg)
     (define key (key-msg-key msg))
     (define ctrl? (key-msg-ctrl msg))
     (cond
       [(eq? key 'up) (values (ta-cursor-up! ta) #f)]
       [(eq? key 'down) (values (ta-cursor-down! ta) #f)]
       [(eq? key 'left) (values (ta-cursor-left! ta) #f)]
       [(eq? key 'right) (values (ta-cursor-right! ta) #f)]
       [(or (eq? key 'home) (and ctrl? (char? key) (char=? key #\a)))
        (values (ta-cursor-start! ta) #f)]
       [(or (eq? key 'end) (and ctrl? (char? key) (char=? key #\e)))
        (values (ta-cursor-end! ta) #f)]
       [(eq? key 'backspace)
        (values (ta-delete-char-backward! ta) #f)]
       [(eq? key 'delete)
        (values (ta-delete-char-forward! ta) #f)]
       [(or (eq? key 'enter) (and ctrl? (char? key) (char=? key #\m)))
        (values (ta-newline! ta) #f)]
       [(and ctrl? (char? key) (char=? key #\k))
        (define lines (textarea-lines ta))
        (define row (textarea-row ta))
        (define col (textarea-col ta))
        (define line (vector-ref lines row))
        (vector-set! lines row (substring line 0 col))
        (values ta #f)]
       [(and ctrl? (char? key) (char=? key #\u))
        (define lines (textarea-lines ta))
        (define row (textarea-row ta))
        (define col (textarea-col ta))
        (define line (vector-ref lines row))
        (vector-set! lines row (substring line col))
        (set-textarea-col! ta 0)
        (values ta #f)]
       [(and (char? key) (char-graphic? key))
        (values (textarea-insert-char! ta key) #f)]
       [else (values ta #f)])]
    [else (values ta #f)]))

(define (textarea-view ta)
  (define lines (textarea-lines ta))
  (define height (textarea-height ta))
  (define show-ln (textarea-show-line-numbers? ta))
  (define prompt (textarea-prompt ta))
  (define row (textarea-row ta))
  (define col (textarea-col ta))
  (define focused? (textarea-focused? ta))
  (define placeholder (textarea-placeholder ta))

  (define ln-width
    (if show-ln
        (+ 2 (string-length (number->string (textarea-line-count ta))))
        0))

  ;; If empty and has placeholder
  (if (and (string=? (textarea-value ta) "")
           (not (string=? placeholder "")))
      (string-append prompt
                     (if show-ln (format "~a " (string-pad-left "1" ln-width)) "")
                     placeholder)
      (string-join
       (for/list ([i (in-range height)])
         (define line-text
           (if (< i (vector-length lines))
               (vector-ref lines i)
               ""))
         (define ln-part
           (cond
             [(not show-ln) ""]
             [(< i (vector-length lines))
              (format "~a " (string-pad-left (number->string (add1 i)) ln-width))]
             [else (format "~a " (make-string ln-width #\space))]))
         (define content
           (if (and focused? (= i row) (< i (vector-length lines)))
               ;; Line with cursor
               (let* ([before (substring line-text 0 (min col (string-length line-text)))]
                      [at-cursor (if (< col (string-length line-text))
                                     (format "\e[7m~a\e[27m"
                                             (string (string-ref line-text col)))
                                     "\e[7m \e[27m")]
                      [after (if (< col (string-length line-text))
                                 (substring line-text
                                            (min (add1 col) (string-length line-text)))
                                 "")])
                 (string-append before at-cursor after))
               line-text))
         (string-append prompt ln-part content))
       "\n")))

;;; Helpers

(define (char-graphic? ch)
  (and (char? ch) (>= (char->integer ch) 32) (not (= (char->integer ch) 127))))

(define (string-pad-left str width [pad-char #\space])
  (define len (string-length str))
  (if (>= len width)
      str
      (string-append (make-string (- width len) pad-char) str)))
