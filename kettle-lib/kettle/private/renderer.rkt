#lang racket/base

;; renderer.rkt
;;
;; SPDX-License-Identifier: MIT
;;
;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;; Racket port
;;
;; Cell-buffer rendering engine.
;; Walks an image tree (or accepts a plain string), fills a cell grid,
;; diffs against the previous frame, and emits minimal ANSI output.

(require racket/match
         racket/string
         "style.rkt"
         "image.rkt")

(provide (struct-out renderer)
         make-renderer
         render!
         render-image!
         move-cursor-home
         clear-to-end-of-screen
         move-cursor

         ;; Cell buffer (for testing / advanced use)
         (struct-out cell)
         make-cell-buffer
         cell-buffer->string
         paint!
         image->string)

(define ESC "\e")

;;; ============================================================
;;; Cell buffer
;;; ============================================================

;; A cell holds a character and an optional style
(struct cell (ch style) #:transparent)

(define empty-cell (cell #\space #f))

;; A cell-buffer is a width x height vector-of-vectors
(struct cell-buffer (w h rows) #:transparent)

(define (make-cell-buffer w h)
  (cell-buffer w h (build-vector h (lambda (_) (make-vector w empty-cell)))))

(define (cell-buffer-ref buf col row)
  (if (and (>= col 0) (< col (cell-buffer-w buf)) (>= row 0) (< row (cell-buffer-h buf)))
      (vector-ref (vector-ref (cell-buffer-rows buf) row) col)
      empty-cell))

(define (cell-buffer-set! buf col row c)
  (when (and (>= col 0) (< col (cell-buffer-w buf)) (>= row 0) (< row (cell-buffer-h buf)))
    (vector-set! (vector-ref (cell-buffer-rows buf) row) col c)))

;;; ============================================================
;;; Paint: walk the image tree and fill a cell buffer
;;; ============================================================

(define (paint! buf img x y [sty #f])
  (match img
    [(image:text w h str s) (paint-text! buf str x y (merge-style sty s))]
    [(image:blank w h) (void)]
    [(image:char w h ch s) (cell-buffer-set! buf x y (cell ch (merge-style sty s)))]
    [(image:hcat w h align children)
     (define cx x)
     (for ([child (in-list children)])
       (define child-y (align-offset align y h (image-h child)))
       (paint! buf child cx child-y sty)
       (set! cx (+ cx (image-w child))))]
    [(image:vcat w h align children)
     (define cy y)
     (for ([child (in-list children)])
       (define child-x (align-offset-h align x w (image-w child)))
       (paint! buf child child-x cy sty)
       (set! cy (+ cy (image-h child))))]
    [(image:zcat w h children)
     ;; Paint back-to-front (last child is background)
     (for ([child (in-list (reverse children))])
       (paint! buf child x y sty))]
    ;; Paint inner offset, then the buffer naturally clips
    [(image:crop w h col row inner) (paint! buf inner (- x col) (- y row) sty)]
    [(image:pad w h left right top bottom inner) (paint! buf inner (+ x left) (+ y top) sty)]
    [(image:styled w h s inner) (paint! buf inner x y (merge-style sty s))]
    [(image:flex w h sw mw sh mh inner) (paint! buf inner x y sty)]
    [_ (void)]))

;; Paint a single-line string (possibly with ANSI codes) into the buffer
(define (paint-text! buf str x y sty)
  (define len (string-length str))
  (define col x)
  (define i 0)
  ;; Track inline ANSI style overrides
  (define inline-codes '())
  (let loop ()
    (when (< i len)
      (define ch (string-ref str i))
      (cond
        ;; ANSI escape sequence -- track the codes
        [(char=? ch #\u001B)
         (set! i (add1 i))
         (when (and (< i len) (char=? (string-ref str i) #\[))
           (set! i (add1 i))
           (define code-start i)
           (let inner ()
             (when (< i len)
               (define c (string-ref str i))
               (define code (char->integer c))
               (set! i (add1 i))
               (cond
                 [(and (>= code #x40) (<= code #x7E))
                  ;; End of escape - extract the code string
                  (define code-str (substring str code-start (sub1 i)))
                  (cond
                    ;; Reset
                    [(string=? code-str "0") (set! inline-codes '())]
                    ;; Otherwise accumulate
                    [else (set! inline-codes (append inline-codes (list code-str)))])]
                 [else (inner)]))))
         (loop)]
        [else
         (define cw (char-display-width ch))
         (when (> cw 0)
           (define effective-style
             (if (null? inline-codes)
                 sty
                 (make-inline-style sty inline-codes)))
           (cell-buffer-set! buf col y (cell ch effective-style))
           ;; For wide chars, fill the second cell with a placeholder
           (when (= cw 2)
             (cell-buffer-set! buf (add1 col) y (cell #\nul effective-style)))
           (set! col (+ col cw)))
         (set! i (add1 i))
         (loop)]))))

;; Build a style from inline ANSI codes on top of a base style
(define (make-inline-style base-sty codes)
  (define fg (and base-sty (style-foreground base-sty)))
  (define bg (and base-sty (style-background base-sty)))
  (define bold? (and base-sty (style-bold? base-sty)))
  (define italic? (and base-sty (style-italic? base-sty)))
  (define underline? (and base-sty (style-underline? base-sty)))
  (define reverse? (and base-sty (style-reverse? base-sty)))
  (define faint? (and base-sty (style-faint? base-sty)))
  (define strikethrough? (and base-sty (style-strikethrough? base-sty)))
  (define blink? (and base-sty (style-blink? base-sty)))
  (for ([code (in-list codes)])
    (cond
      [(string=? code "1") (set! bold? #t)]
      [(string=? code "2") (set! faint? #t)]
      [(string=? code "3") (set! italic? #t)]
      [(string=? code "4") (set! underline? #t)]
      [(string=? code "5") (set! blink? #t)]
      [(string=? code "7") (set! reverse? #t)]
      [(string=? code "9") (set! strikethrough? #t)]
      [(string=? code "22")
       (set! bold? #f)
       (set! faint? #f)]
      [(string=? code "23") (set! italic? #f)]
      [(string=? code "24") (set! underline? #f)]
      [(string=? code "27") (set! reverse? #f)]
      ;; Foreground colors 30-37, 90-97
      [(and (>= (string-length code) 2)
            (let ([n (string->number code)])
              (and n (or (and (>= n 30) (<= n 37)) (and (>= n 90) (<= n 97))))))
       (set! fg code)]
      ;; Background colors 40-47, 100-107
      [(and (>= (string-length code) 2)
            (let ([n (string->number code)])
              (and n (or (and (>= n 40) (<= n 47)) (and (>= n 100) (<= n 107))))))
       (set! bg code)]
      ;; Extended colors (38;5;N or 38;2;R;G;B)
      [(string-prefix? code "38") (set! fg code)]
      [(string-prefix? code "48") (set! bg code)]
      [else (void)]))
  (make-style #:foreground fg
              #:background bg
              #:bold bold?
              #:italic italic?
              #:underline underline?
              #:reverse reverse?
              #:faint faint?
              #:strikethrough strikethrough?
              #:blink blink?))

;; Alignment helpers
(define (align-offset align base total child-size)
  (case align
    [(top left) base]
    [(bottom right) (+ base (- total child-size))]
    [(middle center) (+ base (quotient (- total child-size) 2))]
    [else base]))

(define (align-offset-h align base total child-size)
  (case align
    [(left top) base]
    [(right bottom) (+ base (- total child-size))]
    [(center middle) (+ base (quotient (- total child-size) 2))]
    [else base]))

;; Merge two styles: outer provides defaults, inner overrides
(define (merge-style outer inner)
  (cond
    [(not outer) inner]
    [(not inner) outer]
    [else
     (make-style #:foreground (or (style-foreground inner) (style-foreground outer))
                 #:background (or (style-background inner) (style-background outer))
                 #:bold (or (style-bold? inner) (style-bold? outer))
                 #:italic (or (style-italic? inner) (style-italic? outer))
                 #:underline (or (style-underline? inner) (style-underline? outer))
                 #:blink (or (style-blink? inner) (style-blink? outer))
                 #:reverse (or (style-reverse? inner) (style-reverse? outer))
                 #:strikethrough (or (style-strikethrough? inner) (style-strikethrough? outer))
                 #:faint (or (style-faint? inner) (style-faint? outer)))]))

;;; ============================================================
;;; Emit: convert cell buffer to ANSI string
;;; ============================================================

(define (cell-buffer->string buf)
  (define out (open-output-string))
  (for ([row-idx (in-range (cell-buffer-h buf))])
    (define row-vec (vector-ref (cell-buffer-rows buf) row-idx))
    (define last-style #f)
    (for ([col-idx (in-range (cell-buffer-w buf))])
      (define c (vector-ref row-vec col-idx))
      (define ch (cell-ch c))
      (define sty (cell-style c))
      ;; Skip wide-char placeholders
      (unless (char=? ch #\nul)
        ;; Style transition
        (unless (equal? sty last-style)
          (when last-style
            (write-string (ansi-reset) out))
          (when sty
            (write-string (style->ansi-open sty) out))
          (set! last-style sty))
        (write-char ch out)))
    ;; Reset at end of line
    (when last-style
      (write-string (ansi-reset) out)
      (set! last-style #f))
    ;; Clear any stale characters to the right from a previous wider frame
    (write-string (format "~a[K" ESC) out)
    ;; CR+LF between rows: in raw mode OPOST is off, so a bare
    ;; newline only does LF (cursor moves down but stays at the
    ;; same column).  We need CR to return to column 1.
    (when (< row-idx (sub1 (cell-buffer-h buf)))
      (write-string "\r\n" out)))
  (get-output-string out))

;; Convert a style struct to an ANSI opening escape sequence
(define (style->ansi-open sty)
  (define codes '())
  (when (style-bold? sty)
    (set! codes (cons "1" codes)))
  (when (style-faint? sty)
    (set! codes (cons "2" codes)))
  (when (style-italic? sty)
    (set! codes (cons "3" codes)))
  (when (style-underline? sty)
    (set! codes (cons "4" codes)))
  (when (style-blink? sty)
    (set! codes (cons "5" codes)))
  (when (style-reverse? sty)
    (set! codes (cons "7" codes)))
  (when (style-strikethrough? sty)
    (set! codes (cons "9" codes)))
  (when (style-foreground sty)
    (set! codes (cons (style-foreground sty) codes)))
  (when (style-background sty)
    (set! codes (cons (style-background sty) codes)))
  (if (null? codes)
      ""
      (format "~a[~am" ESC (string-join (reverse codes) ";"))))

;;; ============================================================
;;; Renderer
;;; ============================================================

(struct renderer ([last-output #:mutable] [last-buffer #:mutable] output-stream) #:transparent)

(define (make-renderer [output-stream (current-output-port)])
  (renderer "" #f output-stream))

;; Render an image to the terminal
(define (render-image! r img)
  (define w (image-w img))
  (define h (image-h img))
  (define buf (make-cell-buffer w h))
  (paint! buf img 0 0)
  (define new-output (cell-buffer->string buf))
  (unless (string=? new-output (renderer-last-output r))
    (set-renderer-last-output! r new-output)
    (set-renderer-last-buffer! r buf)
    (define stream (renderer-output-stream r))
    (move-cursor-home stream)
    (display new-output stream)
    (clear-to-end-of-screen stream)
    (flush-output stream)))

;; Render a view (image or string) to the terminal
(define (render! r view)
  (cond
    [(image? view) (render-image! r view)]
    [(string? view)
     ;; Legacy string path
     (unless (string=? view (renderer-last-output r))
       (set-renderer-last-output! r view)
       (define stream (renderer-output-stream r))
       (move-cursor-home stream)
       (display view stream)
       (clear-to-end-of-screen stream)
       (flush-output stream))]
    [else (void)]))

;; Convert an image directly to a string (for testing or non-terminal use)
(define (image->string img)
  (define w (image-w img))
  (define h (image-h img))
  (if (or (zero? w) (zero? h))
      ""
      (let ([buf (make-cell-buffer w h)])
        (paint! buf img 0 0)
        (cell-buffer->string buf))))

;;; ============================================================
;;; ANSI cursor control
;;; ============================================================

(define (move-cursor-home [stream (current-output-port)])
  (display (format "~a[H" ESC) stream))

(define (clear-to-end-of-screen [stream (current-output-port)])
  (display (format "~a[J" ESC) stream))

(define (move-cursor row col [stream (current-output-port)])
  (display (format "~a[~a;~aH" ESC row col) stream))
