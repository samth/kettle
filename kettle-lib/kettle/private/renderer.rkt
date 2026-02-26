#lang racket/base

;; renderer.rkt
;;
;; SPDX-License-Identifier: MIT
;;
;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;; Racket port
;;
;; Cell-buffer rendering engine.
;; Walks an image tree (or accepts a plain string), fills a cell grid
;; backed by tui-ubuf, and emits minimal ANSI output via dirty tracking.

(require racket/match
         racket/list
         racket/string
         (prefix-in ansi: ansi/ansi)
         (only-in tui/ubuf
                  make-ubuf ubuf? display-ubuf!
                  ubuf-putchar! ubuf-clear!
                  ubuf-display-linear?
                  ubuf-display-only-dirty?
                  ubuf-display-clear-dirty?
                  ubuf-display-position)
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
         make-cell-buffer
         cell-buffer->string
         paint!
         image->string)

;;; ============================================================
;;; Cell buffer — thin wrapper around tui-ubuf
;;; ============================================================

;; make-cell-buffer creates a ubuf of the given dimensions
(define (make-cell-buffer w h)
  (make-ubuf w h))

;;; ============================================================
;;; Style decomposition — convert Kettle style to ubuf attributes
;;; ============================================================

;; Write a single character with style into the ubuf
(define (ubuf-set-styled! buf col row ch sty)
  (ubuf-putchar! buf col row
                 #:char ch
                 #:fg (or (and sty (style-foreground sty)) 7)
                 #:bg (or (and sty (style-background sty)) 0)
                 #:bold (and sty (style-bold? sty) #t)
                 #:italic (and sty (style-italic? sty) #t)
                 #:underline (if (and sty (style-underline? sty)) 'single #f)
                 #:blink (if (and sty (style-blink? sty)) 'slow #f)))

;;; ============================================================
;;; Paint: walk the image tree and fill a cell buffer
;;; ============================================================

(define (paint! buf img x y [sty #f])
  (match img
    [(image:text w h str s) (paint-text! buf str x y (merge-style sty s))]
    [(image:blank w h) (void)]
    [(image:char w h ch s) (ubuf-set-styled! buf x y ch (merge-style sty s))]
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
           ;; ubuf-putchar! handles wide chars natively
           (ubuf-set-styled! buf col y ch effective-style)
           (set! col (+ col cw)))
         (set! i (add1 i))
         (loop)]))))

;; Parse an inline ANSI SGR code string into an integer color
(define (parse-sgr-color code fg?)
  (define parts (string-split code ";"))
  (define prefix (if fg? "38" "48"))
  (cond
    ;; Extended: 38;5;N or 48;5;N
    [(and (>= (length parts) 3)
          (string=? (first parts) prefix)
          (string=? (second parts) "5"))
     (string->number (third parts))]
    ;; Truecolor: 38;2;R;G;B or 48;2;R;G;B
    [(and (>= (length parts) 5)
          (string=? (first parts) prefix)
          (string=? (second parts) "2"))
     (define r (string->number (third parts)))
     (define g (string->number (fourth parts)))
     (define b (string->number (fifth parts)))
     (and r g b (color-rgb r g b))]
    ;; Basic foreground 30-37, bright 90-97
    [(and fg? (= (length parts) 1))
     (define n (string->number code))
     (cond
       [(and n (>= n 30) (<= n 37)) (- n 30)]
       [(and n (>= n 90) (<= n 97)) (+ (- n 90) 8)]
       [else #f])]
    ;; Basic background 40-47, bright 100-107
    [(and (not fg?) (= (length parts) 1))
     (define n (string->number code))
     (cond
       [(and n (>= n 40) (<= n 47)) (- n 40)]
       [(and n (>= n 100) (<= n 107)) (+ (- n 100) 8)]
       [else #f])]
    [else #f]))

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
      ;; Foreground colors 30-37, 90-97, or extended 38;...
      [(and (>= (string-length code) 2)
            (let ([n (string->number code)])
              (and n (or (and (>= n 30) (<= n 37)) (and (>= n 90) (<= n 97))))))
       (set! fg (parse-sgr-color code #t))]
      [(string-prefix? code "38")
       (define parsed (parse-sgr-color code #t))
       (when parsed (set! fg parsed))]
      ;; Background colors 40-47, 100-107, or extended 48;...
      [(and (>= (string-length code) 2)
            (let ([n (string->number code)])
              (and n (or (and (>= n 40) (<= n 47)) (and (>= n 100) (<= n 107))))))
       (set! bg (parse-sgr-color code #f))]
      [(string-prefix? code "48")
       (define parsed (parse-sgr-color code #f))
       (when parsed (set! bg parsed))]
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
;;; Emit: convert cell buffer to ANSI string (for testing)
;;; ============================================================

(define (cell-buffer->string buf)
  (define out (open-output-string))
  (display-ubuf! buf out #:linear #t #:only-dirty #f #:clear-dirty #f)
  (get-output-string out))

;;; ============================================================
;;; Renderer
;;; ============================================================

(struct renderer ([buffer #:mutable] [width #:mutable] [height #:mutable]
                  output-stream [last-string-output #:mutable])
  #:transparent)

(define (make-renderer [output-stream (current-output-port)])
  (renderer #f 0 0 output-stream ""))

;; Render an image to the terminal
(define (render-image! r img)
  (define w (image-w img))
  (define h (image-h img))
  ;; Reallocate buffer if size changed
  (define buf
    (if (and (renderer-buffer r)
             (= w (renderer-width r))
             (= h (renderer-height r)))
        (let ([b (renderer-buffer r)])
          (ubuf-clear! b)
          b)
        (let ([b (make-ubuf w h)])
          (set-renderer-buffer! r b)
          (set-renderer-width! r w)
          (set-renderer-height! r h)
          b)))
  (paint! buf img 0 0)
  (define stream (renderer-output-stream r))
  (move-cursor-home stream)
  (display-ubuf! buf stream #:linear #t #:only-dirty #f #:clear-dirty #f)
  (clear-to-end-of-screen stream)
  (flush-output stream))

;; Render a view (image or string) to the terminal
(define (render! r view)
  (cond
    [(image? view) (render-image! r view)]
    [(string? view)
     ;; Legacy string path
     (unless (string=? view (renderer-last-string-output r))
       (set-renderer-last-string-output! r view)
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
  (display (ansi:goto 1 1) stream))

(define (clear-to-end-of-screen [stream (current-output-port)])
  (display (ansi:clear-screen-from-cursor) stream))

(define (move-cursor row col [stream (current-output-port)])
  (display (ansi:goto row col) stream))
