#lang racket/base

;; image.rkt
;;
;; SPDX-License-Identifier: MIT
;;
;; Algebraic image type for structured terminal UI rendering.
;; Images are inspectable, composable trees of styled text.

(require racket/match
         racket/list
         racket/string
         "style.rkt")

;; Base type
(provide image
         image?
         image-w
         image-h

         ;; Primitives
         image:text
         image:text?
         image:text-str
         image:text-style
         image:blank
         image:blank?
         image:char
         image:char?
         image:char-ch
         image:char-style

         ;; Composition
         image:hcat
         image:hcat?
         image:hcat-align
         image:hcat-children
         image:vcat
         image:vcat?
         image:vcat-align
         image:vcat-children
         image:zcat
         image:zcat?
         image:zcat-children

         ;; Spatial transforms
         image:crop
         image:crop?
         image:crop-col
         image:crop-row
         image:crop-inner
         image:pad
         image:pad?
         image:pad-left
         image:pad-right
         image:pad-top
         image:pad-bottom
         image:pad-inner

         ;; Styling
         image:styled
         image:styled?
         image:styled-style
         image:styled-inner

         ;; Stretchable layout
         image:flex
         image:flex?
         image:flex-sw
         image:flex-mw
         image:flex-sh
         image:flex-mh
         image:flex-inner

         ;; Smart constructors
         text
         blank
         char-image
         hcat
         vcat
         zcat
         crop
         pad
         styled
         flex

         ;; Convenience
         hcat*
         vcat*
         newline-image
         empty-image
         string->image

         ;; Image predicates and utilities
         image-empty?

         ;; Auto-coercion
         ensure-image

         ;; Convenience styling
         bold
         italic
         underline)

;;; ============================================================
;;; Core image struct hierarchy
;;; ============================================================

;; Abstract base -- all images carry width and height
(struct image (w h) #:transparent)

;; Primitives
(struct image:text image (str style) #:transparent)
(struct image:blank image () #:transparent)
(struct image:char image (ch style) #:transparent)

;; Composition
(struct image:hcat image (align children) #:transparent)
(struct image:vcat image (align children) #:transparent)
(struct image:zcat image (children) #:transparent)

;; Spatial transforms
(struct image:crop image (col row inner) #:transparent)
(struct image:pad image (left right top bottom inner) #:transparent)

;; Styling
(struct image:styled image (style inner) #:transparent)

;; Stretchable layout (Nottui-inspired)
;; sw = stretch width priority (0 = fixed), mw = max width (or #f for unbounded)
;; sh = stretch height priority (0 = fixed), mh = max height (or #f for unbounded)
(struct image:flex image (sw mw sh mh inner) #:transparent)

;;; ============================================================
;;; Auto-coercion
;;; ============================================================

;; Coerce a value to an image: images pass through, strings become text.
(define (ensure-image x)
  (cond
    [(image? x) x]
    [(string? x) (text x)]
    [else (raise-argument-error 'ensure-image "(or/c image? string?)" x)]))

;;; ============================================================
;;; Smart constructors
;;; ============================================================

;; Text: a single line of optionally-styled text.
;; Multi-line strings are split into vcat of text lines.
(define (text s [sty #f])
  (cond
    [(string=? s "") (image:blank 0 0)]
    [(string-contains? s "\n")
     (define lines (string-split s "\n" #:trim? #f))
     (apply vcat* 'left (map (lambda (l) (text l sty)) lines))]
    [else (image:text (visible-length s) 1 s sty)]))

;; Blank: empty rectangle of given dimensions
(define (blank [w 0] [h 0])
  (image:blank w h))

;; Char: single character, optionally styled
(define (char-image ch [sty #f])
  (image:char (char-display-width ch) 1 ch sty))

;; Horizontal concatenation
(define (hcat align . images)
  (define children
    (filter (lambda (i) (not (and (image:blank? i) (zero? (image-w i)) (zero? (image-h i)))))
            (map ensure-image images)))
  (cond
    [(null? children) (image:blank 0 0)]
    [(null? (cdr children)) (car children)]
    [else
     (image:hcat (apply + (map image-w children))
                 (apply max 1 (map image-h children))
                 align
                 children)]))

;; Vertical concatenation
(define (vcat align . images)
  (define children
    (filter (lambda (i) (not (and (image:blank? i) (zero? (image-w i)) (zero? (image-h i)))))
            (map ensure-image images)))
  (cond
    [(null? children) (image:blank 0 0)]
    [(null? (cdr children)) (car children)]
    [else
     (image:vcat (apply max 1 (map image-w children))
                 (apply + (map image-h children))
                 align
                 children)]))

;; Overlay (front-to-back z-ordering)
(define (zcat . images)
  (define children
    (filter (lambda (i) (not (and (image:blank? i) (zero? (image-w i)) (zero? (image-h i)))))
            (map ensure-image images)))
  (cond
    [(null? children) (image:blank 0 0)]
    [(null? (cdr children)) (car children)]
    [else
     (image:zcat (apply max 1 (map image-w children))
                 (apply max 1 (map image-h children))
                 children)]))

;; Crop: extract a w x h region starting at (col, row)
(define (crop w h col row inner)
  (image:crop w h col row inner))

;; Pad: add space around an image
(define (pad inner #:left [l 0] #:right [r 0] #:top [t 0] #:bottom [b 0])
  (define img (ensure-image inner))
  (if (and (zero? l) (zero? r) (zero? t) (zero? b))
      img
      (image:pad (+ l r (image-w img)) (+ t b (image-h img)) l r t b img)))

;; Styled: apply a style to an image subtree
(define (styled sty inner)
  (define img (ensure-image inner))
  (if sty
      (image:styled (image-w img) (image-h img) sty img)
      img))

;; Flex: mark an image as stretchable in layout
;; sw/sh = stretch priority (0 = don't stretch), mw/mh = max size (#f = unbounded)
(define (flex inner #:sw [sw 1] #:mw [mw #f] #:sh [sh 0] #:mh [mh #f])
  (image:flex (image-w inner) (image-h inner) sw mw sh mh inner))

;;; ============================================================
;;; Convenience constructors
;;; ============================================================

;; hcat*/vcat* accept a list instead of rest args
(define (hcat* align . images-or-list)
  (define imgs
    (if (and (= (length images-or-list) 1) (list? (car images-or-list)))
        (car images-or-list)
        images-or-list))
  (apply hcat align imgs))

(define (vcat* align . images-or-list)
  (define imgs
    (if (and (= (length images-or-list) 1) (list? (car images-or-list)))
        (car images-or-list)
        images-or-list))
  (apply vcat align imgs))

;; A single newline (1-tall blank)
(define newline-image (image:blank 0 1))

;; Zero-size empty image
(define empty-image (image:blank 0 0))

;; Convert a multi-line string (possibly with ANSI) to an image
(define (string->image s [sty #f])
  (text s sty))

;; Check if an image has zero dimensions
(define (image-empty? img)
  (and (zero? (image-w img)) (zero? (image-h img))))

;;; ============================================================
;;; Convenience styling (image-level)
;;; ============================================================

(define (bold inner)
  (styled (make-style #:bold #t) (ensure-image inner)))

(define (italic inner)
  (styled (make-style #:italic #t) (ensure-image inner)))

(define (underline inner)
  (styled (make-style #:underline #t) (ensure-image inner)))
