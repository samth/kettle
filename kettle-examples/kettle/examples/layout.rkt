#lang racket/base

;; layout.rkt -- Lipgloss-style layout demo (non-interactive, prints and exits).
;; Adapted from cl-tuition examples/layout.lisp
;; https://github.com/atgreen/cl-tuition/blob/master/examples/layout.lisp
;;
;; Key differences from cl-tuition:
;; - cl-tuition's layout.lisp is a non-interactive print-and-exit example using
;;   render-styled + join-horizontal/join-vertical for complex layouts. Kettle
;;   can do the same with its legacy layout API (same function names).
;; - This port demonstrates render-styled, render-border, join-horizontal,
;;   join-vertical, and color-rgb for truecolor gradient generation -- all
;;   available in Kettle's string-based layout module.
;; - Runs as a plain program that prints the layout and exits, not a TUI loop.

(require racket/format
         racket/string
         kettle/style
         kettle/border
         kettle/layout)

(define width 96)
(define column-width 30)

;; Color grid with 2D bilinear interpolation
(define (lerp a b frac) (exact-round (+ a (* frac (- b a)))))

(define (color-grid x-steps y-steps)
  ;; Corner colors: TL=#F25D94 TR=#EDFF82 BL=#643AFF BR=#14F9D5
  (define-values (r0 g0 b0) (values #xF2 #x5D #x94))  ;; top-left
  (define-values (r1 g1 b1) (values #xED #xFF #x82))  ;; top-right
  (define-values (r2 g2 b2) (values #x64 #x3A #xFF))  ;; bottom-left
  (define-values (r3 g3 b3) (values #x14 #xF9 #xD5))  ;; bottom-right
  (string-join
   (for/list ([y (in-range y-steps)])
     (define yf (if (> y-steps 1) (/ (exact->inexact y) (sub1 y-steps)) 0.0))
     (define lr (lerp r0 r2 yf)) (define lg (lerp g0 g2 yf)) (define lb (lerp b0 b2 yf))
     (define rr (lerp r1 r3 yf)) (define rg (lerp g1 g3 yf)) (define rb (lerp b1 b3 yf))
     (apply string-append
            (for/list ([x (in-range x-steps)])
              (define xf (if (> x-steps 1) (/ (exact->inexact x) (sub1 x-steps)) 0.0))
              (define cr (lerp lr rr xf))
              (define cg (lerp lg rg xf))
              (define cb (lerp lb rb xf))
              (colored "  " #:bg (color-rgb cr cg cb)))))
   "\n"))

;; Tab section
(define (build-tabs)
  (define highlight (parse-hex-color "#7D56F4"))
  (define active-border
    (make-border #:top "─" #:bottom " " #:left "│" #:right "│"
                 #:top-left "╭" #:top-right "╮" #:bottom-left "┘" #:bottom-right "└"))
  (define tab-border
    (make-border #:top "─" #:bottom "─" #:left "│" #:right "│"
                 #:top-left "╭" #:top-right "╮" #:bottom-left "┴" #:bottom-right "┴"))
  (define row
    (join-horizontal top
      (render-border " Kettle " active-border #:fg-color highlight)
      (render-border " Styles " tab-border #:fg-color highlight)
      (render-border " Layouts " tab-border #:fg-color highlight)
      (render-border " Colors " tab-border #:fg-color highlight)))
  (string-append row "\n\n"))

;; Title section
(define (build-title)
  (define colors '("#F25D94" "#E86BA0" "#DE7DAC" "#D48FB8" "#CAA1C4"))
  (define fg-col (parse-hex-color "#FFF7DB"))
  (define lines
    (for/list ([hex (in-list colors)]
               [i (in-naturals)])
      (render-styled (make-style #:foreground fg-col
                                 #:background (parse-hex-color hex)
                                 #:italic #t
                                 #:padding-left 1 #:padding-right 1
                                 #:margin-left (* i 2))
                     "Kettle")))
  (define title (string-join lines "\n"))
  (define subtle (parse-hex-color "#383838"))
  (define desc (render-styled (make-style #:foreground (parse-hex-color "#EEEEEE")
                                          #:margin-top 1)
                              "Style Definitions for Nice Terminal Layouts"))
  (string-append (join-horizontal top title desc) "\n\n"))

;; Lists section
(define (build-lists)
  (define subtle (parse-hex-color "#383838"))
  (define normal (parse-hex-color "#EEEEEE"))
  (define special (parse-hex-color "#73F59F"))
  (define done-col (parse-hex-color "#696969"))
  (define header-style (make-style #:foreground normal))
  (define item-style (make-style #:foreground normal #:padding-left 2))
  (define done-style (make-style #:foreground done-col #:strikethrough #t))
  (define check (colored "✓ " #:fg special))
  (define list1
    (join-vertical left
      (render-styled header-style "Citrus Fruits to Try")
      (string-append check (render-styled done-style "Grapefruit"))
      (string-append check (render-styled done-style "Yuzu"))
      (render-styled item-style "Citron")
      (render-styled item-style "Kumquat")
      (render-styled item-style "Pomelo")))
  (define list2
    (join-vertical left
      (render-styled header-style "Actual Lip Gloss Vendors")
      (render-styled item-style "Glossier")
      (render-styled item-style "Claire's Boutique")
      (string-append check (render-styled done-style "Nyx"))
      (render-styled item-style "Mac")
      (string-append check (render-styled done-style "Milk"))))
  (join-horizontal top list1 "   " list2))

;; Status bar
(define (build-status-bar)
  (define status-key
    (render-styled (make-style #:foreground (parse-hex-color "#FFFDF5")
                               #:background (parse-hex-color "#FF5F87")
                               #:padding-left 1 #:padding-right 1)
                   "STATUS"))
  (define encoding
    (render-styled (make-style #:foreground (parse-hex-color "#FFFDF5")
                               #:background (parse-hex-color "#A550DF")
                               #:padding-left 1 #:padding-right 1)
                   "UTF-8"))
  (define fish
    (render-styled (make-style #:foreground (parse-hex-color "#FFFDF5")
                               #:background (parse-hex-color "#6124DF")
                               #:padding-left 1 #:padding-right 1)
                   "🍥 Fish Cake"))
  (join-horizontal top status-key "  Ravishing  " encoding fish))

;; Main
(module+ main
  (define tabs (build-tabs))
  (define title (build-title))
  (define lists (build-lists))
  (define colors (color-grid 14 8))
  (define lists+colors (join-horizontal top lists "   " colors))
  (define status (build-status-bar))
  (displayln
   (string-append "\n" tabs title lists+colors "\n\n" status "\n")))
