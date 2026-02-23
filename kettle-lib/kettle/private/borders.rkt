#lang racket/base

;; borders.rkt
;;
;; SPDX-License-Identifier: MIT
;;
;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;; Racket port
;;
;; Border definitions and rendering

(require racket/string
         racket/list
         "style.rkt")

(provide (struct-out border)
         make-border
         render-border
         render-shadow
         shadow-chars
         border-normal
         border-rounded
         border-thick
         border-double
         border-block
         border-hidden
         border-ascii
         border-markdown)

;;; Border structure
(struct border
  (top bottom left right
   top-left top-right bottom-left bottom-right
   middle-left middle-right middle middle-top middle-bottom)
  #:transparent)

;;; Predefined border styles

(define border-normal
  (border "─" "─" "│" "│"
          "┌" "┐" "└" "┘"
          "├" "┤" "┼" "┬" "┴"))

(define border-rounded
  (border "─" "─" "│" "│"
          "╭" "╮" "╰" "╯"
          "├" "┤" "┼" "┬" "┴"))

(define border-thick
  (border "━" "━" "┃" "┃"
          "┏" "┓" "┗" "┛"
          "┣" "┫" "╋" "┳" "┻"))

(define border-double
  (border "═" "═" "║" "║"
          "╔" "╗" "╚" "╝"
          "╠" "╣" "╬" "╦" "╩"))

(define border-block
  (border "█" "█" "█" "█"
          "█" "█" "█" "█"
          "█" "█" "█" "█" "█"))

(define border-hidden
  (border " " " " " " " "
          " " " " " " " "
          " " " " " " " " " "))

(define border-ascii
  (border "-" "-" "|" "|"
          "+" "+" "+" "+"
          "+" "+" "+" "+" "+"))

(define border-markdown
  (border "-" "-" "|" "|"
          "|" "|" "|" "|"
          "|" "|" "|" "|" "|"))

(define (make-border #:top [t "─"] #:bottom [b "─"]
                     #:left [l "│"] #:right [r "│"]
                     #:top-left [tl "┌"] #:top-right [tr "┐"]
                     #:bottom-left [bl "└"] #:bottom-right [br "┘"]
                     #:middle-left [ml "├"] #:middle-right [mr "┤"]
                     #:middle [m "┼"] #:middle-top [mt "┬"]
                     #:middle-bottom [mb "┴"])
  (border t b l r tl tr bl br ml mr m mt mb))

;;; Border rendering

(define (render-border text bdr
                       #:top [show-top #t] #:bottom [show-bottom #t]
                       #:left [show-left #t] #:right [show-right #t]
                       #:fg-color [fg-color #f] #:bg-color [bg-color #f]
                       #:title [title #f]
                       #:title-position [title-position 'center])
  "Render text with a border around it."
  (define lines (split-string-by-newline text))
  (define max-width (apply max (map visible-length lines)))
  (define result '())

  (define (color-border str)
    (if (or fg-color bg-color)
        (colored str #:fg fg-color #:bg bg-color)
        str))

  ;; Top border
  (when show-top
    (define top-line
      (if title
          ;; Build top border with title
          (let* ([title-vis-len (visible-length title)]
                 [available (- max-width 2)]
                 [truncated-title (if (> title-vis-len available)
                                      (substring title 0 (min (string-length title) available))
                                      title)]
                 [t-len (visible-length truncated-title)]
                 [border-char (string-ref (border-top bdr) 0)]
                 [left-pad (case title-position
                             [(left) 1]
                             [(right) (max 1 (- max-width t-len 1))]
                             [else (max 1 (quotient (- max-width t-len) 2))])]
                 [right-pad (max 1 (- max-width t-len left-pad))])
            (string-append
             (if show-left (border-top-left bdr) "")
             (make-string left-pad border-char)
             truncated-title
             (make-string right-pad border-char)
             (if show-right (border-top-right bdr) "")))
          ;; Normal top border
          (string-append
           (if show-left (border-top-left bdr) "")
           (make-string max-width (string-ref (border-top bdr) 0))
           (if show-right (border-top-right bdr) ""))))
    (set! result (append result (list (color-border top-line)))))

  ;; Content with left/right borders
  (for ([line (in-list lines)])
    (define vlen (visible-length line))
    (define padding (- max-width vlen))
    (define left-part (if show-left (color-border (border-left bdr)) ""))
    (define pad-part (color-border (make-string padding #\space)))
    (define right-part (if show-right (color-border (border-right bdr)) ""))
    (set! result
          (append result
                  (list (string-append left-part line pad-part right-part)))))

  ;; Bottom border
  (when show-bottom
    (define bottom-line
      (string-append
       (if show-left (border-bottom-left bdr) "")
       (make-string max-width (string-ref (border-bottom bdr) 0))
       (if show-right (border-bottom-right bdr) "")))
    (set! result (append result (list (color-border bottom-line)))))

  (string-join result "\n"))

;;; Shadow rendering

(define (shadow-chars style)
  "Return (cons right-char bottom-char) for the given shadow style."
  (case style
    [(solid) (cons "█" "█")]
    [(light) (cons "░" "░")]
    [(medium) (cons "▒" "▒")]
    [(heavy) (cons "▓" "▓")]
    [else (cons "█" "▀")])) ; dark default

(define (render-shadow text
                       #:width [shadow-width 2]
                       #:offset [offset 1]
                       #:color [color fg-bright-black]
                       #:style [shadow-style 'dark])
  "Add a drop shadow to a text block."
  (define chars (shadow-chars shadow-style))
  (define right-char (car chars))
  (define bottom-char (cdr chars))
  (define lines (split-string-by-newline text))
  (define num-lines (length lines))
  (define max-w (if (null? lines) 0 (apply max (map visible-length lines))))
  (define shadow-str
    (colored (make-string shadow-width (string-ref right-char 0)) #:fg color))
  (define pad-str (make-string shadow-width #\space))
  (define result '())

  ;; Content lines with right shadow
  (for ([line (in-list lines)]
        [i (in-naturals)])
    (if (>= i offset)
        (set! result (append result (list (string-append line shadow-str))))
        (set! result (append result (list (string-append line pad-str))))))

  ;; Bottom shadow
  (define bottom-width (+ max-w shadow-width (- offset)))
  (define bottom-str
    (colored (make-string (max 0 bottom-width) (string-ref bottom-char 0)) #:fg color))
  (define leading-spaces (make-string offset #\space))
  (set! result (append result (list (string-append leading-spaces bottom-str))))

  (string-join result "\n"))
