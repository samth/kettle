#lang racket/base

;; style.rkt
;;
;; SPDX-License-Identifier: MIT
;;
;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;; Racket port
;;
;; Style system - terminal styling and formatting inspired by Lipgloss

(require racket/string
         racket/list
         racket/match
         racket/format
         (prefix-in ansi: ansi/ansi)
         tui/ubuf/color
         (only-in tui/ubuf/eaw-categories char-east-asian-width-property))

(provide ;; ANSI utilities
         ansi-color
         ansi-reset
         color->sgr-code

         ;; Extended colors
         color-256
         color-rgb
         parse-hex-color

         ;; Adaptive colors
         (struct-out adaptive-color)
         (struct-out complete-color)
         (struct-out complete-adaptive-color)
         detect-dark-background
         detect-color-support
         resolve-color
         resolve-adaptive-color

         ;; Foreground colors
         fg-black fg-red fg-green fg-yellow
         fg-blue fg-magenta fg-cyan fg-white
         fg-bright-black fg-bright-red fg-bright-green fg-bright-yellow
         fg-bright-blue fg-bright-magenta fg-bright-cyan fg-bright-white

         ;; Background colors
         bg-black bg-red bg-green bg-yellow
         bg-blue bg-magenta bg-cyan bg-white
         bg-bright-black bg-bright-red bg-bright-green bg-bright-yellow
         bg-bright-blue bg-bright-magenta bg-bright-cyan bg-bright-white

         ;; Style
         (struct-out style)
         make-style
         render-styled

         ;; Convenience
         colored

         ;; Reflow
         wrap-text
         truncate-text
         ellipsize
         indent-lines

         ;; Measurement
         text-width
         text-height
         text-size
         visible-length
         char-display-width
         split-string-by-newline

         ;; Bidi
         bidi-isolate
         bidi-isolate-ltr)

;;; ANSI color codes
(define (ansi-color code)
  (ansi:select-graphic-rendition code))

(define (ansi-reset)
  (ansi:select-graphic-rendition))

;;; Extended color support
;;
;; Colors are integers following the tui-ubuf model:
;;   0-7:    basic ANSI colors
;;   8-15:   bright ANSI colors
;;   16-255: 256-color palette
;;   #x1RRGGBB: truecolor (packed via make-ubuf-truecolor)
;;   #f:     no color / default

(define (color-256 n)
  n)

(define (color-rgb r g b)
  (make-ubuf-truecolor r g b))

(define (parse-hex-color hex)
  (define clean (string-trim hex "#"))
  (cond
    [(= (string-length clean) 6)
     (define r (string->number (substring clean 0 2) 16))
     (define g (string->number (substring clean 2 4) 16))
     (define b (string->number (substring clean 4 6) 16))
     (color-rgb r g b)]
    [(= (string-length clean) 3)
     (define r (string->number (string (string-ref clean 0)) 16))
     (define g (string->number (string (string-ref clean 1)) 16))
     (define b (string->number (string (string-ref clean 2)) 16))
     (color-rgb (* r 17) (* g 17) (* b 17))]
    [else (error 'parse-hex-color "Invalid hex color: ~a" hex)]))

;;; Convert an integer color to an ANSI SGR code string
(define (color->sgr-code color fg?)
  (cond
    [(not color) #f]
    [(ubuf-truecolor? color)
     (define-values (r g b) (split-ubuf-truecolor color))
     (format "~a;2;~a;~a;~a" (if fg? "38" "48") r g b)]
    [(<= 0 color 7)
     (number->string (+ color (if fg? 30 40)))]
    [(<= 8 color 15)
     (number->string (+ (- color 8) (if fg? 90 100)))]
    [(<= 0 color 255)
     (format "~a;5;~a" (if fg? "38" "48") color)]
    [else #f]))

;;; Adaptive colors
(struct adaptive-color (light dark) #:transparent)
(struct complete-color (truecolor ansi256 ansi) #:transparent)
(struct complete-adaptive-color (light dark) #:transparent)

(define (detect-dark-background)
  (let/ec return
    (define colorfgbg (getenv "COLORFGBG"))
    (when colorfgbg
      (define parts (string-split colorfgbg ";"))
      (when (>= (length parts) 2)
        (define bg (string->number (last parts)))
        (when bg
          (return (< bg 8)))))
    (when (getenv "GNOME_TERMINAL_SCREEN")
      (return #t))
    (define term-program (getenv "TERM_PROGRAM"))
    (when term-program
      (cond
        [(string-contains? term-program "iTerm") (return #t)]
        [(string-contains? term-program "Apple_Terminal") (return #f)]
        [(string-contains? term-program "vscode") (return #t)]))
    #t))

(define (detect-color-support)
  (cond
    [(or (equal? (getenv "COLORTERM") "truecolor")
         (equal? (getenv "COLORTERM") "24bit"))
     'truecolor]
    [(let ([term (getenv "TERM")])
       (and term (or (string-contains? term "256color")
                     (string-contains? term "256colour"))))
     '256color]
    [(let ([term (getenv "TERM")])
       (and term (or (string-contains? term "color")
                     (string-contains? term "ansi")
                     (string-contains? term "xterm"))))
     '16color]
    [else 'monochrome]))

(define (resolve-color color)
  (cond
    [(complete-color? color)
     (define profile (detect-color-support))
     (case profile
       [(truecolor)
        (or (and (complete-color-truecolor color)
                 (parse-hex-color (complete-color-truecolor color)))
            (complete-color-ansi256 color)
            (complete-color-ansi color))]
       [(256color)
        (or (complete-color-ansi256 color)
            (complete-color-ansi color))]
       [else (complete-color-ansi color)])]
    [else color]))

(define (resolve-adaptive-color color)
  (cond
    [(complete-adaptive-color? color)
     (define selected
       (if (detect-dark-background)
           (complete-adaptive-color-dark color)
           (complete-adaptive-color-light color)))
     (resolve-color selected)]
    [(adaptive-color? color)
     (if (detect-dark-background)
         (adaptive-color-dark color)
         (adaptive-color-light color))]
    [else (resolve-color color)]))

;;; Foreground colors (integers — fg/bg distinction is context-dependent)
(define fg-black 0)
(define fg-red 1)
(define fg-green 2)
(define fg-yellow 3)
(define fg-blue 4)
(define fg-magenta 5)
(define fg-cyan 6)
(define fg-white 7)
(define fg-bright-black 8)
(define fg-bright-red 9)
(define fg-bright-green 10)
(define fg-bright-yellow 11)
(define fg-bright-blue 12)
(define fg-bright-magenta 13)
(define fg-bright-cyan 14)
(define fg-bright-white 15)

;;; Background colors (same integer values — fg/bg applied at render time)
(define bg-black 0)
(define bg-red 1)
(define bg-green 2)
(define bg-yellow 3)
(define bg-blue 4)
(define bg-magenta 5)
(define bg-cyan 6)
(define bg-white 7)
(define bg-bright-black 8)
(define bg-bright-red 9)
(define bg-bright-green 10)
(define bg-bright-yellow 11)
(define bg-bright-blue 12)
(define bg-bright-magenta 13)
(define bg-bright-cyan 14)
(define bg-bright-white 15)

;;; Text attributes (SGR parameter numbers)
(define attr-bold 1)
(define attr-dim 2)
(define attr-italic 3)
(define attr-underline 4)
(define attr-blink 5)
(define attr-reverse 7)
(define attr-hidden 8)
(define attr-strikethrough 9)

;;; Style structure
(struct style
  (foreground background
   bold? italic? underline? blink? reverse? strikethrough? faint?
   padding-left padding-right padding-top padding-bottom
   margin-left margin-right margin-top margin-bottom
   width height max-width max-height
   inline? align)
  #:transparent)

(define (make-style #:foreground [fg #f] #:background [bg #f]
                    #:bold [bold? #f] #:italic [italic? #f]
                    #:underline [underline? #f] #:blink [blink? #f]
                    #:reverse [reverse? #f] #:strikethrough [strikethrough? #f]
                    #:faint [faint? #f]
                    #:padding [padding 0]
                    #:padding-left [pl #f] #:padding-right [pr #f]
                    #:padding-top [pt #f] #:padding-bottom [pb #f]
                    #:margin [margin 0]
                    #:margin-left [ml #f] #:margin-right [mr #f]
                    #:margin-top [mt #f] #:margin-bottom [mb #f]
                    #:width [width #f] #:height [height #f]
                    #:max-width [max-width #f] #:max-height [max-height #f]
                    #:inline [inline? #f] #:align [align 'left])
  (style fg bg bold? italic? underline? blink? reverse? strikethrough? faint?
         (or pl padding) (or pr padding) (or pt padding) (or pb padding)
         (or ml margin) (or mr margin) (or mt margin) (or mb margin)
         width height max-width max-height inline? align))

;;; String utilities

(define (split-string-by-newline str)
  (define input (or str ""))
  (define lines (string-split input "\n" #:trim? #f))
  ;; Strip trailing \r from each line
  (map (lambda (line)
         (if (and (> (string-length line) 0)
                  (char=? (string-ref line (sub1 (string-length line))) #\return))
             (substring line 0 (sub1 (string-length line)))
             line))
       lines))

;;; Character width utilities
;; Uses tui-ubuf's auto-generated East Asian Width tables for comprehensive
;; Unicode coverage instead of hand-maintained ranges.

(define (char-display-width ch)
  (define code (char->integer ch))
  (cond
    [(< code 32) 0]
    [(= code #x7F) 0]
    [else
     (case (char-east-asian-width-property ch)
       [(F W) 2]
       [else 1])]))

(define (visible-length str)
  "Calculate visible display width of STR, excluding ANSI escapes."
  (define result 0)
  (define i 0)
  (define len (string-length str))
  (let loop ()
    (when (< i len)
      (define ch (string-ref str i))
      (cond
        [(char=? ch #\u001B)
         (set! i (add1 i))
         (when (and (< i len) (char=? (string-ref str i) #\[))
           (set! i (add1 i))
           (let inner ()
             (when (< i len)
               (define code (char->integer (string-ref str i)))
               (set! i (add1 i))
               (unless (and (>= code #x40) (<= code #x7E))
                 (inner)))))]
        [else
         (set! result (+ result (char-display-width ch)))
         (set! i (add1 i))])
      (loop)))
  result)

;;; Bidi helpers
(define (bidi-isolate text)
  (format "~a~a~a" (string (integer->char #x2068)) text (string (integer->char #x2069))))

(define (bidi-isolate-ltr text)
  (format "~a~a~a~a"
          (string (integer->char #x2066))
          text
          (string (integer->char #x200E))
          (string (integer->char #x2069))))

;;; Render styled text

(define (render-styled s text)
  (define result text)

  ;; Inline mode
  (when (style-inline? s)
    (set! result (first (split-string-by-newline result))))

  ;; Pre-wrap
  (when (and (not (style-inline? s)) (style-width s))
    (define wrap-at (- (style-width s) (style-padding-left s) (style-padding-right s)))
    (when (> wrap-at 0)
      (set! result (wrap-text result wrap-at #:break-words #f #:normalize-spaces #f))))

  ;; Apply padding
  (when (and (not (style-inline? s))
             (or (> (style-padding-left s) 0)
                 (> (style-padding-right s) 0)
                 (> (style-padding-top s) 0)
                 (> (style-padding-bottom s) 0)))
    (set! result (apply-padding result s)))

  ;; Apply width and alignment
  (when (style-width s)
    (set! result (apply-width-align result s)))

  ;; Apply min height
  (when (style-height s)
    (set! result (apply-min-height result (style-height s))))

  ;; Defensive clamp
  (when (style-width s)
    (set! result (truncate-to-width result (style-width s))))

  ;; Underline on text only
  (when (style-underline? s)
    (set! result (apply-underline-to-text-only result)))

  ;; Collect ANSI codes
  (define codes '())
  (when (style-bold? s) (set! codes (append codes (list attr-bold))))
  (when (style-faint? s) (set! codes (append codes (list attr-dim))))
  (when (style-italic? s) (set! codes (append codes (list attr-italic))))
  (when (style-blink? s) (set! codes (append codes (list attr-blink))))
  (when (style-reverse? s) (set! codes (append codes (list attr-reverse))))
  (when (style-strikethrough? s) (set! codes (append codes (list attr-strikethrough))))
  (when (style-foreground s)
    (define resolved-fg (resolve-adaptive-color (style-foreground s)))
    (define fg-code (color->sgr-code resolved-fg #t))
    (when fg-code (set! codes (append codes (list fg-code)))))
  (when (style-background s)
    (define resolved-bg (resolve-adaptive-color (style-background s)))
    (define bg-code (color->sgr-code resolved-bg #f))
    (when bg-code (set! codes (append codes (list bg-code)))))

  (set! result
        (if (not (null? codes))
            (let* ([seq (apply ansi:select-graphic-rendition codes)]
                   [rst (ansi-reset)]
                   [rst+seq (string-append rst seq)]
                   [lines (split-string-by-newline result)])
              (string-join
               (map (lambda (line)
                      ;; Re-apply outer style after any inner reset
                      (define reapplied
                        (let loop ([pos 0] [out ""])
                          (define idx (string-search rst line pos))
                          (if idx
                              (loop (+ idx (string-length rst))
                                    (string-append out (substring line pos idx) rst+seq))
                              (string-append out (substring line pos)))))
                      (format "~a~a~a" seq reapplied rst))
                    lines)
               "\n"))
            result))

  ;; Margins
  (when (and (not (style-inline? s))
             (or (> (style-margin-left s) 0)
                 (> (style-margin-right s) 0)
                 (> (style-margin-top s) 0)
                 (> (style-margin-bottom s) 0)))
    (set! result (apply-margin result s)))

  ;; Max constraints
  (when (style-max-width s)
    (set! result (truncate-to-width result (style-max-width s))))
  (when (style-max-height s)
    (set! result (truncate-to-height result (style-max-height s))))

  result)

;; Helper: find substring index
(define (string-search needle haystack start)
  (define nlen (string-length needle))
  (define hlen (string-length haystack))
  (let loop ([i start])
    (cond
      [(> (+ i nlen) hlen) #f]
      [(string=? (substring haystack i (+ i nlen)) needle) i]
      [else (loop (add1 i))])))

(define (apply-underline-to-text-only text)
  (define on (ansi-color attr-underline))
  (define off (ansi-color 24))
  (string-join
   (map (lambda (line)
          (define len (string-length line))
          (define start
            (for/first ([i (in-range len)]
                        #:when (not (char=? (string-ref line i) #\space)))
              i))
          (define last-ns
            (for/last ([i (in-range len)]
                       #:when (not (char=? (string-ref line i) #\space)))
              i))
          (define end (and last-ns (add1 last-ns)))
          (if (and start end (> end start))
              (string-append (substring line 0 start)
                             on
                             (substring line start end)
                             off
                             (substring line end))
              line))
        (split-string-by-newline text))
   "\n"))

(define (apply-padding text s)
  (define lines (split-string-by-newline text))
  (define pad-left (make-string (style-padding-left s) #\space))
  (define pad-right (make-string (style-padding-right s) #\space))
  (define padded (map (lambda (line) (string-append pad-left line pad-right)) lines))
  (define top-lines (make-list (style-padding-top s) ""))
  (define bottom-lines (make-list (style-padding-bottom s) ""))
  (string-join (append top-lines padded bottom-lines) "\n"))

(define (apply-width-align text s)
  (define lines (split-string-by-newline text))
  (define w (style-width s))
  (define al (style-align s))
  (string-join
   (map (lambda (line)
          (define vlen (visible-length line))
          (cond
            [(or (not w) (<= w 0)) ""]
            [(> vlen w)
             (truncate-text line w #:ellipsis "")]
            [else
             (define padding (max 0 (- w vlen)))
             (define aligned
               (case al
                 [(left) (string-append line (make-string padding #\space))]
                 [(right) (string-append (make-string padding #\space) line)]
                 [(center)
                  (define lp (quotient padding 2))
                  (define rp (- padding lp))
                  (string-append (make-string lp #\space) line (make-string rp #\space))]
                 [else line]))
             (if (> (visible-length aligned) w)
                 (truncate-text aligned w #:ellipsis "")
                 aligned)]))
        lines)
   "\n"))

(define (apply-margin text s)
  (define lines (split-string-by-newline text))
  (define ml (make-string (style-margin-left s) #\space))
  (define mr (make-string (style-margin-right s) #\space))
  (define margined (map (lambda (line) (string-append ml line mr)) lines))
  (define top-lines (make-list (style-margin-top s) ""))
  (define bottom-lines (make-list (style-margin-bottom s) ""))
  (string-join (append top-lines margined bottom-lines) "\n"))

(define (apply-min-height text min-h)
  (define lines (split-string-by-newline text))
  (define current-h (length lines))
  (if (>= current-h min-h)
      text
      (let* ([maxw (if (null? lines) 0 (apply max (map visible-length lines)))]
             [needed (- min-h current-h)]
             [blank (make-string maxw #\space)])
        (string-join (append lines (make-list needed blank)) "\n"))))

(define (truncate-to-width text max-w)
  (string-join
   (map (lambda (line) (truncate-text line max-w #:ellipsis ""))
        (split-string-by-newline text))
   "\n"))

(define (truncate-to-height text max-h)
  (define lines (split-string-by-newline text))
  (if (<= (length lines) max-h)
      text
      (string-join (take lines max-h) "\n")))

;;; Convenience functions

(define (colored text #:fg [fg #f] #:bg [bg #f])
  (render-styled (make-style #:foreground fg #:background bg) text))

;;; Reflow utilities

(define (indent-lines text n)
  (define pad (make-string (max 0 n) #\space))
  (string-join
   (map (lambda (line) (string-append pad line))
        (split-string-by-newline text))
   "\n"))

(define (truncate-text text width #:ellipsis [ellipsis "\u2026"])
  (define maxw (max 0 width))
  (define ellw (visible-length ellipsis))
  (define budget (max 0 (- maxw ellw)))
  (define w 0)
  (define truncated? #f)
  (define out (open-output-string))

  (define i 0)
  (define len (string-length text))
  (let loop ()
    (when (and (< i len) (not truncated?))
      (define ch (string-ref text i))
      (cond
        ;; ANSI escape
        [(char=? ch #\u001B)
         (write-char ch out)
         (set! i (add1 i))
         (when (and (< i len) (char=? (string-ref text i) #\[))
           (write-char (string-ref text i) out)
           (set! i (add1 i))
           (let inner ()
             (when (< i len)
               (define c (string-ref text i))
               (write-char c out)
               (set! i (add1 i))
               (define code (char->integer c))
               (unless (and (>= code #x40) (<= code #x7E))
                 (inner)))))
         (loop)]
        ;; Newline
        [(char=? ch #\newline)
         (write-char ch out)
         (set! i (add1 i))
         (loop)]
        ;; Regular char
        [else
         (define cw (char-display-width ch))
         (if (<= (+ w cw) budget)
             (begin
               (set! w (+ w cw))
               (write-char ch out)
               (set! i (add1 i))
               (loop))
             (set! truncated? #t))])))

  (define s (get-output-string out))
  (if truncated? (string-append s ellipsis) s))

(define (ellipsize text width)
  (truncate-text text width #:ellipsis "\u2026"))

(define (wrap-text text width
                   #:break-words [break-words #f]
                   #:normalize-spaces [normalize-spaces #t]
                   #:indent [indent* 0]
                   #:continuation-indent [cont-indent 0])
  (define maxw (max 1 width))
  (define line "")
  (define out '())
  (define w 0)
  (define current-indent (max 0 indent*))
  (define need-space #f)

  (define (emit-line!)
    (set! out (append out (list line)))
    (set! line "")
    (set! w 0)
    (set! need-space #f)
    (set! current-indent (max 0 cont-indent)))

  (define (add-text! txt)
    (set! line (string-append line txt)))

  (define (ensure-indent!)
    (when (> current-indent 0)
      (add-text! (make-string current-indent #\space))
      (set! w (+ w current-indent))
      (set! current-indent 0)))

  ;; Tokenize
  (define tokens (tokenize text))

  (for ([tok (in-list tokens)])
    (match-define (list type txt) tok)
    (case type
      [(ansi) (add-text! txt)]
      [(newline) (emit-line!)]
      [(space)
       (when (and (not need-space) (not (string=? line "")))
         (set! need-space #t))]
      [(word)
       (define ww (visible-length txt))
       (define sp (if need-space 1 0))
       (ensure-indent!)
       (cond
         [(<= (+ sp ww) (- maxw w))
          (when (and need-space (> sp 0))
            (add-text! " ")
            (set! w (add1 w)))
          (add-text! txt)
          (set! w (+ w ww))
          (set! need-space #f)]
         [(and break-words (> ww maxw))
          ;; Break long word
          (let wloop ([remaining txt] [first? #t])
            (when (> (visible-length remaining) 0)
              (define take-n (min (visible-length remaining) (- maxw (if first? w 0))))
              (when (and need-space (<= 1 (- maxw w)))
                (add-text! " ")
                (set! w (add1 w))
                (set! need-space #f))
              (define chars-to-take (min take-n (string-length remaining)))
              (add-text! (substring remaining 0 chars-to-take))
              (define rest-str (substring remaining chars-to-take))
              (cond
                [(= (string-length rest-str) 0)
                 (set! w (+ w chars-to-take))]
                [else (emit-line!)
                      (wloop rest-str #f)])))]
         [else
          (emit-line!)
          (ensure-indent!)
          (add-text! txt)
          (set! w (+ w ww))
          (set! need-space #f)])]))

  (when (or (not (string=? line "")) (null? out))
    (set! out (append out (list line))))

  (string-join out "\n"))

;;; Tokenizer

(define (whitespace? c)
  (or (char=? c #\space) (char=? c #\tab) (char=? c #\page) (char=? c #\return)))

(define (tokenize str)
  (define out '())
  (define i 0)
  (define n (string-length str))
  (let loop ()
    (when (< i n)
      (define ch (string-ref str i))
      (cond
        ;; Newline
        [(char=? ch #\newline)
         (set! out (append out (list (list 'newline "\n"))))
         (set! i (add1 i))]
        ;; ANSI escape
        [(char=? ch #\u001B)
         (define j (add1 i))
         (let inner ()
           (when (and (< j n) (not (char=? (string-ref str j) #\m)))
             (set! j (add1 j))
             (inner)))
         (when (< j n) (set! j (add1 j)))
         (set! out (append out (list (list 'ansi (substring str i j)))))
         (set! i j)]
        ;; Whitespace
        [(whitespace? ch)
         (define j i)
         (let inner ()
           (when (and (< j n) (whitespace? (string-ref str j)))
             (set! j (add1 j))
             (inner)))
         (set! out (append out (list (list 'space (substring str i j)))))
         (set! i j)]
        ;; Word
        [else
         (define j i)
         (let inner ()
           (when (and (< j n)
                      (let ([c (string-ref str j)])
                        (and (not (whitespace? c))
                             (not (char=? c #\newline))
                             (not (char=? c #\u001B)))))
             (set! j (add1 j))
             (inner)))
         (set! out (append out (list (list 'word (substring str i j)))))
         (set! i j)])
      (loop)))
  out)

;;; Measurement utilities

(define (text-width text)
  (define lines (split-string-by-newline text))
  (if (null? lines) 0 (apply max (map visible-length lines))))

(define (text-height text)
  (length (split-string-by-newline text)))

(define (text-size text)
  (values (text-width text) (text-height text)))
