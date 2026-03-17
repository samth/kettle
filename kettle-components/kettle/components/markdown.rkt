#lang racket/base

;; components/markdown.rkt
;;
;; SPDX-License-Identifier: MIT
;;
;; Terminal markdown renderer inspired by Charmbracelet's Glamour.
;; Ported from cl-tuition's markdown renderer (tuition/markdown.lisp).
;;
;; Uses the `commonmark` package for parsing and renders the resulting
;; AST to styled terminal text.

(require racket/match
         racket/string
         racket/format
         racket/list
         commonmark
         commonmark/struct
         kettle/style)

(provide (struct-out markdown-style)
         make-markdown-style-dark
         make-markdown-style-light
         make-markdown-style-pink
         make-markdown-style-ascii
         render-markdown)

;;; ---------- Markdown style configuration ----------

(struct markdown-style
  (doc-margin
   h1-prefix h1-color h1-bold?
   h2-prefix h2-color h2-bold?
   h3-prefix h3-color h3-bold?
   bold-color italic-color code-color
   code-block-bg code-block-fg code-block-margin
   list-bullet list-indent
   ordered-indent
   quote-prefix quote-color
   link-color link-underline?
   hr-char
   text-color)
  #:transparent)

(define (make-markdown-style-dark)
  (markdown-style
   2  ; doc-margin
   "# " fg-bright-magenta #t
   "## " fg-bright-cyan #t
   "### " fg-bright-yellow #t
   fg-bright-white fg-bright-white fg-bright-green
   bg-bright-black fg-bright-white 2
   "• " 2
   2
   "│ " fg-bright-blue
   fg-bright-cyan #t
   "─"
   #f))

(define (make-markdown-style-light)
  (markdown-style
   2
   "# " fg-magenta #t
   "## " fg-blue #t
   "### " fg-green #t
   fg-black fg-black fg-red
   bg-white fg-black 2
   "• " 2
   2
   "│ " fg-blue
   fg-blue #t
   "─"
   #f))

(define (make-markdown-style-pink)
  (markdown-style
   2
   "♥ " fg-bright-magenta #t
   "♦ " fg-magenta #t
   "♣ " fg-bright-red #t
   fg-bright-magenta fg-bright-magenta fg-bright-red
   bg-bright-black fg-bright-magenta 2
   "♡ " 2
   2
   "┃ " fg-bright-magenta
   fg-bright-red #t
   "♥"
   #f))

(define (make-markdown-style-ascii)
  (markdown-style
   2
   "# " #f #t
   "## " #f #t
   "### " #f #t
   #f #f #f
   #f #f 2
   "* " 2
   2
   "> " #f
   #f #f
   "-"
   #f))

;;; ---------- Inline rendering ----------

;; Render inline content to a styled string.
;; Inline content can be: a string, a struct (bold, italic, code, link, image,
;; line-break), or a list of inline content.
(define (render-inline ms x)
  (cond
    [(string? x) x]
    [(list? x) (apply string-append (map (lambda (c) (render-inline ms c)) x))]
    [(bold? x)
     (define text (render-inline ms (bold-content x)))
     (define color (markdown-style-bold-color ms))
     (if color
         (render-styled (make-style #:foreground color #:bold #t) text)
         (render-styled (make-style #:bold #t) text))]
    [(italic? x)
     (define text (render-inline ms (italic-content x)))
     (define color (markdown-style-italic-color ms))
     (if color
         (render-styled (make-style #:foreground color #:italic #t) text)
         (render-styled (make-style #:italic #t) text))]
    [(code? x)
     (define text (code-content x))
     (define color (markdown-style-code-color ms))
     (if color
         (render-styled (make-style #:foreground color) text)
         text)]
    [(link? x)
     (define text (render-inline ms (link-content x)))
     (define color (markdown-style-link-color ms))
     (if color
         (render-styled (make-style #:foreground color
                                    #:underline (markdown-style-link-underline? ms))
                        text)
         text)]
    [(image? x)
     (define desc (image-description x))
     (if (and desc (not (equal? desc "")))
         (render-inline ms desc)
         "[image]")]
    [(line-break? x) "\n"]
    [(html? x) (html-content x)]
    [else ""]))

;;; ---------- Block rendering ----------

;; Render a list of blocks, inserting blank lines between blocks.
(define (render-blocks ms margin-str content-width blocks)
  (define block-results
    (for/list ([b (in-list blocks)])
      (render-block ms margin-str content-width b)))
  (define non-empty (filter pair? block-results))
  (cond
    [(null? non-empty) '()]
    [else (apply append (add-between non-empty '("")))]))

;; Render a single block to a list of output lines.
(define (render-block ms margin-str content-width b)
  (cond
    [(heading? b)
     (render-heading ms margin-str (heading-depth b) (heading-content b))]
    [(paragraph? b)
     (render-paragraph ms margin-str content-width (paragraph-content b))]
    [(itemization? b)
     (if (itemization-start-num b)
         (render-ordered-list ms margin-str content-width b)
         (render-unordered-list ms margin-str content-width b))]
    [(blockquote? b)
     (render-blockquote ms margin-str content-width (blockquote-blocks b))]
    [(code-block? b)
     (render-code-block ms margin-str (code-block-content b))]
    [(html-block? b)
     ;; Render raw HTML as plain text
     (define lines (string-split (html-block-content b) "\n" #:trim? #f))
     (for/list ([l (in-list lines)] #:unless (regexp-match? #rx"^\\s*$" l))
       (string-append margin-str l))]
    [(thematic-break? b)
     (list (string-append margin-str
                          (make-string content-width
                                       (string-ref (markdown-style-hr-char ms) 0))))]
    [else '()]))

(define (render-heading ms margin-str level content)
  (define-values (prefix color bold?)
    (case level
      [(1) (values (markdown-style-h1-prefix ms)
                   (markdown-style-h1-color ms)
                   (markdown-style-h1-bold? ms))]
      [(2) (values (markdown-style-h2-prefix ms)
                   (markdown-style-h2-color ms)
                   (markdown-style-h2-bold? ms))]
      [else (values (markdown-style-h3-prefix ms)
                    (markdown-style-h3-color ms)
                    (markdown-style-h3-bold? ms))]))
  (define txt (render-inline ms content))
  (define full-text (string-append prefix txt))
  (list (string-append margin-str
                        (cond
                          [color (render-styled (make-style #:foreground color #:bold bold?)
                                               full-text)]
                          [bold? (render-styled (make-style #:bold #t) full-text)]
                          [else full-text]))))

(define (render-paragraph ms margin-str content-width content)
  (define txt (render-inline ms content))
  (define wrapped (wrap-text txt content-width))
  (define text-color (markdown-style-text-color ms))
  (define wrapped-lines (string-split wrapped "\n" #:trim? #f))
  (for/list ([line (in-list wrapped-lines)])
    (string-append margin-str
                    (if text-color
                        (render-styled (make-style #:foreground text-color) line)
                        line))))

(define (render-unordered-list ms margin-str content-width itemz)
  (apply append
    (for/list ([item-blocks (in-list (itemization-blockss itemz))])
      (define bullet (markdown-style-list-bullet ms))
      (define indent (markdown-style-list-indent ms))
      (define indent-str (make-string indent #\space))
      ;; For tight lists, extract inline content from paragraphs
      (define txt
        (render-inline ms
          (apply append
            (for/list ([b (in-list item-blocks)])
              (cond
                [(paragraph? b) (let ([c (paragraph-content b)])
                                  (if (list? c) c (list c)))]
                [else (list (render-block-to-text ms content-width b))])))))
      ;; Wrap long list items, indent continuation lines
      (define first-prefix (string-append margin-str indent-str bullet))
      (define cont-prefix (string-append margin-str (make-string (+ indent (string-length bullet)) #\space)))
      (define avail (max 20 (- content-width indent (string-length bullet))))
      (define wrapped (wrap-text txt avail))
      (define lines (string-split wrapped "\n" #:trim? #f))
      (if (null? lines)
          (list (string-append first-prefix ""))
          (cons (string-append first-prefix (first lines))
                (map (lambda (l) (string-append cont-prefix l)) (rest lines)))))))

(define (render-ordered-list ms margin-str content-width itemz)
  (define start (or (itemization-start-num itemz) 1))
  (apply append
    (for/list ([item-blocks (in-list (itemization-blockss itemz))]
               [i (in-naturals start)])
      (define indent-str (make-string (markdown-style-ordered-indent ms) #\space))
      (define txt
        (render-inline ms
          (apply append
            (for/list ([b (in-list item-blocks)])
              (cond
                [(paragraph? b) (let ([c (paragraph-content b)])
                                  (if (list? c) c (list c)))]
                [else (list (render-block-to-text ms content-width b))])))))
      (define num-prefix (string-append (number->string i) ". "))
      (define first-prefix (string-append margin-str indent-str num-prefix))
      (define cont-prefix (string-append margin-str (make-string (+ (markdown-style-ordered-indent ms) (string-length num-prefix)) #\space)))
      (define avail (max 20 (- content-width (markdown-style-ordered-indent ms) (string-length num-prefix))))
      (define wrapped (wrap-text txt avail))
      (define lines (string-split wrapped "\n" #:trim? #f))
      (if (null? lines)
          (list (string-append first-prefix ""))
          (cons (string-append first-prefix (first lines))
                (map (lambda (l) (string-append cont-prefix l)) (rest lines)))))))

;; Render a block to a flat text string (for use inside list items with nested blocks)
(define (render-block-to-text ms content-width b)
  (define lines (render-block ms "" content-width b))
  (string-join lines "\n"))

(define (render-code-block ms margin-str content)
  (define code-lines (string-split content "\n" #:trim? #f))
  ;; Remove trailing empty line if present
  (define trimmed-lines
    (if (and (pair? code-lines) (string=? (last code-lines) ""))
        (drop-right code-lines 1)
        code-lines))
  (define block-margin (markdown-style-code-block-margin ms))
  (define inner-margin (make-string block-margin #\space))
  (define code-bg (markdown-style-code-block-bg ms))
  (define code-fg (markdown-style-code-block-fg ms))
  (for/list ([cl (in-list trimmed-lines)])
    (define styled-line
      (if (or code-bg code-fg)
          (render-styled (make-style #:foreground code-fg #:background code-bg)
                         (string-append inner-margin cl))
          (string-append inner-margin cl)))
    (string-append margin-str styled-line)))

(define (render-blockquote ms margin-str content-width children)
  (define prefix (markdown-style-quote-prefix ms))
  (define qcolor (markdown-style-quote-color ms))
  (define styled-prefix
    (if qcolor (render-styled (make-style #:foreground qcolor) prefix) prefix))
  ;; Render children as blocks, then prepend quote prefix
  (define inner-lines (render-blocks ms "" content-width children))
  (for/list ([line (in-list inner-lines)])
    (string-append margin-str styled-prefix line)))

;;; ---------- Main entry point ----------

(define (render-markdown content
                         #:style [style-or-sym 'dark]
                         #:width [width 70])
  (define ms
    (cond
      [(markdown-style? style-or-sym) style-or-sym]
      [(eq? style-or-sym 'dark) (make-markdown-style-dark)]
      [(eq? style-or-sym 'light) (make-markdown-style-light)]
      [(eq? style-or-sym 'pink) (make-markdown-style-pink)]
      [(eq? style-or-sym 'ascii) (make-markdown-style-ascii)]
      [else (make-markdown-style-dark)]))

  (define margin (markdown-style-doc-margin ms))
  (define margin-str (make-string margin #\space))
  (define content-width (- width (* margin 2)))

  (define doc (string->document content))
  (define lines (render-blocks ms margin-str content-width (document-blocks doc)))
  (string-join lines "\n"))
