#lang racket/base

;; components/markdown.rkt
;;
;; SPDX-License-Identifier: MIT
;;
;; Terminal markdown renderer inspired by Charmbracelet's Glamour.
;; Ported from cl-tuition's markdown renderer (tuition/markdown.lisp).
;;
;; Uses the `markdown` package for parsing and renders the resulting
;; xexprs to styled terminal text.

(require racket/match
         racket/string
         racket/format
         racket/list
         markdown/parse
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

;;; ---------- Typographic entities ----------

;; The markdown/parse library produces bare symbols for HTML entities
;; (e.g., lsquo for &lsquo;).  Map them to their Unicode characters.
(define (entity-symbol->string sym)
  (case sym
    [(lsquo) "\u2018"]
    [(rsquo) "\u2019"]
    [(ldquo) "\u201C"]
    [(rdquo) "\u201D"]
    [(ndash) "\u2013"]
    [(mdash) "\u2014"]
    [(hellip) "\u2026"]
    [(amp) "&"]
    [(lt) "<"]
    [(gt) ">"]
    [(nbsp) "\u00A0"]
    [(copy) "\u00A9"]
    [(reg) "\u00AE"]
    [(trade) "\u2122"]
    [else (format "~a" sym)]))

;;; ---------- Inline rendering ----------

;; Render a list of inline xexpr children to a styled string.
(define (render-inlines ms children)
  (apply string-append
    (for/list ([c (in-list children)])
      (render-inline ms c))))

(define (render-inline ms x)
  (match x
    [(? string? s) s]
    [`(strong ,_ . ,children)
     (define text (render-inlines ms children))
     (define color (markdown-style-bold-color ms))
     (if color
         (render-styled (make-style #:foreground color #:bold #t) text)
         (render-styled (make-style #:bold #t) text))]
    [`(em ,_ . ,children)
     (define text (render-inlines ms children))
     (define color (markdown-style-italic-color ms))
     (if color
         (render-styled (make-style #:foreground color #:italic #t) text)
         (render-styled (make-style #:italic #t) text))]
    [`(code ,_ . ,children)
     (define text (render-inlines ms children))
     (define color (markdown-style-code-color ms))
     (if color
         (render-styled (make-style #:foreground color) text)
         text)]
    [`(a ,attrs . ,children)
     (define link-text (render-inlines ms children))
     (define color (markdown-style-link-color ms))
     (if color
         (render-styled (make-style #:foreground color
                                    #:underline (markdown-style-link-underline? ms))
                        link-text)
         link-text)]
    [`(br . ,_) "\n"]
    [`(img ,attrs . ,_)
     (define alt-pair (assoc 'alt attrs))
     (if alt-pair (cadr alt-pair) "[image]")]
    ;; Pass through content of unknown inline wrappers
    [`(sup ,_ . ,children) (render-inlines ms children)]
    [`(span ,_ . ,children) (render-inlines ms children)]
    ;; Typographic entities produced by markdown/parse
    [(? symbol? sym) (entity-symbol->string sym)]
    [_ ""]))

;;; ---------- Block rendering ----------

;; Extract inline content from children, unwrapping p tags.
;; Handles both (li () "text") and (li () (p () "text")).
(define (extract-inline-content children)
  (apply append
    (for/list ([c (in-list children)])
      (match c
        [`(p ,_ . ,inner) inner]
        [_ (list c)]))))

;; Render a list of block-level xexprs, inserting blank lines between blocks.
(define (render-blocks ms margin-str content-width xexprs)
  (define block-results
    (for/list ([x (in-list xexprs)]
               #:unless (and (string? x) (regexp-match? #rx"^\\s*$" x)))
      (render-block ms margin-str content-width x)))
  (define non-empty (filter pair? block-results))
  (cond
    [(null? non-empty) '()]
    [else (apply append (add-between non-empty '("")))]))

;; Render a single block-level xexpr element to a list of output lines.
(define (render-block ms margin-str content-width x)
  (match x
    [(? string? s)
     (if (regexp-match? #rx"^\\s*$" s) '() (list (string-append margin-str s)))]
    [`(h1 ,_ . ,children)
     (render-heading ms margin-str 1 children)]
    [`(h2 ,_ . ,children)
     (render-heading ms margin-str 2 children)]
    [`(h3 ,_ . ,children)
     (render-heading ms margin-str 3 children)]
    [`(,(? (lambda (t) (memq t '(h4 h5 h6)))) ,_ . ,children)
     (render-heading ms margin-str 3 children)]
    [`(p ,_ . ,children)
     (render-paragraph ms margin-str children)]
    [`(ul ,_ . ,children)
     (render-unordered-list ms margin-str children)]
    [`(ol ,_ . ,children)
     (render-ordered-list ms margin-str children)]
    [`(pre ,_ . ,children)
     (render-code-block ms margin-str children)]
    [`(blockquote ,_ . ,children)
     (render-blockquote ms margin-str content-width children)]
    [`(hr . ,_)
     (list (string-append margin-str
                          (make-string content-width
                                       (string-ref (markdown-style-hr-char ms) 0))))]
    [`(div ,_ . ,children)
     (render-blocks ms margin-str content-width children)]
    [_ '()]))

(define (render-heading ms margin-str level children)
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
  (define txt (render-inlines ms children))
  (define full-text (string-append prefix txt))
  (list (string-append margin-str
                        (cond
                          [color (render-styled (make-style #:foreground color #:bold bold?)
                                               full-text)]
                          [bold? (render-styled (make-style #:bold #t) full-text)]
                          [else full-text]))))

(define (render-paragraph ms margin-str children)
  (define txt (render-inlines ms children))
  (define text-color (markdown-style-text-color ms))
  (list (string-append margin-str
                        (if text-color
                            (render-styled (make-style #:foreground text-color) txt)
                            txt))))

(define (render-unordered-list ms margin-str items)
  (apply append
    (for/list ([item (in-list items)]
               #:when (and (pair? item) (eq? (car item) 'li)))
      (match item
        [`(li ,_ . ,children)
         (define bullet (markdown-style-list-bullet ms))
         (define indent-str (make-string (markdown-style-list-indent ms) #\space))
         (define inline-children (extract-inline-content children))
         (define txt (render-inlines ms inline-children))
         (list (string-append margin-str indent-str bullet txt))]))))

(define (render-ordered-list ms margin-str items)
  (define li-items (filter (lambda (x) (and (pair? x) (eq? (car x) 'li))) items))
  (for/list ([item (in-list li-items)]
             [i (in-naturals 1)])
    (match item
      [`(li ,_ . ,children)
       (define indent-str (make-string (markdown-style-ordered-indent ms) #\space))
       (define inline-children (extract-inline-content children))
       (define txt (render-inlines ms inline-children))
       (string-append margin-str indent-str (number->string i) ". " txt)])))

(define (render-code-block ms margin-str children)
  ;; parse-markdown produces: (pre () (code ((class "brush: lang")) "code\n"))
  (define code-text
    (match children
      [`((code ,_ . ,code-children))
       (apply string-append (map (lambda (c) (if (string? c) c "")) code-children))]
      [_ (apply string-append (map (lambda (c) (if (string? c) c "")) children))]))
  (define code-lines (string-split code-text "\n" #:trim? #f))
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

  (define xexprs (parse-markdown content))
  (define lines (render-blocks ms margin-str content-width xexprs))
  (string-join lines "\n"))
