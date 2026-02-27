#lang racket/base

;; components/markdown.rkt
;;
;; SPDX-License-Identifier: MIT
;;
;; Terminal markdown renderer inspired by Charmbracelet's Glamour.
;; Ported from cl-tuition's markdown renderer (tuition/markdown.lisp).
;;
;; Renders markdown to styled text suitable for display in a terminal.
;; Supports headers, bold, italic, inline code, code blocks, lists,
;; blockquotes, horizontal rules, and links.

(require racket/match
         racket/string
         racket/format
         racket/list
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

;;; ---------- Inline parsing ----------

;; Parse and render inline markdown elements.
;; Returns a styled string.
(define (parse-inline ms text)
  (define (go str)
    (cond
      [(string=? str "") ""]
      ;; Bold: **text**
      [(regexp-match #rx"^(.*)\\*\\*(.+?)\\*\\*(.*)" str)
       => (lambda (m)
            (string-append (go (list-ref m 1))
                           (render-inline-styled (markdown-style-bold-color ms) #t #f
                                                 (list-ref m 2))
                           (go (list-ref m 3))))]
      ;; Italic: *text*
      [(regexp-match #rx"^(.*)\\*(.+?)\\*(.*)" str)
       => (lambda (m)
            (string-append (go (list-ref m 1))
                           (render-inline-styled (markdown-style-italic-color ms) #f #t
                                                 (list-ref m 2))
                           (go (list-ref m 3))))]
      ;; Inline code: `text`
      [(regexp-match #rx"^(.*)`(.+?)`(.*)" str)
       => (lambda (m)
            (string-append (go (list-ref m 1))
                           (render-inline-styled (markdown-style-code-color ms) #f #f
                                                 (list-ref m 2))
                           (go (list-ref m 3))))]
      ;; Links: [text](url)
      [(regexp-match #rx"^(.*)\\[(.+?)\\]\\((.+?)\\)(.*)" str)
       => (lambda (m)
            (define link-text (list-ref m 2))
            (define link-style
              (make-style #:foreground (markdown-style-link-color ms)
                          #:underline (markdown-style-link-underline? ms)))
            (string-append (go (list-ref m 1))
                           (if (markdown-style-link-color ms)
                               (render-styled link-style link-text)
                               link-text)
                           (go (list-ref m 4))))]
      [else str]))
  (go text))

(define (render-inline-styled color bold? italic? text)
  (if color
      (render-styled (make-style #:foreground color #:bold bold? #:italic italic?)
                     text)
      (if bold?
          (render-styled (make-style #:bold #t) text)
          (if italic?
              (render-styled (make-style #:italic #t) text)
              text))))

;;; ---------- Block rendering ----------

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

  (define lines (string-split content "\n" #:trim? #f))
  (define output '())
  (define in-code-block? #f)
  (define code-lines '())

  (define (emit line)
    (set! output (cons line output)))

  (define (emit-code-block)
    (define block-margin (markdown-style-code-block-margin ms))
    (define inner-margin (make-string block-margin #\space))
    (define code-bg (markdown-style-code-block-bg ms))
    (define code-fg (markdown-style-code-block-fg ms))
    (for ([cl (in-list (reverse code-lines))])
      (define styled-line
        (if (or code-bg code-fg)
            (render-styled (make-style #:foreground code-fg #:background code-bg)
                           (string-append inner-margin cl))
            (string-append inner-margin cl)))
      (emit (string-append margin-str styled-line)))
    (set! code-lines '()))

  (for ([line (in-list lines)])
    (cond
      ;; Toggle code block
      [(regexp-match? #rx"^```" line)
       (cond
         [in-code-block?
          (emit-code-block)
          (set! in-code-block? #f)]
         [else
          (set! in-code-block? #t)
          (set! code-lines '())])]

      ;; Inside code block
      [in-code-block?
       (set! code-lines (cons line code-lines))]

      ;; Horizontal rule
      [(regexp-match? #rx"^(---+|\\*\\*\\*+|___+)\\s*$" line)
       (define hr-ch (markdown-style-hr-char ms))
       (emit (string-append margin-str (make-string content-width
                                                    (string-ref hr-ch 0))))]

      ;; H1
      [(regexp-match #rx"^# (.+)" line)
       => (lambda (m)
            (define txt (list-ref m 1))
            (define prefix (markdown-style-h1-prefix ms))
            (define s (make-style #:foreground (markdown-style-h1-color ms)
                                  #:bold (markdown-style-h1-bold? ms)))
            (emit (string-append margin-str
                                 (if (markdown-style-h1-color ms)
                                     (render-styled s (string-append prefix txt))
                                     (string-append prefix txt)))))]

      ;; H2
      [(regexp-match #rx"^## (.+)" line)
       => (lambda (m)
            (define txt (list-ref m 1))
            (define prefix (markdown-style-h2-prefix ms))
            (define s (make-style #:foreground (markdown-style-h2-color ms)
                                  #:bold (markdown-style-h2-bold? ms)))
            (emit (string-append margin-str
                                 (if (markdown-style-h2-color ms)
                                     (render-styled s (string-append prefix txt))
                                     (string-append prefix txt)))))]

      ;; H3
      [(regexp-match #rx"^### (.+)" line)
       => (lambda (m)
            (define txt (list-ref m 1))
            (define prefix (markdown-style-h3-prefix ms))
            (define s (make-style #:foreground (markdown-style-h3-color ms)
                                  #:bold (markdown-style-h3-bold? ms)))
            (emit (string-append margin-str
                                 (if (markdown-style-h3-color ms)
                                     (render-styled s (string-append prefix txt))
                                     (string-append prefix txt)))))]

      ;; Unordered list item
      [(regexp-match #rx"^\\s*[-*+]\\s+(.+)" line)
       => (lambda (m)
            (define txt (list-ref m 1))
            (define bullet (markdown-style-list-bullet ms))
            (define indent-str (make-string (markdown-style-list-indent ms) #\space))
            (emit (string-append margin-str indent-str bullet
                                 (parse-inline ms txt))))]

      ;; Ordered list item
      [(regexp-match #rx"^\\s*([0-9]+)\\.\\s+(.+)" line)
       => (lambda (m)
            (define num (list-ref m 1))
            (define txt (list-ref m 2))
            (define indent-str (make-string (markdown-style-ordered-indent ms) #\space))
            (emit (string-append margin-str indent-str num ". "
                                 (parse-inline ms txt))))]

      ;; Blockquote
      [(regexp-match #rx"^>\\s*(.*)" line)
       => (lambda (m)
            (define txt (list-ref m 1))
            (define prefix (markdown-style-quote-prefix ms))
            (define qcolor (markdown-style-quote-color ms))
            (define styled-prefix
              (if qcolor (render-styled (make-style #:foreground qcolor) prefix) prefix))
            (emit (string-append margin-str styled-prefix
                                 (parse-inline ms txt))))]

      ;; Empty line
      [(regexp-match? #rx"^\\s*$" line)
       (emit "")]

      ;; Normal paragraph text
      [else
       (define rendered (parse-inline ms line))
       (define text-color (markdown-style-text-color ms))
       (emit (string-append margin-str
                            (if text-color
                                (render-styled (make-style #:foreground text-color) rendered)
                                rendered)))]))

  ;; Close any remaining code block
  (when in-code-block?
    (emit-code-block))

  (string-join (reverse output) "\n"))
