#lang racket/base

;; markdown.rkt -- Terminal markdown rendering with switchable themes.
;; Adapted from cl-tuition examples/markdown.lisp
;; https://github.com/atgreen/cl-tuition/blob/master/examples/markdown.lisp
;;
;; Key differences from cl-tuition:
;; - Uses Kettle's markdown component (kettle/components/markdown) which provides
;;   render-markdown returning a styled string, vs. cl-tuition's built-in
;;   tui:render-markdown.
;; - The markdown renderer uses Racket regexps for inline parsing instead of
;;   CL's manual character-by-character scanning.
;; - Style themes are Racket structs (markdown-style) instead of CL keywords
;;   (:dark, :light, :pink, :ascii).
;; - View wraps rendered markdown with string->image for display in the
;;   image-tree renderer.
;;
;; Press 1-4 to change theme, q to quit.

(require racket/match
         racket/format
         kettle/program
         kettle/image
         kettle/style
         (prefix-in md: kettle/components/markdown))

(define sample-markdown "# Kettle Markdown Renderer

Welcome to the **Kettle** markdown renderer, inspired by [Glamour](https://github.com/charmbracelet/glamour)!

## Features

This renderer supports:

- **Bold** and *italic* text
- `Inline code` with syntax highlighting
- Headers (H1, H2, H3)
- Unordered lists
- Ordered lists
- Block quotes
- Code blocks
- Links
- Horizontal rules

---

## Code Example

Here's a simple Racket example:

```racket
(define (hello-world)
  (displayln \"Hello, World!\"))

(hello-world)
```

## Lists

### Unordered List

- First item
- Second item
- Third item with **bold** text

### Ordered List

1. Step one
2. Step two
3. Step three

## Quotes

> Lisp is worth learning for the profound enlightenment experience
> you will have when you finally get it; that experience will make you
> a better programmer for the rest of your days.
>
> — Eric S. Raymond

## Styling

You can mix `code`, **bold**, *italic*, and [links](https://racket-lang.org) in the same paragraph!

---

*Press 'q' to quit, '1-4' to change styles*")

(define style-names #("" "dark" "light" "pink" "ascii"))

(define-kettle-program markdown-demo
  #:fields ([current-style 'dark])
  #:update (lambda (self msg)
    (match msg
      [(key-msg #\q _ _) (cmd self (quit-cmd))]
      [(key-msg _ _ #t)  (cmd self (quit-cmd))]
      [(key-msg #\1 _ _)
       (struct-copy markdown-demo self [current-style 'dark])]
      [(key-msg #\2 _ _)
       (struct-copy markdown-demo self [current-style 'light])]
      [(key-msg #\3 _ _)
       (struct-copy markdown-demo self [current-style 'pink])]
      [(key-msg #\4 _ _)
       (struct-copy markdown-demo self [current-style 'ascii])]
      [_ self]))
  #:view (lambda (self)
    (define style (markdown-demo-current-style self))
    (define rendered (md:render-markdown sample-markdown
                                         #:style style
                                         #:width 70))
    (define style-name (symbol->string style))
    (vcat 'left
          (string->image rendered)
          ""
          (text (format "Current style: ~a | Press 1-4 to change style, 'q' to quit"
                        style-name)))))

(module+ main
  (program-run (make-program (make-markdown-demo) #:alt-screen #t)))
