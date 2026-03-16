#lang racket/base

;; test-markdown.rkt -- Unit tests for the markdown rendering component.
;; Tests that typographic entities from the markdown parser are correctly
;; rendered to terminal output.

(require rackunit
         racket/string
         (prefix-in md: kettle/components/markdown))

(provide (all-defined-out))

;;; ============================================================
;;; Typographic entity rendering
;;; ============================================================

(test-case "markdown: smart single quotes preserved"
  ;; markdown/parse converts 'text' to lsquo + text + rsquo
  (define result (md:render-markdown "*'hello'*" #:style 'ascii))
  (check-true (string-contains? result "\u2018") "should contain left single quote")
  (check-true (string-contains? result "\u2019") "should contain right single quote")
  (check-true (string-contains? result "hello")))

(test-case "markdown: en dash preserved in ranges"
  ;; markdown/parse converts 1-4 inside quotes to 1 ndash 4
  (define result (md:render-markdown "*'1-4'*" #:style 'ascii))
  (check-true (string-contains? result "\u2013") "should contain en dash")
  (check-true (string-contains? result "1"))
  (check-true (string-contains? result "4")))

(test-case "markdown: em dash preserved"
  ;; markdown/parse converts --- to mdash
  (define result (md:render-markdown "text---more" #:style 'ascii))
  (check-true (string-contains? result "\u2014") "should contain em dash"))

(test-case "markdown: apostrophe in contractions"
  ;; Here's → Here + rsquo + s
  (define result (md:render-markdown "Here's an example" #:style 'ascii))
  (check-true (string-contains? result "Here"))
  (check-true (string-contains? result "s"))
  (check-true (string-contains? result "\u2019") "should contain right single quote"))

(test-case "markdown: entities in styled context"
  ;; Entities inside bold text should also render correctly
  (define result (md:render-markdown "**it's bold**" #:style 'ascii))
  (check-true (string-contains? result "\u2019") "should contain apostrophe entity"))

;;; ============================================================
;;; List item wrapping
;;; ============================================================

(test-case "markdown: unordered list items wrap at content width"
  ;; With width 30, doc-margin 2 on each side = 26 content-width,
  ;; list-indent 2, bullet "* " (2 chars) = 22 avail for text.
  ;; A long item should wrap to multiple lines.
  (define result (md:render-markdown
                  "* This is a very long list item that should definitely wrap to multiple lines"
                  #:style 'ascii #:width 30))
  (define lines (string-split result "\n" #:trim? #f))
  ;; Should produce more than one line for the list item
  (check-true (> (length lines) 1)
              "long list item should wrap")
  ;; First line should have the bullet
  (check-true (string-contains? (car lines) "* ")
              "first line should contain bullet")
  ;; Continuation lines should be indented (no bullet)
  (for ([line (in-list (cdr lines))]
        #:when (not (string=? line "")))
    (check-false (string-contains? line "* ")
                 "continuation lines should not have bullet")))

(test-case "markdown: short list items do not wrap"
  (define result (md:render-markdown "* Short item" #:style 'ascii #:width 70))
  (define lines (string-split result "\n" #:trim? #f))
  ;; Should be exactly one non-empty line
  (define non-empty (filter (lambda (l) (not (string=? l ""))) lines))
  (check-equal? (length non-empty) 1)
  (check-true (string-contains? (car non-empty) "Short item")))
