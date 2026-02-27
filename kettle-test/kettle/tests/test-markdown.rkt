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
