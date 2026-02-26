#lang info

(define collection 'multi)
(define version "1.0")
(define deps '("base"
               "https://github.com/samth/racket-ansi.git?path=ansi#kettle-extensions"
               "tui-ubuf"
               "rackunit-lib"))

(define pkg-desc "implementation (no documentation) part of \"kettle\"")

(define pkg-authors '(green))

(define license '(MIT))
