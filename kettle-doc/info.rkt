#lang info

(define collection 'multi)

(define deps '("base"))

(define pkg-desc "documentation part of \"kettle\"")

(define pkg-authors '(green))
(define build-deps '("kettle-lib"
                     "htdp-doc"
                     "htdp-lib"
                     "racket-doc"
                     "scribble-lib"))
(define update-implies '("kettle-lib"))

(define license '(MIT))
