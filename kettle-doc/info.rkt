#lang info

(define collection 'multi)

(define deps '("base"))

(define pkg-desc "documentation part of \"kettle\"")

(define pkg-authors '(green))
(define build-deps '("kettle-lib"
                     "kettle-test-lib"
                     "htdp-doc"
                     "htdp-lib"
                     "racket-doc"
                     "rackunit-doc"
                     "rackunit-lib"
                     "scribble-lib"))
(define update-implies '("kettle-lib"))

(define license 'MIT)
