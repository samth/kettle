#lang racket/base

(require rackunit
         kettle/examples/counter
         kettle/program
         kettle/image)

;; Helper: make a key-msg for a character
(define (char-key ch)
  (key-msg ch #f #f))

;; --- counter-on-key tests ---

(test-case "increment with +"
  (define-values (new-count c) (extract-update-result (counter-on-key 0 (char-key #\+))))
  (check-equal? new-count 1)
  (check-false c))

(test-case "increment with ="
  (define-values (new-count c) (extract-update-result (counter-on-key 5 (char-key #\=))))
  (check-equal? new-count 6)
  (check-false c))

(test-case "decrement with -"
  (define-values (new-count c) (extract-update-result (counter-on-key 3 (char-key #\-))))
  (check-equal? new-count 2)
  (check-false c))

(test-case "decrement with _"
  (define-values (new-count c) (extract-update-result (counter-on-key 0 (char-key #\_))))
  (check-equal? new-count -1)
  (check-false c))

(test-case "quit with q"
  (define-values (new-count c) (extract-update-result (counter-on-key 42 (char-key #\q))))
  (check-equal? new-count 42)
  (check-pred procedure? c))

(test-case "unrecognized key is no-op"
  (define-values (new-count c) (extract-update-result (counter-on-key 7 (char-key #\x))))
  (check-equal? new-count 7)
  (check-false c))

(test-case "multiple increments"
  (define-values (c1 _1) (extract-update-result (counter-on-key 0 (char-key #\+))))
  (define-values (c2 _2) (extract-update-result (counter-on-key c1 (char-key #\+))))
  (define-values (c3 _3) (extract-update-result (counter-on-key c2 (char-key #\+))))
  (check-equal? c3 3))

;; --- counter-view tests ---

(test-case "view produces an image"
  (define img (counter-view 0))
  (check-pred image? img))

(test-case "view contains count value"
  (define img (counter-view 42))
  (check-pred image? img)
  ;; The image should contain the text "Count: 42"
  (define s (image->string img))
  (check-regexp-match #rx"Count: 42" s))
