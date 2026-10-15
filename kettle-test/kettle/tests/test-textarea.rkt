#lang racket/base

;; test-textarea.rkt -- the textarea component's view must follow the cursor.

(require rackunit
         racket/string
         racket/list
         kettle/image
         kettle/components/textarea
         (only-in kettle key-msg update view))

(define (press ta k)
  (update ta (key-msg k #f #f)))
(define (type ta s)
  (for/fold ([ta ta]) ([c (in-string s)])
    (press ta (if (char=? c #\newline) 'enter c))))

;; Rendered rows as plain strings, without ANSI codes.
(define (rows ta)
  (map (lambda (l) (string-trim (regexp-replace* #rx"\e\\[[0-9;]*m" l "") #:left? #f))
       (string-split (image->string (view ta)) "\n")))

(define (fresh #:height [h 3])
  (textarea-focus (make-textarea #:width 30 #:height h #:show-line-numbers #f #:prompt "")))

(test-case "textarea: typing past the bottom keeps the cursor's line visible"
  (define ta (type (fresh) "one\ntwo\nthree\nfour\nfive"))
  (define r (rows ta))
  (check-equal? (length r) 3)
  (check-true (string-prefix? (last r) "five") (format "rows: ~s" r)))

(test-case "textarea: moving back up scrolls the view back"
  (define ta
    (for/fold ([ta (type (fresh) "one\ntwo\nthree\nfour\nfive")]) ([_ 4])
      (press ta 'up)))
  (check-true (string-prefix? (first (rows ta)) "one") (format "rows: ~s" (rows ta))))

(test-case "textarea: moving within the visible window does not scroll"
  (define ta (press (type (fresh) "one\ntwo\nthree\nfour\nfive") 'up))
  (check-equal? (map (lambda (s) (substring s 0 (min 4 (string-length s)))) (rows ta))
                '("thre" "four" "five")))

(test-case "textarea: line numbers follow the scroll"
  (define ta (type (textarea-focus (make-textarea #:width 30 #:height 2 #:prompt "")) "a\nb\nc\nd"))
  (define r (rows ta))
  (check-true (regexp-match? #px"^\\s*3 c" (first r)) (format "rows: ~s" r))
  (check-true (regexp-match? #px"^\\s*4 d" (second r)) (format "rows: ~s" r)))

(test-case "textarea: short content still renders `height` rows"
  (check-equal? (length (rows (type (fresh #:height 4) "x"))) 4))
