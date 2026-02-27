#lang racket/base

(require rackunit
         racket/list
         kettle/private/protocol
         kettle/private/program)

;;; ============================================================
;;; coalesce-mouse-events
;;; ============================================================

;; Helper constructors
(define (move x y)
  (mouse-move-event x y #f #f #f))
(define (scroll x y dir [count 1])
  (mouse-scroll-event x y #f #f #f dir count))
(define (press x y btn)
  (mouse-press-event x y #f #f #f btn))
(define (key ch)
  (key-msg ch #f #f))

(test-case "coalesce: empty list"
  (check-equal? (coalesce-mouse-events '()) '()))

(test-case "coalesce: single move event unchanged"
  (define msgs (list (move 5 10)))
  (check-equal? (coalesce-mouse-events msgs) msgs))

(test-case "coalesce: consecutive moves collapse to last"
  (define m1 (move 1 1))
  (define m2 (move 2 2))
  (define m3 (move 3 3))
  (define result (coalesce-mouse-events (list m1 m2 m3)))
  (check-equal? (length result) 1)
  (check-equal? (mouse-event-x (car result)) 3)
  (check-equal? (mouse-event-y (car result)) 3))

(test-case "coalesce: moves separated by other events are not collapsed"
  (define m1 (move 1 1))
  (define k (key #\a))
  (define m2 (move 2 2))
  (define result (coalesce-mouse-events (list m1 k m2)))
  (check-equal? (length result) 3)
  (check-true (mouse-move-event? (first result)))
  (check-true (key-msg? (second result)))
  (check-true (mouse-move-event? (third result))))

(test-case "coalesce: consecutive scrolls same direction merge count"
  (define s1 (scroll 5 5 'up))
  (define s2 (scroll 5 5 'up))
  (define s3 (scroll 5 5 'up))
  (define result (coalesce-mouse-events (list s1 s2 s3)))
  (check-equal? (length result) 1)
  (check-equal? (mouse-scroll-event-count (car result)) 3))

(test-case "coalesce: consecutive scrolls different directions stay separate"
  (define s1 (scroll 5 5 'up))
  (define s2 (scroll 5 5 'down))
  (define result (coalesce-mouse-events (list s1 s2)))
  (check-equal? (length result) 2))

(test-case "coalesce: moves and scrolls intermixed"
  (define m1 (move 1 1))
  (define m2 (move 2 2))
  (define s1 (scroll 3 3 'up))
  (define s2 (scroll 3 3 'up))
  (define m3 (move 4 4))
  (define result (coalesce-mouse-events (list m1 m2 s1 s2 m3)))
  ;; m1+m2 -> one move, s1+s2 -> one scroll, m3 -> one move
  (check-equal? (length result) 3)
  (check-true (mouse-move-event? (first result)))
  (check-equal? (mouse-event-x (first result)) 2)
  (check-true (mouse-scroll-event? (second result)))
  (check-equal? (mouse-scroll-event-count (second result)) 2)
  (check-true (mouse-move-event? (third result)))
  (check-equal? (mouse-event-x (third result)) 4))

(test-case "coalesce: press events are not collapsed"
  (define p1 (press 1 1 'left))
  (define p2 (press 2 2 'left))
  (define result (coalesce-mouse-events (list p1 p2)))
  (check-equal? (length result) 2))

(test-case "coalesce: move between presses preserved"
  (define p1 (press 1 1 'left))
  (define m1 (move 5 5))
  (define m2 (move 6 6))
  (define p2 (press 10 10 'left))
  (define result (coalesce-mouse-events (list p1 m1 m2 p2)))
  ;; press, collapsed move, press
  (check-equal? (length result) 3)
  (check-true (mouse-press-event? (first result)))
  (check-true (mouse-move-event? (second result)))
  (check-equal? (mouse-event-x (second result)) 6)
  (check-true (mouse-press-event? (third result))))
