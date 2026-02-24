#lang racket/base

(require rackunit
         kettle/examples/stopwatch
         kettle/program
         kettle/image)

(define (char-key ch)
  (key-msg ch #f #f))

;; --- Construction ---

(test-case "initial state is stopped with zero elapsed"
  (define sw (make-stopwatch))
  (check-equal? (stopwatch-elapsed sw) 0.0)
  (check-false (stopwatch-running? sw))
  (check-false (stopwatch-last-tick sw)))

;; --- Start/stop with space ---

(test-case "space starts the stopwatch"
  (define sw (make-stopwatch))
  (define-values (sw2 cmd) (update sw (char-key #\space)))
  (check-true (stopwatch-running? sw2))
  (check-pred number? (stopwatch-last-tick sw2))
  (check-false cmd))

(test-case "space again stops it"
  (define sw (make-stopwatch))
  (define-values (sw2 _) (update sw (char-key #\space)))
  (define-values (sw3 cmd) (update sw2 (char-key #\space)))
  (check-false (stopwatch-running? sw3))
  (check-false (stopwatch-last-tick sw3))
  (check-false cmd))

;; --- Tick accumulates elapsed time ---

(test-case "tick while running adds elapsed time"
  (define sw (stopwatch 0.0 #t 1000.0)) ;; running, last-tick at 1000ms
  (define-values (sw2 cmd) (update sw (tick-msg 2000.0))) ;; 1 second later
  (check-= (stopwatch-elapsed sw2) 1.0 0.001)
  (check-equal? (stopwatch-last-tick sw2) 2000.0)
  (check-false cmd))

(test-case "tick while stopped is a no-op"
  (define sw (stopwatch 5.0 #f #f))
  (define-values (sw2 cmd) (update sw (tick-msg 9999.0)))
  (check-= (stopwatch-elapsed sw2) 5.0 0.001)
  (check-false cmd))

(test-case "first tick after start (no last-tick) adds 0"
  (define sw (stopwatch 0.0 #t #f))
  (define-values (sw2 cmd) (update sw (tick-msg 5000.0)))
  (check-= (stopwatch-elapsed sw2) 0.0 0.001)
  (check-equal? (stopwatch-last-tick sw2) 5000.0))

;; --- Reset ---

(test-case "r resets elapsed and stops"
  (define sw (stopwatch 42.5 #t 1000.0))
  (define-values (sw2 cmd) (update sw (char-key #\r)))
  (check-= (stopwatch-elapsed sw2) 0.0 0.001)
  (check-false (stopwatch-running? sw2))
  (check-false (stopwatch-last-tick sw2))
  (check-false cmd))

;; --- Quit ---

(test-case "q returns quit command"
  (define sw (make-stopwatch))
  (define-values (sw2 cmd) (update sw (char-key #\q)))
  (check-pred procedure? cmd))

;; --- View ---

(test-case "view produces an image"
  (define sw (make-stopwatch))
  (check-pred image? (view sw)))

(test-case "view shows STOPPED when not running"
  (define sw (make-stopwatch))
  (define s (image->string (view sw)))
  (check-regexp-match #rx"STOPPED" s))

(test-case "view shows RUNNING when running"
  (define sw (stopwatch 0.0 #t 1000.0))
  (define s (image->string (view sw)))
  (check-regexp-match #rx"RUNNING" s))

(test-case "view shows elapsed time"
  (define sw (stopwatch 65.3 #f #f)) ;; 1 min 5.3 sec
  (define s (image->string (view sw)))
  (check-regexp-match #rx"01:" s)
  (check-regexp-match #rx"05" s))

;; --- Purity: update does not mutate original ---

(test-case "update returns new struct, original unchanged"
  (define sw (make-stopwatch))
  (define-values (sw2 _) (update sw (char-key #\space)))
  (check-false (stopwatch-running? sw))
  (check-true (stopwatch-running? sw2)))
