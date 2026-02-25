#lang racket/base

;; test-integration.rkt -- Integration tests using the headless test harness.
;; Tests full application lifecycle for all 4 example programs.

(require rackunit
         racket/string
         kettle/test
         kettle/examples/counter
         kettle/examples/stopwatch
         kettle/examples/todo
         kettle/examples/viewer
         kettle/examples/log-viewer)

;;; ============================================================
;;; Counter (run-style)
;;; ============================================================

(test-case "counter: increment/decrement sequence"
  (define tp (make-test-program/run 0 #:on-key counter-on-key #:to-view counter-view))
  (check-test-program-running tp)
  (check-test-program-contains tp "Count: 0")
  ;; Increment
  (test-program-press tp #\+)
  (check-test-program-contains tp "Count: 1")
  (test-program-press tp #\=)
  (check-test-program-contains tp "Count: 2")
  ;; Decrement
  (test-program-press tp #\-)
  (check-test-program-contains tp "Count: 1")
  (test-program-press tp #\_)
  (check-test-program-contains tp "Count: 0"))

(test-case "counter: quit with q"
  (define tp (make-test-program/run 0 #:on-key counter-on-key #:to-view counter-view))
  (test-program-press tp #\+)
  (test-program-press tp #\q)
  (check-test-program-done tp))

(test-case "counter: unknown keys are no-ops"
  (define tp (make-test-program/run 0 #:on-key counter-on-key #:to-view counter-view))
  (test-program-press tp #\x)
  (test-program-press tp #\z)
  (check-test-program-contains tp "Count: 0"))

(test-case "counter: view contains help text"
  (define tp (make-test-program/run 0 #:on-key counter-on-key #:to-view counter-view))
  (check-test-program-contains tp "increment")
  (check-test-program-contains tp "decrement")
  (check-test-program-contains tp "quit"))

;;; ============================================================
;;; Stopwatch (define-tea-program)
;;; ============================================================

(test-case "stopwatch: initial state"
  (define tp (make-test-program (make-stopwatch)))
  (check-test-program-running tp)
  (check-test-program-contains tp "STOPPED"))

(test-case "stopwatch: start/stop with space"
  (define tp (make-test-program (make-stopwatch)))
  (test-program-press tp #\space)
  (check-test-program-contains tp "RUNNING")
  (test-program-press tp #\space)
  (check-test-program-contains tp "STOPPED"))

(test-case "stopwatch: tick advances time deterministically"
  ;; Start with known state: running, last-tick at 1000ms
  (define tp (make-test-program (stopwatch 0.0 #t 1000.0)))
  ;; Send tick at 2000ms -- 1 second elapsed
  (test-program-send tp (tick-msg 2000.0))
  (check-test-program-contains tp "00:001.0")
  ;; Another tick at 3000ms -- 2 seconds total
  (test-program-send tp (tick-msg 3000.0))
  (check-test-program-contains tp "00:002.0"))

(test-case "stopwatch: reset clears elapsed"
  (define tp (make-test-program (stopwatch 42.5 #t 1000.0)))
  (test-program-press tp #\r)
  (check-test-program-contains tp "00:00")
  (check-test-program-contains tp "STOPPED"))

(test-case "stopwatch: quit"
  (define tp (make-test-program (make-stopwatch)))
  (test-program-press tp #\q)
  (check-test-program-done tp))

(test-case "stopwatch: view shows help text"
  (define tp (make-test-program (make-stopwatch)))
  (check-test-program-contains tp "start/stop")
  (check-test-program-contains tp "reset")
  (check-test-program-contains tp "quit"))

;;; ============================================================
;;; Todo (component delegation)
;;; ============================================================

(test-case "todo: initial state shows INSERT mode"
  (define tp (make-test-program (make-todo)))
  (check-test-program-running tp)
  (check-test-program-contains tp "[INSERT]")
  (check-test-program-contains tp "0 items"))

(test-case "todo: type and add item"
  (define tp (make-test-program (make-todo)))
  (test-program-type tp "buy milk")
  (test-program-press tp 'enter)
  (check-test-program-contains tp "buy milk")
  (check-test-program-contains tp "1 items"))

(test-case "todo: add multiple items"
  (define tp (make-test-program (make-todo)))
  (test-program-type tp "item one")
  (test-program-press tp 'enter)
  (test-program-type tp "item two")
  (test-program-press tp 'enter)
  (check-test-program-contains tp "item one")
  (check-test-program-contains tp "item two")
  (check-test-program-contains tp "2 items"))

(test-case "todo: switch to navigate mode with tab"
  (define tp (make-test-program (make-todo)))
  (test-program-press tp 'tab)
  (check-test-program-contains tp "[NAVIGATE]"))

(test-case "todo: delete item in navigate mode"
  (define tp (make-test-program (make-todo)))
  ;; Add an item
  (test-program-type tp "to delete")
  (test-program-press tp 'enter)
  (check-test-program-contains tp "1 items")
  ;; Switch to navigate mode and delete
  (test-program-press tp 'tab)
  (test-program-press tp #\d)
  (check-test-program-contains tp "0 items"))

(test-case "todo: C-q quits"
  (define tp (make-test-program (make-todo)))
  (test-program-press tp #\q #:ctrl #t)
  (check-test-program-done tp))

;;; ============================================================
;;; Viewer (viewport delegation)
;;; ============================================================

(define sample-content
  (string-join (for/list ([i (in-range 1 51)])
                 (format "Line ~a of sample content" i))
               "\n"))

(test-case "viewer: initial view shows content"
  (define fv (make-file-viewer sample-content "test.txt" #:width 60 #:height 20))
  (define tp (make-test-program fv))
  (check-test-program-running tp)
  (check-test-program-contains tp "test.txt")
  (check-test-program-contains tp "Line 1"))

(test-case "viewer: scroll with j/k"
  (define fv (make-file-viewer sample-content "test.txt" #:width 60 #:height 10))
  (define tp (make-test-program fv))
  ;; Scroll down
  (test-program-press tp #\j)
  (test-program-press tp #\j)
  (test-program-press tp #\j)
  (check-test-program-running tp)
  ;; Scroll back up
  (test-program-press tp #\k)
  (check-test-program-running tp))

(test-case "viewer: page scroll with space"
  (define fv (make-file-viewer sample-content "test.txt" #:width 60 #:height 10))
  (define tp (make-test-program fv))
  (test-program-press tp #\space)
  (check-test-program-running tp))

(test-case "viewer: resize with window-size-msg"
  (define fv (make-file-viewer sample-content "test.txt" #:width 60 #:height 10))
  (define tp (make-test-program fv))
  (test-program-resize tp 120 40)
  (check-test-program-running tp)
  (check-test-program-contains tp "test.txt"))

(test-case "viewer: quit with q"
  (define fv (make-file-viewer sample-content "test.txt"))
  (define tp (make-test-program fv))
  (test-program-press tp #\q)
  (check-test-program-done tp))

;;; ============================================================
;;; Log Viewer (vector-based, O(1) scroll)
;;; ============================================================

(define log-sample-content
  (string-join (for/list ([i (in-range 1 201)])
                 (format "2026-02-24 INFO Line ~a of log output" i))
               "\n"))

(test-case "log-viewer: initial view shows line 1 with line number"
  (define lv (make-log-viewer-from-string log-sample-content "test.log" #:width 80 #:height 20))
  (define tp (make-test-program lv))
  (check-test-program-running tp)
  (check-test-program-contains tp "test.log")
  (check-test-program-contains tp "200 lines")
  (check-test-program-contains tp "| 2026-02-24 INFO Line 1 of log output"))

(test-case "log-viewer: scroll down advances line numbers"
  (define lv (make-log-viewer-from-string log-sample-content "test.log" #:width 80 #:height 10))
  (define tp (make-test-program lv))
  (test-program-press tp #\j)
  (test-program-press tp #\j)
  (test-program-press tp #\j)
  (check-test-program-contains tp "| 2026-02-24 INFO Line 4 of log output"))

(test-case "log-viewer: jump to bottom with G shows last lines"
  (define lv (make-log-viewer-from-string log-sample-content "test.log" #:width 80 #:height 10))
  (define tp (make-test-program lv))
  (test-program-press tp #\G)
  (check-test-program-contains tp "| 2026-02-24 INFO Line 200 of log output"))

(test-case "log-viewer: resize adapts view"
  (define lv (make-log-viewer-from-string log-sample-content "test.log" #:width 80 #:height 10))
  (define tp (make-test-program lv))
  (test-program-resize tp 120 40)
  (check-test-program-running tp)
  (check-test-program-contains tp "test.log"))

(test-case "log-viewer: quit with q"
  (define lv (make-log-viewer-from-string log-sample-content "test.log"))
  (define tp (make-test-program lv))
  (test-program-press tp #\q)
  (check-test-program-done tp))

;;; ============================================================
;;; History tracking
;;; ============================================================

(test-case "history records model/view pairs"
  (define tp (make-test-program/run 0 #:on-key counter-on-key #:to-view counter-view))
  (test-program-press tp #\+)
  (test-program-press tp #\+)
  ;; History should have entries (reverse chronological)
  (define hist (test-program-history tp))
  (check-true (> (length hist) 1)))

;;; ============================================================
;;; Bug regression tests
;;; ============================================================

;; Regression: read-key returned (void) instead of #f when no input
;; was available, because `when` returns (void) on false condition.
;; This caused read-all-available-events to loop infinitely.
;; The fix: changed `when` to `and` in read-key.
(test-case "regression: model without subscriptions does not crash"
  ;; Models that implement gen:tea-model via #:methods but omit subscriptions
  ;; should not crash sync-subscriptions! or the test harness.
  (define tp (make-test-program/run 0 #:on-key counter-on-key #:to-view counter-view))
  (check-equal? (test-program-subscriptions tp) '())
  ;; Should still function normally
  (test-program-press tp #\+)
  (check-test-program-contains tp "Count: 1"))

;; Regression: models can handle many rapid updates without accumulation issues
(test-case "regression: rapid updates work correctly"
  (define tp (make-test-program/run 0 #:on-key counter-on-key #:to-view counter-view))
  (for ([_ (in-range 100)])
    (test-program-press tp #\+))
  (check-test-program-contains tp "Count: 100"))
