#lang racket/base

;; test-e2e.rkt -- End-to-end tests using tmux.
;; Runs real Kettle programs in a pseudo-terminal and verifies behavior.
;; Tests are skipped when tmux is not available.
;;
;; Optimization: All tmux sessions are started in parallel at the beginning,
;; so the startup delay is paid only once rather than per-test.

(require rackunit
         racket/file
         setup/collects
         kettle/test-tmux)

(define counter-path (collection-file-path "counter.rkt" "kettle" "examples"))
(define stopwatch-path (collection-file-path "stopwatch.rkt" "kettle" "examples"))
(define todo-path (collection-file-path "todo.rkt" "kettle" "examples"))
(define viewer-path (collection-file-path "viewer.rkt" "kettle" "examples"))

;; Guard: skip all tests if tmux is not available
(define have-tmux?
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (tmux-available?)))

(unless have-tmux?
  (displayln "tmux not available -- skipping e2e tests"))

;; Helper: make a viewer temp file with specified content
(define (make-viewer-temp-file lines)
  (define temp-file (make-temporary-file "kettle-viewer-~a.txt"))
  (call-with-output-file temp-file
                         (lambda (out)
                           (for ([i (in-range 1 (add1 lines))])
                             (fprintf out "Line ~a of test content" i)
                             (unless (= i lines)
                               (newline out))))
                         #:exists 'replace)
  temp-file)

(when have-tmux?

  ;; Create temp files before starting sessions
  (define viewer-temp-file (make-viewer-temp-file 100))
  (define viewer-short-temp-file (make-viewer-temp-file 5))

  ;; Start ALL sessions in parallel by launching them from threads.
  ;; tmux-start's startup-delay is 0.5s -- by starting all at once
  ;; we pay this cost only once.
  (define counter-box (box #f))
  (define todo-box (box #f))
  (define stopwatch-box (box #f))
  (define viewer-box (box #f))
  (define viewer-short-box (box #f))

  (define threads
    (list
     (thread (lambda () (set-box! counter-box (tmux-start counter-path #:width 80 #:height 24))))
     (thread (lambda () (set-box! todo-box (tmux-start todo-path #:width 80 #:height 24))))
     (thread (lambda () (set-box! stopwatch-box (tmux-start stopwatch-path #:width 80 #:height 24))))
     (thread (lambda ()
               (set-box!
                viewer-box
                (tmux-start viewer-path #:width 80 #:height 24 #:args (list viewer-temp-file)))))
     (thread
      (lambda ()
        (set-box!
         viewer-short-box
         (tmux-start viewer-path #:width 80 #:height 24 #:args (list viewer-short-temp-file)))))))

  ;; Wait for all sessions to start
  (for-each thread-wait threads)

  (define sc (unbox counter-box))
  (define st (unbox todo-box))
  (define sw (unbox stopwatch-box))
  (define sv (unbox viewer-box))
  (define svs (unbox viewer-short-box))

  ;; Cleanup at end
  (define (cleanup-all)
    (for-each (lambda (s)
                (with-handlers ([exn:fail? void])
                  (tmux-kill s)))
              (list sc st sw sv svs))
    (for-each (lambda (f)
                (with-handlers ([exn:fail? void])
                  (delete-file f)))
              (list viewer-temp-file viewer-short-temp-file)))

  (dynamic-wind
   void
   (lambda ()

     ;;; ============================================================
     ;;; Counter
     ;;; ============================================================

     (test-case "e2e counter: initial render shows count and help"
       (tmux-wait-for sc "Count: 0" #:timeout 10)
       (check-tmux-contains sc "Count: 0")
       (check-tmux-contains sc "increment")
       (check-tmux-contains sc "decrement")
       (check-tmux-contains sc "quit"))

     (test-case "e2e counter: increment with +"
       (tmux-send-keys sc "+")
       (tmux-wait-for sc "Count: 1" #:timeout 5)
       (check-tmux-contains sc "Count: 1"))

     (test-case "e2e counter: increment with ="
       (tmux-send-keys sc "=")
       (tmux-wait-for sc "Count: 2" #:timeout 5)
       (check-tmux-contains sc "Count: 2"))

     (test-case "e2e counter: decrement with -"
       (tmux-send-keys sc "-")
       (tmux-wait-for sc "Count: 1" #:timeout 5)
       (check-tmux-contains sc "Count: 1"))

     (test-case "e2e counter: decrement with _"
       (tmux-send-keys sc "_")
       (tmux-wait-for sc "Count: 0" #:timeout 5)
       (check-tmux-contains sc "Count: 0"))

     (test-case "e2e counter: decrement below zero"
       (tmux-send-keys sc "-")
       (tmux-wait-for sc "Count: -1" #:timeout 5)
       (check-tmux-contains sc "Count: -1"))

     (test-case "e2e counter: multiple increments"
       (tmux-send-keys sc "+")
       (tmux-wait-for sc "Count: 0" #:timeout 5)
       (tmux-send-keys sc "+")
       (tmux-wait-for sc "Count: 1" #:timeout 5)
       (tmux-send-keys sc "+")
       (tmux-wait-for sc "Count: 2" #:timeout 5)
       (tmux-send-keys sc "+")
       (tmux-wait-for sc "Count: 3" #:timeout 5)
       (check-tmux-contains sc "Count: 3"))

     (test-case "e2e counter: unknown keys are no-ops"
       (tmux-send-keys sc "x")
       (tmux-send-keys sc "z")
       (sleep 0.15)
       (check-tmux-contains sc "Count: 3"))

     (test-case "e2e counter: quit exits program"
       (tmux-send-keys sc "q")
       (sleep 0.3)
       (define captured (tmux-capture sc #:trim #t))
       (check-false (regexp-match? #rx"increment" captured)
                    (format "Counter TUI should no longer be visible after quit, got: ~a" captured)))

     ;;; ============================================================
     ;;; Todo
     ;;; ============================================================

     (test-case "e2e todo: initial render"
       (tmux-wait-for st "[INSERT]" #:timeout 10)
       (check-tmux-contains st "[INSERT]")
       (check-tmux-contains st "0 items")
       (check-tmux-contains st "Todo List"))

     (test-case "e2e todo: help text shows keybindings"
       (check-tmux-contains st "switch mode")
       (check-tmux-contains st "add item")
       (check-tmux-contains st "delete item")
       (check-tmux-contains st "C-q"))

     (test-case "e2e todo: empty enter does not add item"
       (tmux-send-keys st "Enter")
       (sleep 0.15)
       (check-tmux-contains st "0 items"))

     (test-case "e2e todo: add a single item"
       (tmux-type st "buy milk")
       (tmux-send-keys st "Enter")
       (tmux-wait-for st "1 items" #:timeout 5)
       (check-tmux-contains st "1 items")
       (check-tmux-contains st "buy milk"))

     (test-case "e2e todo: add multiple items"
       (tmux-type st "item two")
       (tmux-send-keys st "Enter")
       (tmux-wait-for st "2 items" #:timeout 5)
       (tmux-type st "item three")
       (tmux-send-keys st "Enter")
       (tmux-wait-for st "3 items" #:timeout 5)
       (check-tmux-contains st "buy milk")
       (check-tmux-contains st "item two")
       (check-tmux-contains st "item three"))

     (test-case "e2e todo: switch modes with tab"
       (tmux-send-keys st "Tab")
       (tmux-wait-for st "[NAVIGATE]" #:timeout 5)
       (check-tmux-contains st "[NAVIGATE]"))

     (test-case "e2e todo: navigate list with arrow keys"
       (tmux-send-keys st "Down")
       (sleep 0.1)
       (tmux-send-keys st "Down")
       (sleep 0.1)
       (check-tmux-contains st "buy milk")
       (check-tmux-contains st "item two")
       (check-tmux-contains st "item three"))

     (test-case "e2e todo: delete item in navigate mode"
       (tmux-send-keys st "d")
       (tmux-wait-for st "2 items" #:timeout 5)
       (check-tmux-contains st "2 items"))

     (test-case "e2e todo: switch back to insert mode"
       (tmux-send-keys st "Tab")
       (tmux-wait-for st "[INSERT]" #:timeout 5)
       (check-tmux-contains st "[INSERT]"))

     (test-case "e2e todo: quit exits program"
       (tmux-send-keys st "C-q")
       (sleep 0.3)
       (define captured (tmux-capture st #:trim #t))
       (check-false (regexp-match? #rx"Todo List" captured)
                    (format "Todo TUI should no longer be visible after quit, got: ~a" captured)))

     ;;; ============================================================
     ;;; Stopwatch
     ;;; ============================================================

     (test-case "e2e stopwatch: initial render shows STOPPED"
       (tmux-wait-for sw "STOPPED" #:timeout 10)
       (check-tmux-contains sw "STOPPED")
       (check-tmux-contains sw "start/stop")
       (check-tmux-contains sw "reset")
       (check-tmux-contains sw "quit"))

     (test-case "e2e stopwatch: start with space"
       (tmux-send-keys sw "Space")
       (tmux-wait-for sw "RUNNING" #:timeout 5)
       (check-tmux-contains sw "RUNNING"))

     (test-case "e2e stopwatch: time accumulates while running"
       ;; Already running from previous test
       (sleep 1.2)
       (tmux-send-keys sw "Space")
       (tmux-wait-for sw "STOPPED" #:timeout 5)
       (check-tmux-contains sw "STOPPED")
       (define captured (tmux-capture sw #:trim #t))
       (check-false (regexp-match? #rx"00:000\\.0" captured)
                    (format "Elapsed time should be > 0 after running, got: ~a" captured)))

     (test-case "e2e stopwatch: reset clears elapsed"
       (tmux-send-keys sw "r")
       (sleep 0.15)
       (check-tmux-contains sw "00:000.0"))

     (test-case "e2e stopwatch: reset while running"
       (tmux-send-keys sw "Space")
       (tmux-wait-for sw "RUNNING" #:timeout 5)
       (sleep 0.2)
       (tmux-send-keys sw "r")
       (sleep 0.15)
       (check-tmux-contains sw "STOPPED")
       (check-tmux-contains sw "00:000.0"))

     (test-case "e2e stopwatch: quit exits program"
       (tmux-send-keys sw "q")
       (sleep 0.3)
       (define captured (tmux-capture sw #:trim #t))
       (check-false (regexp-match? #rx"start/stop" captured)
                    (format "Stopwatch TUI should no longer be visible after quit, got: ~a"
                            captured)))

     ;;; ============================================================
     ;;; Viewer
     ;;; ============================================================

     (test-case "e2e viewer: initial render shows content and help"
       (tmux-wait-for sv "Line 1" #:timeout 10)
       (check-tmux-contains sv "Line 1")
       (check-tmux-contains sv "j/k:scroll")
       (check-tmux-contains sv "q:quit"))

     (test-case "e2e viewer: scroll down with j"
       (for ([_ (in-range 5)])
         (tmux-send-keys sv "j")
         (sleep 0.03))
       (sleep 0.15)
       (check-tmux-matches sv #rx"Line"))

     (test-case "e2e viewer: scroll up with k"
       (for ([_ (in-range 5)])
         (tmux-send-keys sv "k")
         (sleep 0.03))
       (tmux-wait-for sv "Line 1" #:timeout 3)
       (check-tmux-contains sv "Line 1"))

     (test-case "e2e viewer: page down with space"
       (tmux-send-keys sv "Space")
       (sleep 0.15)
       (check-tmux-matches sv #rx"Line"))

     (test-case "e2e viewer: page up with b"
       (tmux-send-keys sv "b")
       (sleep 0.15)
       (check-tmux-matches sv #rx"Line"))

     (test-case "e2e viewer: go to bottom with G"
       (tmux-send-keys sv "G")
       (tmux-wait-for sv "Line 100" #:timeout 3)
       (check-tmux-contains sv "Line 100"))

     (test-case "e2e viewer: go to top with g"
       (tmux-send-keys sv "g")
       (tmux-wait-for sv "Line 1" #:timeout 3)
       (check-tmux-contains sv "Line 1"))

     (test-case "e2e viewer: quit exits program"
       (tmux-send-keys sv "q")
       (sleep 0.3)
       (define captured (tmux-capture sv #:trim #t))
       (check-false (regexp-match? #rx"j/k:scroll" captured)
                    (format "Viewer TUI should no longer be visible after quit, got: ~a" captured)))

     (test-case "e2e viewer: short file shows all lines"
       (tmux-wait-for svs "Line 1" #:timeout 10)
       (check-tmux-contains svs "Line 1")
       (check-tmux-contains svs "Line 5")))
   cleanup-all)) ;; end when have-tmux?
