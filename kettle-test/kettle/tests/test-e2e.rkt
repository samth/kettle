#lang racket/base

;; test-e2e.rkt -- End-to-end tests using tmux.
;; Runs real Kettle programs in a pseudo-terminal and verifies behavior.
;; Tests are skipped when tmux is not available.
;;
;; Uses with-e2e-sessions, send+wait, and check-quit-exits from
;; kettle/test-tmux for concise test definitions.

(require rackunit
         racket/file
         racket/runtime-path
         setup/collects
         kettle/test-tmux)

(define-runtime-path kitty-test-path "kitty-test-program.rkt")
(define counter-path (collection-file-path "counter.rkt" "kettle" "examples"))
(define stopwatch-path (collection-file-path "stopwatch.rkt" "kettle" "examples"))
(define todo-path (collection-file-path "todo.rkt" "kettle" "examples"))
(define viewer-path (collection-file-path "viewer.rkt" "kettle" "examples"))
(define log-viewer-path (collection-file-path "log-viewer.rkt" "kettle" "examples"))

;; Guard: skip all tests if tmux is not available
(define have-tmux?
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (tmux-available?)))

(unless have-tmux?
  (displayln "tmux not available -- skipping e2e tests"))

;; Helper: make a viewer temp file with specified content
(define (make-viewer-temp-file lines)
  (define temp-file (make-temporary-file "kv~a.txt"))
  (call-with-output-file temp-file
                         (lambda (out)
                           (for ([i (in-range 1 (add1 lines))])
                             (fprintf out "Line ~a of test content" i)
                             (unless (= i lines)
                               (newline out))))
                         #:exists 'replace)
  temp-file)

;; Helper: make a log-viewer temp file with realistic log lines
(define (make-log-temp-file lines)
  (define temp-file (make-temporary-file "klv~a.log"))
  (call-with-output-file temp-file
                         (lambda (out)
                           (for ([i (in-range 1 (add1 lines))])
                             (fprintf out "2026-02-24 12:00:00 INFO Processing request ~a" i)
                             (unless (= i lines)
                               (newline out))))
                         #:exists 'replace)
  temp-file)

(when have-tmux?

  ;; Create temp files before starting sessions
  (define viewer-temp-file (make-viewer-temp-file 100))
  (define viewer-short-temp-file (make-viewer-temp-file 5))
  (define log-viewer-temp-file (make-log-temp-file 100000))

  (with-e2e-sessions ([sc counter-path #:width 80 #:height 24]
                      [st todo-path #:width 80 #:height 24]
                      [sw stopwatch-path #:width 80 #:height 24]
                      [sv viewer-path #:width 80 #:height 24 #:args (list viewer-temp-file)]
                      [svs viewer-path #:width 80 #:height 24 #:args (list viewer-short-temp-file)]
                      [slv log-viewer-path #:width 80 #:height 24 #:args (list log-viewer-temp-file)])
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
                       (send+wait sc "+" "Count: 1")
                       (check-tmux-contains sc "Count: 1"))
                     (test-case "e2e counter: increment with ="
                       (send+wait sc "=" "Count: 2")
                       (check-tmux-contains sc "Count: 2"))
                     (test-case "e2e counter: decrement with -"
                       (send+wait sc "-" "Count: 1")
                       (check-tmux-contains sc "Count: 1"))
                     (test-case "e2e counter: decrement with _"
                       (send+wait sc "_" "Count: 0")
                       (check-tmux-contains sc "Count: 0"))
                     (test-case "e2e counter: decrement below zero"
                       (send+wait sc "-" "Count: -1")
                       (check-tmux-contains sc "Count: -1"))
                     (test-case "e2e counter: multiple increments"
                       (send+wait sc "+" "Count: 0")
                       (send+wait sc "+" "Count: 1")
                       (send+wait sc "+" "Count: 2")
                       (send+wait sc "+" "Count: 3")
                       (check-tmux-contains sc "Count: 3"))
                     (test-case "e2e counter: unknown keys are no-ops"
                       (tmux-send-keys sc "x")
                       (tmux-send-keys sc "z")
                       (sleep 0.15)
                       (check-tmux-contains sc "Count: 3"))
                     (test-case "e2e counter: Alt+F toggles FPS display"
                       ;; Send Alt+F as raw bytes (ESC + f) atomically so
                       ;; the parser sees both bytes together as Alt+key.
                       (tmux-send-raw sc "\x1bf")
                       (tmux-wait-for sc "fps" #:timeout 3)
                       (check-tmux-contains sc "fps")
                       ;; Toggle off
                       (tmux-send-raw sc "\x1bf")
                       (sleep 0.3)
                       (check-tmux-not-contains sc "fps")
                       ;; Counter state should be unaffected
                       (check-tmux-contains sc "Count: 3"))
                     (test-case "e2e counter: quit exits program"
                       (check-quit-exits sc "q" "increment"))
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
                       (send+wait st "Enter" "1 items")
                       (check-tmux-contains st "buy milk"))
                     (test-case "e2e todo: add multiple items"
                       (tmux-type st "item two")
                       (send+wait st "Enter" "2 items")
                       (tmux-type st "item three")
                       (send+wait st "Enter" "3 items")
                       (check-tmux-contains st "buy milk")
                       (check-tmux-contains st "item two")
                       (check-tmux-contains st "item three"))
                     (test-case "e2e todo: switch modes with tab"
                       (send+wait st "Tab" "[NAVIGATE]")
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
                       (send+wait st "d" "2 items")
                       (check-tmux-contains st "2 items"))
                     (test-case "e2e todo: switch back to insert mode"
                       (send+wait st "Tab" "[INSERT]")
                       (check-tmux-contains st "[INSERT]"))
                     (test-case "e2e todo: quit exits program"
                       (check-quit-exits st "C-q" "Todo List"))
                     ;;; ============================================================
                     ;;; Stopwatch
                     ;;; ============================================================
                     (test-case "e2e stopwatch: initial render shows STOPPED"
                       (tmux-wait-for sw "STOPPED" #:timeout 10)
                       (check-tmux-contains sw "STOPPED")
                       (check-tmux-contains sw "go/stop")
                       (check-tmux-contains sw "rst")
                       (check-tmux-contains sw "quit"))
                     (test-case "e2e stopwatch: start with space"
                       (send+wait sw "Space" "RUNNING")
                       (check-tmux-contains sw "RUNNING"))
                     (test-case "e2e stopwatch: time accumulates while running"
                       ;; Already running from previous test
                       (sleep 0.5)
                       (send+wait sw "Space" "STOPPED")
                       (check-tmux-contains sw "STOPPED")
                       ;; With hundredths display, 00:00.00 means zero elapsed
                       (check-tmux-not-contains sw "00:00.00"))
                     (test-case "e2e stopwatch: reset clears elapsed"
                       (tmux-send-keys sw "r")
                       (tmux-wait-for sw "00:00.00" #:timeout 3))
                     (test-case "e2e stopwatch: reset while running"
                       (send+wait sw "Space" "RUNNING")
                       (sleep 0.2)
                       (tmux-send-keys sw "r")
                       (tmux-wait-for sw "STOPPED" #:timeout 3)
                       (check-tmux-contains sw "STOPPED"))
                     (test-case "e2e stopwatch: quit exits program"
                       (check-quit-exits sw "q" "go/stop"))
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
                       (send+wait sv "G" "Line 100" #:timeout 3)
                       (check-tmux-contains sv "Line 100"))
                     (test-case "e2e viewer: go to top with g"
                       (send+wait sv "g" "Line 1" #:timeout 3)
                       (check-tmux-contains sv "Line 1"))
                     (test-case "e2e viewer: quit exits program"
                       (check-quit-exits sv "q" "j/k:scroll"))
                     (test-case "e2e viewer: short file shows all lines"
                       (tmux-wait-for svs "Line 1" #:timeout 10)
                       (check-tmux-contains svs "Line 1")
                       (check-tmux-contains svs "Line 5"))
                     ;;; ============================================================
                     ;;; Log Viewer (100K lines)
                     ;;; ============================================================
                     (test-case "e2e log-viewer: initial render shows line numbers and content"
                       (tmux-wait-for slv "request 1" #:timeout 15)
                       (check-tmux-contains slv "1 |")
                       (check-tmux-contains slv "request 1")
                       (check-tmux-contains slv "100000 lines")
                       (check-tmux-contains slv "j/k:scroll"))
                     (test-case "e2e log-viewer: scroll down with j"
                       (for ([_ (in-range 5)])
                         (tmux-send-keys slv "j")
                         (sleep 0.03))
                       (sleep 0.15)
                       (check-tmux-matches slv #rx"request"))
                     (test-case "e2e log-viewer: jump to bottom with G"
                       (send+wait slv "G" "100000" #:timeout 5)
                       (check-tmux-contains slv "100000"))
                     (test-case "e2e log-viewer: quit exits program"
                       (check-quit-exits slv "q" "j/k:scroll")))

  ;;; ============================================================
  ;;; Kitty Keyboard Protocol (CSI u injection)
  ;;; ============================================================
  (with-e2e-sessions ([sk kitty-test-path #:width 80 #:height 24])
                     (test-case "e2e kitty: program starts with kitty keyboard enabled"
                       (tmux-wait-for sk "KITTY-TEST-READY" #:timeout 15)
                       (check-tmux-contains sk "NO-EVENTS"))
                     (test-case "e2e kitty: CSI u press for 'a'"
                       ;; ESC[97;1u = key 'a', no modifiers, press
                       (tmux-send-raw sk "\x1b[97;1u")
                       (tmux-wait-for sk "KEY:a" #:timeout 3)
                       (check-tmux-contains sk "KEY:a:press:shift=#f"))
                     (test-case "e2e kitty: CSI u press with Ctrl"
                       ;; ESC[106;5u = key 'j', Ctrl, press
                       (tmux-send-raw sk "\x1b[106;5u")
                       (tmux-wait-for sk "KEY:j:ctrl" #:timeout 3)
                       (check-tmux-contains sk "KEY:j:ctrl:press:shift=#f"))
                     (test-case "e2e kitty: CSI u press with Alt"
                       ;; ESC[98;3u = key 'b', Alt, press
                       (tmux-send-raw sk "\x1b[98;3u")
                       (tmux-wait-for sk "KEY:b:alt" #:timeout 3)
                       (check-tmux-contains sk "KEY:b:alt:press:shift=#f"))
                     (test-case "e2e kitty: CSI u press with Shift"
                       ;; ESC[99;2u = key 'c', Shift, press
                       (tmux-send-raw sk "\x1b[99;2u")
                       (tmux-wait-for sk "KEY:c" #:timeout 3)
                       (check-tmux-contains sk "KEY:c:press:shift=#t"))
                     (test-case "e2e kitty: CSI u key repeat"
                       ;; ESC[120;1:2u = key 'x', no modifiers, repeat
                       (tmux-send-raw sk "\x1b[120;1:2u")
                       (tmux-wait-for sk "KEY:x" #:timeout 3)
                       (check-tmux-contains sk "KEY:x:repeat:shift=#f"))
                     (test-case "e2e kitty: CSI u key release"
                       ;; ESC[97;1:3u = key 'a', no modifiers, release
                       (tmux-send-raw sk "\x1b[97;1:3u")
                       (tmux-wait-for sk "REL:a" #:timeout 3)
                       (check-tmux-contains sk "REL:a"))
                     (test-case "e2e kitty: CSI u release with Ctrl+Shift"
                       ;; ESC[106;6:3u = key 'j', Ctrl+Shift, release
                       (tmux-send-raw sk "\x1b[106;6:3u")
                       (tmux-wait-for sk "REL:j:ctrl:shift" #:timeout 3)
                       (check-tmux-contains sk "REL:j:ctrl:shift"))
                     (test-case "e2e kitty: CSI u arrow key"
                       ;; ESC[57352;1u = arrow up
                       (tmux-send-raw sk "\x1b[57352;1u")
                       (tmux-wait-for sk "KEY:up" #:timeout 3)
                       (check-tmux-contains sk "KEY:up:press:shift=#f"))
                     (test-case "e2e kitty: CSI u Enter key"
                       ;; ESC[13;1u = Enter
                       (tmux-send-raw sk "\x1b[13;1u")
                       (tmux-wait-for sk "KEY:enter" #:timeout 3)
                       (check-tmux-contains sk "KEY:enter:press:shift=#f"))
                     (test-case "e2e kitty: legacy keys still work"
                       ;; Send a plain 'x' via tmux (not CSI u)
                       (tmux-send-keys sk "x")
                       (tmux-wait-for sk "KEY:x" #:timeout 3)
                       (check-tmux-contains sk "KEY:x:legacy"))
                     (test-case "e2e kitty: quit with q"
                       (check-quit-exits sk "q" "KITTY-TEST-READY")))

  ;; Clean up temp files
  (for-each (lambda (f)
              (with-handlers ([exn:fail? void])
                (delete-file f)))
            (list viewer-temp-file viewer-short-temp-file log-viewer-temp-file)))
