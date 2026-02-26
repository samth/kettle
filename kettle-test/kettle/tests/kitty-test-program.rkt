#lang racket/base

;; kitty-test-program.rkt -- Minimal Kettle program with Kitty keyboard enabled.
;; Used by e2e tests to verify CSI u parsing through a real terminal.
;; Displays the most recent key event details.

(require racket/match
         racket/format
         kettle
         kettle/run)

(module+ main
  (run (list)
       #:alt-screen #t
       #:kitty-keyboard #t
       #:on-key
       (lambda (events km)
         (define desc
           (match km
             [(key-msg key alt ctrl)
              (format "KEY:~a~a~a:~a"
                      key
                      (if alt ":alt" "")
                      (if ctrl ":ctrl" "")
                      (cond
                        [(key-event-msg? km)
                         (format "~a:shift=~a"
                                 (key-event-msg-event-type km)
                                 (key-event-msg-shift km))]
                        [else "legacy"]))]))
         (define new-events (cons desc events))
         (if (and (key-msg? km)
                  (equal? (key-msg-key km) #\q)
                  (not (key-msg-ctrl km))
                  (not (key-msg-alt km)))
             (cmd new-events (quit-cmd))
             new-events))
       #:on-msg
       (lambda (events msg)
         (cond
           [(key-release? msg)
            (cons (format "REL:~a~a~a~a"
                          (key-release-msg-key msg)
                          (if (key-release-msg-alt msg) ":alt" "")
                          (if (key-release-msg-ctrl msg) ":ctrl" "")
                          (if (key-release-msg-shift msg) ":shift" ""))
                  events)]
           [else events]))
       #:to-view
       (lambda (events)
         (apply vcat 'left
                (text "KITTY-TEST-READY")
                (if (null? events)
                    (list (text "NO-EVENTS"))
                    (map text events))))))
