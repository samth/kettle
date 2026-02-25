#lang racket/base

;; test-input.rkt -- Unit tests for input parsing.
;; Tests the CSI u (Kitty keyboard protocol) parser and
;; regression tests for existing input parsing.

(require rackunit
         racket/match
         kettle/test
         kettle)

(provide (all-defined-out))

;; Helper: parse a byte string through read-key
(define (parse-input str)
  (define port (open-input-string str))
  (parameterize ([current-input-stream port])
    (read-key)))

;;; ============================================================
;;; CSI u parsing (Kitty keyboard protocol)
;;; ============================================================

(test-case "csi-u: simple key press (a)"
  (define evt (parse-input "\e[97;1u"))
  (check-true (key-event-msg? evt))
  (check-equal? (key-msg-key evt) #\a)
  (check-false (key-msg-alt evt))
  (check-false (key-msg-ctrl evt))
  (check-false (key-event-msg-shift evt))
  (check-equal? (key-event-msg-event-type evt) 'press))

(test-case "csi-u: keycode only, no modifiers"
  (define evt (parse-input "\e[97u"))
  (check-true (key-event-msg? evt))
  (check-equal? (key-msg-key evt) #\a)
  (check-equal? (key-event-msg-event-type evt) 'press))

(test-case "csi-u: key with Shift"
  (define evt (parse-input "\e[97;2u"))
  (check-true (key-event-msg? evt))
  (check-equal? (key-msg-key evt) #\a)
  (check-false (key-msg-alt evt))
  (check-false (key-msg-ctrl evt))
  (check-true (key-event-msg-shift evt))
  (check-equal? (key-event-msg-event-type evt) 'press))

(test-case "csi-u: key with Alt"
  (define evt (parse-input "\e[97;3u"))
  (check-true (key-event-msg? evt))
  (check-equal? (key-msg-key evt) #\a)
  (check-true (key-msg-alt evt))
  (check-false (key-msg-ctrl evt))
  (check-false (key-event-msg-shift evt)) ;; 3-1=2 = 0b10, bit 1 = alt, bit 0 = no shift
  (check-equal? (key-event-msg-event-type evt) 'press))

(test-case "csi-u: key with Ctrl"
  (define evt (parse-input "\e[97;5u"))
  (check-true (key-event-msg? evt))
  (check-equal? (key-msg-key evt) #\a)
  (check-false (key-msg-alt evt))
  (check-true (key-msg-ctrl evt))
  (check-false (key-event-msg-shift evt))
  (check-equal? (key-event-msg-event-type evt) 'press))

(test-case "csi-u: key with Shift+Ctrl"
  (define evt (parse-input "\e[97;6u"))
  (check-true (key-event-msg? evt))
  (check-equal? (key-msg-key evt) #\a)
  (check-false (key-msg-alt evt))
  (check-true (key-msg-ctrl evt))
  (check-true (key-event-msg-shift evt))
  (check-equal? (key-event-msg-event-type evt) 'press))

(test-case "csi-u: key repeat"
  (define evt (parse-input "\e[97;1:2u"))
  (check-true (key-event-msg? evt))
  (check-equal? (key-msg-key evt) #\a)
  (check-equal? (key-event-msg-event-type evt) 'repeat))

(test-case "csi-u: key release"
  (define evt (parse-input "\e[97;1:3u"))
  (check-true (key-release-msg? evt))
  (check-false (key-msg? evt)) ;; NOT a key-msg subtype
  (check-equal? (key-release-msg-key evt) #\a)
  (check-false (key-release-msg-alt evt))
  (check-false (key-release-msg-ctrl evt))
  (check-false (key-release-msg-shift evt)))

(test-case "csi-u: modified key release (Ctrl+j)"
  (define evt (parse-input "\e[106;5:3u"))
  (check-true (key-release-msg? evt))
  (check-equal? (key-release-msg-key evt) #\j)
  (check-false (key-release-msg-alt evt))
  (check-true (key-release-msg-ctrl evt))
  (check-false (key-release-msg-shift evt)))

(test-case "csi-u: Enter key"
  (define evt (parse-input "\e[13;1u"))
  (check-true (key-event-msg? evt))
  (check-equal? (key-msg-key evt) 'enter))

(test-case "csi-u: Tab key"
  (define evt (parse-input "\e[9;1u"))
  (check-true (key-event-msg? evt))
  (check-equal? (key-msg-key evt) 'tab))

(test-case "csi-u: Escape key"
  (define evt (parse-input "\e[27;1u"))
  (check-true (key-event-msg? evt))
  (check-equal? (key-msg-key evt) 'escape))

(test-case "csi-u: Backspace key"
  (define evt (parse-input "\e[127;1u"))
  (check-true (key-event-msg? evt))
  (check-equal? (key-msg-key evt) 'backspace))

(test-case "csi-u: arrow up"
  (define evt (parse-input "\e[57352;1u"))
  (check-true (key-event-msg? evt))
  (check-equal? (key-msg-key evt) 'up))

(test-case "csi-u: arrow down"
  (define evt (parse-input "\e[57353;1u"))
  (check-true (key-event-msg? evt))
  (check-equal? (key-msg-key evt) 'down))

(test-case "csi-u: arrow right"
  (define evt (parse-input "\e[57354;1u"))
  (check-true (key-event-msg? evt))
  (check-equal? (key-msg-key evt) 'right))

(test-case "csi-u: arrow left"
  (define evt (parse-input "\e[57355;1u"))
  (check-true (key-event-msg? evt))
  (check-equal? (key-msg-key evt) 'left))

(test-case "csi-u: page-down"
  (define evt (parse-input "\e[57364;1u"))
  (check-true (key-event-msg? evt))
  (check-equal? (key-msg-key evt) 'page-down))

(test-case "csi-u: page-up"
  (define evt (parse-input "\e[57363;1u"))
  (check-true (key-event-msg? evt))
  (check-equal? (key-msg-key evt) 'page-up))

(test-case "csi-u: home"
  (define evt (parse-input "\e[57361;1u"))
  (check-true (key-event-msg? evt))
  (check-equal? (key-msg-key evt) 'home))

(test-case "csi-u: end"
  (define evt (parse-input "\e[57362;1u"))
  (check-true (key-event-msg? evt))
  (check-equal? (key-msg-key evt) 'end))

(test-case "csi-u: insert"
  (define evt (parse-input "\e[57359;1u"))
  (check-true (key-event-msg? evt))
  (check-equal? (key-msg-key evt) 'insert))

(test-case "csi-u: delete"
  (define evt (parse-input "\e[57360;1u"))
  (check-true (key-event-msg? evt))
  (check-equal? (key-msg-key evt) 'delete))

(test-case "csi-u: backtab"
  (define evt (parse-input "\e[57358;1u"))
  (check-true (key-event-msg? evt))
  (check-equal? (key-msg-key evt) 'backtab))

(test-case "csi-u: Unicode character (e-acute)"
  (define evt (parse-input "\e[233;1u"))
  (check-true (key-event-msg? evt))
  (check-equal? (key-msg-key evt) #\u00E9))

(test-case "csi-u: query response"
  (define evt (parse-input "\e[?3u"))
  (check-true (kitty-query-response-msg? evt))
  (check-equal? (kitty-query-response-msg-flags evt) 3))

;;; ============================================================
;;; Helper predicates
;;; ============================================================

(test-case "key-press?: plain key-msg is press"
  (check-true (key-press? (key-msg #\a #f #f))))

(test-case "key-press?: key-event-msg press"
  (check-true (key-press? (key-event-msg #\a #f #f #f 'press))))

(test-case "key-press?: key-event-msg repeat is not press"
  (check-false (key-press? (key-event-msg #\a #f #f #f 'repeat))))

(test-case "key-repeat?: key-event-msg repeat"
  (check-true (key-repeat? (key-event-msg #\a #f #f #f 'repeat))))

(test-case "key-repeat?: plain key-msg is not repeat"
  (check-false (key-repeat? (key-msg #\a #f #f))))

(test-case "key-release?: key-release-msg"
  (check-true (key-release? (key-release-msg #\a #f #f #f))))

(test-case "key-release?: plain key-msg is not release"
  (check-false (key-release? (key-msg #\a #f #f))))

(test-case "key-msg-event-type: plain key-msg returns #f"
  (check-false (key-msg-event-type (key-msg #\a #f #f))))

(test-case "key-msg-event-type: key-event-msg returns event type"
  (check-equal? (key-msg-event-type (key-event-msg #\a #f #f #f 'repeat)) 'repeat))

(test-case "key-msg-event-type: key-release-msg returns 'release"
  (check-equal? (key-msg-event-type (key-release-msg #\a #f #f #f)) 'release))

;;; ============================================================
;;; key-event-msg inherits from key-msg
;;; ============================================================

(test-case "key-event-msg is a key-msg"
  (check-true (key-msg? (key-event-msg #\a #f #f #f 'press))))

(test-case "key-release-msg is NOT a key-msg"
  (check-false (key-msg? (key-release-msg #\a #f #f #f))))

(test-case "key-event-msg matches key-msg pattern"
  (define evt (key-event-msg #\j #f #f #f 'press))
  (define matched?
    (match evt
      [(key-msg #\j _ _) #t]
      [_ #f]))
  (check-true matched?))

(test-case "key-release-msg does NOT match key-msg pattern"
  (define evt (key-release-msg #\j #f #f #f))
  (define matched?
    (match evt
      [(key-msg #\j _ _) #t]
      [_ #f]))
  (check-false matched?))

;;; ============================================================
;;; Regression: existing input parsing still works
;;; ============================================================

(test-case "legacy: regular character"
  (define evt (parse-input "a"))
  (check-true (key-msg? evt))
  (check-false (key-event-msg? evt))
  (check-equal? (key-msg-key evt) #\a))

(test-case "legacy: arrow up (ESC[A)"
  (define evt (parse-input "\e[A"))
  (check-true (key-msg? evt))
  (check-equal? (key-msg-key evt) 'up))

(test-case "legacy: arrow down (ESC[B)"
  (define evt (parse-input "\e[B"))
  (check-true (key-msg? evt))
  (check-equal? (key-msg-key evt) 'down))

(test-case "legacy: delete (ESC[3~)"
  (define evt (parse-input "\e[3~"))
  (check-true (key-msg? evt))
  (check-equal? (key-msg-key evt) 'delete))

(test-case "legacy: page-up (ESC[5~)"
  (define evt (parse-input "\e[5~"))
  (check-true (key-msg? evt))
  (check-equal? (key-msg-key evt) 'page-up))

(test-case "legacy: Shift+Up (ESC[1;2A)"
  (define evt (parse-input "\e[1;2A"))
  (check-true (key-msg? evt))
  (check-equal? (key-msg-key evt) 'shift-up))

(test-case "legacy: backspace (DEL 127)"
  (define evt (parse-input "\x7f"))
  (check-true (key-msg? evt))
  (check-equal? (key-msg-key evt) 'backspace))

(test-case "legacy: Ctrl+A"
  (define evt (parse-input "\x01"))
  (check-true (key-msg? evt))
  (check-equal? (key-msg-key evt) #\a)
  (check-true (key-msg-ctrl evt)))

(test-case "legacy: home (ESC[H)"
  (define evt (parse-input "\e[H"))
  (check-true (key-msg? evt))
  (check-equal? (key-msg-key evt) 'home))

(test-case "legacy: end (ESC[F)"
  (define evt (parse-input "\e[F"))
  (check-true (key-msg? evt))
  (check-equal? (key-msg-key evt) 'end))

(test-case "legacy: backtab (ESC[Z)"
  (define evt (parse-input "\e[Z"))
  (check-true (key-msg? evt))
  (check-equal? (key-msg-key evt) 'backtab))

(test-case "legacy: focus in (ESC[I)"
  (define evt (parse-input "\e[I"))
  (check-true (focus-in-msg? evt)))

(test-case "legacy: focus out (ESC[O)"
  (define evt (parse-input "\e[O"))
  (check-true (focus-out-msg? evt)))
