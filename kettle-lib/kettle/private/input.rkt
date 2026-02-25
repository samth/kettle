#lang racket/base

;; input.rkt
;;
;; SPDX-License-Identifier: MIT
;;
;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;; Racket port
;;
;; Input handling and key event processing

(require racket/match
         racket/list
         racket/string
         "protocol.rkt")

(provide read-key
         read-all-available-events
         current-input-stream)

;; The stream to read input from.
(define current-input-stream (make-parameter (current-input-port)))

(define (read-char-no-hang [port (current-input-stream)])
  "Read a character if available, otherwise return #f."
  ;; Use byte-ready? instead of char-ready? because char-ready? can
  ;; return #f on raw /dev/tty file ports even when data is available.
  (if (byte-ready? port)
      (let ([c (read-char port)])
        (if (eof-object? c) #f c))
      #f))

(define (read-char-blocking [port (current-input-stream)])
  "Read a character, blocking until one is available."
  (let ([c (read-char port)])
    (if (eof-object? c) #f c)))

(define (ctrl-char-to-key ch)
  "Convert a control character to its key representation."
  (define code (char->integer ch))
  (case code
    [(8) 'backspace]
    [(9) 'tab]
    [(10) 'enter]
    [(13) 'enter]
    [else (integer->char (+ code 96))]))

(define (read-key)
  "Read a single key from input and return a key-msg or mouse event.
   Returns #f if no input is available."
  (define port (current-input-stream))
  (define ch (read-char-no-hang port))
  (and ch
       (let ([code (char->integer ch)])
         (cond
           ;; Escape sequences
           [(char=? ch #\u001B)
            (parse-escape-start port)]
           ;; ASCII DEL (127) is commonly sent for backspace
           [(= code 127)
            (key-msg 'backspace #f #f)]
           ;; Control characters
           [(< code 32)
            (key-msg (ctrl-char-to-key ch) #f #t)]
           ;; Regular characters
           [else
            (key-msg ch #f #f)]))))

(define (parse-escape-start port)
  "Parse an escape sequence after reading ESC."
  ;; Wait briefly for next character
  (define tries 0)
  (let loop ()
    (when (and (not (byte-ready? port)) (< tries 8))
      (sleep 0.005)
      (set! tries (+ tries 1))
      (loop)))
  (if (byte-ready? port)
      (let ([next (read-char port)])
        (if (eof-object? next)
            (key-msg 'escape #f #f)
            (parse-escape-sequence next port)))
      ;; No following char - just escape key
      (key-msg 'escape #f #f)))

(define (parse-escape-sequence ch port)
  "Parse an escape sequence starting after ESC."
  (cond
    ;; CSI sequences (ESC [)
    [(char=? ch #\[)
     (parse-csi-sequence port)]
    ;; SS3 sequences (ESC O)
    [(char=? ch #\O)
     (parse-ss3-sequence port)]
    ;; Alt+key
    [(and (char? ch) (char-graphic? ch))
     (key-msg ch #t #f)]
    ;; ESC + DEL
    [(and (char? ch) (= (char->integer ch) 127))
     (key-msg 'backspace #t #f)]
    ;; Unknown
    [else
     (key-msg 'escape #f #f)]))

(define (parse-csi-sequence port)
  "Parse a CSI (Control Sequence Introducer) sequence (ESC [)."
  (define ch (read-char-blocking port))
  (cond
    [(not ch)
     (key-msg 'unknown #f #f)]

    ;; Mouse tracking: ESC [ < ...
    [(char=? ch #\<)
     (parse-mouse-sequence port)]

    ;; Kitty keyboard protocol query response: ESC [ ? flags u
    [(char=? ch #\?)
     (parse-kitty-query-response port)]

    ;; Focus events: ESC [ I (focus in), ESC [ O (focus out)
    [(char=? ch #\I)
     (focus-in-msg)]
    [(char=? ch #\O)
     (focus-out-msg)]

    ;; Navigation keys: arrows, Home, End, Backtab
    [(char=? ch #\A) (key-msg 'up #f #f)]
    [(char=? ch #\B) (key-msg 'down #f #f)]
    [(char=? ch #\C) (key-msg 'right #f #f)]
    [(char=? ch #\D) (key-msg 'left #f #f)]
    [(char=? ch #\H) (key-msg 'home #f #f)]
    [(char=? ch #\F) (key-msg 'end #f #f)]
    [(char=? ch #\Z) (key-msg 'backtab #f #f)]

    ;; Digits: e.g., 3~ for Delete, or 1;2A for Shift+Up
    [(char-numeric? ch)
     (parse-csi-numeric ch port)]

    [else
     ;; Consume remaining chars
     (let loop ()
       (when (byte-ready? port)
         (read-char-no-hang port)
         (loop)))
     (key-msg 'unknown #f #f)]))

(define (parse-csi-numeric first-digit port)
  "Parse CSI sequence with numeric parameters.
   Handles semicolons as parameter separators and colons as sub-parameter
   separators (used by Kitty keyboard protocol for event types)."
  (define params '())       ;; list of numbers
  (define separators '())   ;; list of separator chars (#\; or #\:) between params
  (define current-num (- (char->integer first-digit) (char->integer #\0)))
  (define term #f)

  ;; Read digits, semicolons, colons, until terminator
  (let loop ()
    (define c (read-char-blocking port))
    (when c
      (cond
        [(char-numeric? c)
         (set! current-num (+ (* current-num 10) (- (char->integer c) (char->integer #\0))))
         (loop)]
        [(or (char=? c #\;) (char=? c #\:))
         (set! params (append params (list current-num)))
         (set! separators (append separators (list c)))
         (set! current-num 0)
         (loop)]
        [else
         (set! params (append params (list current-num)))
         (set! term c)])))

  (cond
    ;; CSI u: Kitty keyboard protocol key event
    [(and term (char=? term #\u))
     (parse-csi-u-params params separators)]

    ;; Modified arrow keys: ESC[1;NA
    [(and term
          (memv term '(#\A #\B #\C #\D #\H #\F))
          (>= (length params) 2)
          (= (first params) 1))
     (define modifier (second params))
     (define shift? (bitwise-bit-set? (sub1 modifier) 0))
     (define alt? (bitwise-bit-set? (sub1 modifier) 1))
     (define ctrl? (bitwise-bit-set? (sub1 modifier) 2))
     (define base-key
       (case term
         [(#\A) 'up] [(#\B) 'down] [(#\C) 'right] [(#\D) 'left]
         [(#\H) 'home] [(#\F) 'end]))
     (define final-key
       (if shift?
           (case base-key
             [(up) 'shift-up] [(down) 'shift-down]
             [(left) 'shift-left] [(right) 'shift-right]
             [(home) 'shift-home] [(end) 'shift-end]
             [else base-key])
           base-key))
     (key-msg final-key alt? ctrl?)]

    ;; Special keys with ~ terminator
    [(and term (char=? term #\~))
     (define num (first params))
     (case num
       [(2) (key-msg 'insert #f #f)]
       [(3) (key-msg 'delete #f #f)]
       [(5) (key-msg 'page-up #f #f)]
       [(6) (key-msg 'page-down #f #f)]
       [(200) (parse-bracketed-paste port)]
       [else (key-msg 'unknown #f #f)])]

    [else (key-msg 'unknown #f #f)]))

(define (parse-mouse-sequence port)
  "Parse SGR mouse tracking sequence: ESC [ < Cb ; Cx ; Cy (M or m)"
  (define params (make-vector 3 0))
  (define param-idx 0)
  (define current-num 0)

  (let loop ()
    (define ch (read-char-blocking port))
    (when ch
      (cond
        [(char-numeric? ch)
         (set! current-num (+ (* current-num 10) (- (char->integer ch) (char->integer #\0))))
         (loop)]
        [(char=? ch #\;)
         (vector-set! params param-idx current-num)
         (set! param-idx (add1 param-idx))
         (set! current-num 0)
         (loop)]
        ;; M = press, m = release
        [(or (char=? ch #\M) (char=? ch #\m))
         (vector-set! params param-idx current-num)
         (define cb (vector-ref params 0))
         (define x (add1 (vector-ref params 1)))   ; 1-based
         (define y (add1 (vector-ref params 2)))   ; 1-based
         (define is-press? (char=? ch #\M))
         (define base-button (bitwise-and cb 3))
         (define is-motion? (bitwise-bit-set? cb 5))
         (define button
           (case base-button
             [(0) 'left] [(1) 'middle] [(2) 'right] [else #f]))
         (define shift? (bitwise-bit-set? cb 2))
         (define alt? (bitwise-bit-set? cb 3))
         (define ctrl? (bitwise-bit-set? cb 4))

         (cond
           ;; Scroll events
           [(= cb 64)
            (mouse-scroll-event x y shift? alt? ctrl? 'up 1)]
           [(= cb 65)
            (mouse-scroll-event x y shift? alt? ctrl? 'down 1)]
           ;; Drag (motion with button)
           [(and is-motion? button)
            (mouse-drag-event x y shift? alt? ctrl? button)]
           ;; Move (motion without button)
           [is-motion?
            (mouse-move-event x y shift? alt? ctrl?)]
           ;; Press/release
           [is-press?
            (mouse-press-event x y shift? alt? ctrl? button)]
           [else
            (mouse-release-event x y shift? alt? ctrl? button)])]
        [else #f])))
  ;; fallback if we didn't parse
  )

(define (parse-bracketed-paste port)
  "Read everything until ESC [ 201 ~ and return a paste message."
  (define out (open-output-string))
  (let loop ()
    (define ch (read-char-blocking port))
    (when ch
      (cond
        [(char=? ch #\u001B)
         (define c1 (read-char-blocking port))
         (cond
           [(and c1 (char=? c1 #\[))
            ;; Read digits and terminator
            (define digits '())
            (define term-char #f)
            (let dloop ()
              (define d (read-char-blocking port))
              (when d
                (cond
                  [(char-numeric? d)
                   (set! digits (append digits (list d)))
                   (dloop)]
                  [else
                   (set! term-char d)])))
            (when (and term-char (char=? term-char #\~))
              (define num-str (list->string digits))
              (define num (string->number num-str))
              (when (and num (= num 201))
                ;; End of paste
                (define text (get-output-string out))
                (paste-msg text)))]
           [else
            (write-char ch out)
            (when c1 (write-char c1 out))
            (loop)])]
        [else
         (write-char ch out)
         (loop)]))))

(define (parse-ss3-sequence port)
  "Parse an SS3 sequence (ESC O <char>)."
  (define ch (read-char-blocking port))
  (if ch
      (case ch
        [(#\A) (key-msg 'up #f #f)]
        [(#\B) (key-msg 'down #f #f)]
        [(#\C) (key-msg 'right #f #f)]
        [(#\D) (key-msg 'left #f #f)]
        [(#\F) (key-msg 'end #f #f)]
        [(#\H) (key-msg 'home #f #f)]
        [else (key-msg 'unknown #f #f)])
      (key-msg 'unknown #f #f)))

;;; Kitty keyboard protocol parsing

;; Map Kitty protocol keycodes to Kettle key symbols/chars.
(define (csi-u-keycode->key code)
  (cond
    [(and (>= code 32) (<= code 126)) (integer->char code)]
    [(= code 27) 'escape]
    [(= code 13) 'enter]
    [(= code 9) 'tab]
    [(= code 127) 'backspace]
    [(= code 57358) 'backtab]
    [(= code 57359) 'insert]
    [(= code 57360) 'delete]
    [(= code 57361) 'home]
    [(= code 57362) 'end]
    [(= code 57363) 'page-up]
    [(= code 57364) 'page-down]
    [(= code 57352) 'up]
    [(= code 57353) 'down]
    [(= code 57354) 'right]
    [(= code 57355) 'left]
    ;; Valid Unicode codepoint
    [(and (> code 0) (<= code #x10FFFF)) (integer->char code)]
    [else 'unknown]))

;; Parse CSI u parameters into key-event-msg or key-release-msg.
;; params: list of numbers from the numeric parser
;; separators: list of separator chars (#\; or #\:) between params
;; Format: ESC [ keycode ; modifiers : event_type u
(define (parse-csi-u-params params separators)
  (define keycode (if (pair? params) (first params) 0))
  (define modifiers 1)  ;; default: no modifiers (value 1)
  (define event-type-num #f)

  ;; Extract modifiers and event type.
  ;; Standard format: keycode ; modifiers : event_type
  ;; params = (keycode modifiers event_type) with separators = (#\; #\:)
  ;; Or: keycode ; modifiers with separators = (#\;) -- no event type
  ;; Or: keycode alone -- no modifiers or event type
  (when (> (length params) 1)
    ;; Second param is modifiers (separated by ;)
    (set! modifiers (second params))
    ;; If there's a third param and the second separator is :, it's event type
    (when (and (> (length params) 2)
               (> (length separators) 1)
               (char=? (second separators) #\:))
      (set! event-type-num (third params))))

  (define mod-bits (sub1 (max 1 modifiers)))
  (define shift? (bitwise-bit-set? mod-bits 0))
  (define alt?   (bitwise-bit-set? mod-bits 1))
  (define ctrl?  (bitwise-bit-set? mod-bits 2))

  (define key (csi-u-keycode->key keycode))

  (case event-type-num
    [(3) (key-release-msg key alt? ctrl? shift?)]
    [(2) (key-event-msg key alt? ctrl? shift? 'repeat)]
    [else (key-event-msg key alt? ctrl? shift? 'press)]))

;; Parse Kitty keyboard protocol query response: ESC [ ? flags u
(define (parse-kitty-query-response port)
  (define num 0)
  (let loop ()
    (define c (read-char-blocking port))
    (cond
      [(not c) (key-msg 'unknown #f #f)]
      [(char-numeric? c)
       (set! num (+ (* num 10) (- (char->integer c) (char->integer #\0))))
       (loop)]
      [(char=? c #\u)
       (kitty-query-response-msg num)]
      [else
       (key-msg 'unknown #f #f)])))

(define (read-all-available-events)
  "Read all available input events and return them as a list."
  (let loop ([events '()])
    (define evt (read-key))
    (if evt
        (loop (append events (list evt)))
        events)))
