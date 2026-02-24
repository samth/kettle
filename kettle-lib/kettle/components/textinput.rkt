#lang racket/base

;; components/textinput.rkt
;;
;; SPDX-License-Identifier: MIT
;;
;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;; Racket port
;;
;; Text input component - reusable single-line text input

(require racket/string
         racket/list
         "../private/protocol.rkt"
         "../private/style.rkt"
         "../private/image.rkt")

(provide (struct-out textinput)
         make-textinput
         textinput-focus
         textinput-blur
         textinput-set-value
         textinput-reset)

;;; Text input model
(struct textinput
        (value cursor-pos
               placeholder
               prompt
               width
               offset
               char-limit
               focused?
               echo-mode
               echo-char
               validator
               transform
               on-change
               keymap
               kill-ring
               undo-stack
               redo-stack)
  #:transparent
  #:methods gen:tea-model
  [(define (init input)
     (values input #f))
   (define (update input msg)
     (textinput-update input msg))
   (define (view input)
     (textinput-view input))])

(define (make-textinput #:value [value ""]
                        #:placeholder [placeholder ""]
                        #:prompt [prompt "> "]
                        #:width [width 20]
                        #:char-limit [char-limit 0]
                        #:echo-mode [echo-mode 'normal]
                        #:echo-char [echo-char #\*]
                        #:validator [validator #f]
                        #:transform [transform #f]
                        #:on-change [on-change #f]
                        #:keymap [keymap #f])
  (textinput value
             0
             placeholder
             prompt
             width
             0
             char-limit
             #t
             echo-mode
             echo-char
             validator
             transform
             on-change
             keymap
             '()
             '()
             '()))

;;; Internal helpers (all return new textinput)

(define (ti-push-undo input)
  (struct-copy textinput
               input
               [undo-stack
                (cons (cons (textinput-value input) (textinput-cursor-pos input))
                      (textinput-undo-stack input))]
               [redo-stack '()]))

(define (ti-apply-change input new-value [new-cursor #f])
  (define transform (textinput-transform input))
  (define validator (textinput-validator input))
  (define val*
    (if transform
        (transform new-value)
        new-value))
  (if (or (not validator) (validator val*))
      (let* ([input2 (ti-push-undo input)]
             [input3 (struct-copy textinput
                                  input2
                                  [value val*]
                                  [cursor-pos (or new-cursor (textinput-cursor-pos input2))])])
        (define cb (textinput-on-change input3))
        (when cb
          (cb input3 val*))
        input3)
      input))

(define (ti-prev-word-boundary s pos)
  (define i (max 0 (min pos (string-length s))))
  (if (> i 0)
      (let loop ([i (sub1 i)])
        (if (and (> i 0) (char-alphanumeric? (string-ref s i)))
            (loop (sub1 i))
            (if (and (> i 0) (not (char-alphanumeric? (string-ref s i))))
                (add1 i)
                i)))
      #f))

(define (ti-next-word-boundary s pos)
  (define i (max 0 (min pos (string-length s))))
  (define n (string-length s))
  (if (< i n)
      (let loop ([i i])
        (if (and (< i n) (char-alphanumeric? (string-ref s i)))
            (loop (add1 i))
            i))
      #f))

(define (ti-adjust-offset input)
  (define w (max 1 (textinput-width input)))
  (define pos (textinput-cursor-pos input))
  (define off (textinput-offset input))
  (cond
    [(< pos off) (struct-copy textinput input [offset pos])]
    [(>= pos (+ off w)) (struct-copy textinput input [offset (add1 (- pos w))])]
    [else input]))

;;; Component operations

(define (textinput-update input msg)
  (cond
    ;; Custom keymap
    [(and (textinput-keymap input) ((textinput-keymap input) input msg)) (values input #f)]

    ;; Paste message
    [(paste-msg? msg)
     (define text-str (paste-msg-text msg))
     (define value (textinput-value input))
     (define pos (textinput-cursor-pos input))
     (define cl (textinput-char-limit input))
     (define new-val (string-append (substring value 0 pos) text-str (substring value pos)))
     (define trimmed
       (if (and (> cl 0) (> (string-length new-val) cl))
           (substring new-val 0 cl)
           new-val))
     (define new-pos (min (string-length trimmed) (+ pos (string-length text-str))))
     (define input2 (ti-apply-change input trimmed new-pos))
     (values (ti-adjust-offset input2) #f)]

    ;; Key messages
    [(and (key-msg? msg) (textinput-focused? input))
     (define key (key-msg-key msg))
     (define alt? (key-msg-alt msg))
     (define ctrl? (key-msg-ctrl msg))
     (define value (textinput-value input))
     (define pos (textinput-cursor-pos input))

     (cond
       ;; Backspace
       [(and (eq? key 'backspace) (not alt?))
        (if (> pos 0)
            (let* ([input2 (ti-apply-change input
                                            (string-append (substring value 0 (sub1 pos))
                                                           (substring value pos))
                                            (sub1 pos))])
              (values (ti-adjust-offset input2) #f))
            (values input #f))]

       ;; Alt+Backspace (delete word backward)
       [(and (eq? key 'backspace) alt?)
        (define b (ti-prev-word-boundary value pos))
        (if b
            (let* ([input2 (ti-apply-change input
                                            (string-append (substring value 0 b)
                                                           (substring value pos))
                                            b)])
              (values (ti-adjust-offset input2) #f))
            (values input #f))]

       ;; Delete
       [(and (eq? key 'delete) (not alt?))
        (if (< pos (string-length value))
            (let ([input2 (ti-apply-change input
                                           (string-append (substring value 0 pos)
                                                          (substring value (add1 pos)))
                                           pos)])
              (values (ti-adjust-offset input2) #f))
            (values input #f))]

       ;; Alt+d (delete word forward)
       [(and alt? (char? key) (char=? key #\d))
        (define f (ti-next-word-boundary value pos))
        (if f
            (values
             (ti-apply-change input (string-append (substring value 0 pos) (substring value f)) pos)
             #f)
            (values input #f))]

       ;; Left arrow or Ctrl+b
       [(or (eq? key 'left) (and ctrl? (char? key) (char=? key #\b)))
        (if (> pos 0)
            (values (ti-adjust-offset (struct-copy textinput input [cursor-pos (sub1 pos)])) #f)
            (values input #f))]

       ;; Right arrow or Ctrl+f
       [(or (eq? key 'right) (and ctrl? (char? key) (char=? key #\f)))
        (if (< pos (string-length value))
            (values (ti-adjust-offset (struct-copy textinput input [cursor-pos (add1 pos)])) #f)
            (values input #f))]

       ;; Home or Ctrl+a
       [(or (eq? key 'home) (and ctrl? (char? key) (char=? key #\a)))
        (values (ti-adjust-offset (struct-copy textinput input [cursor-pos 0])) #f)]

       ;; End or Ctrl+e
       [(or (eq? key 'end) (and ctrl? (char? key) (char=? key #\e)))
        (values (ti-adjust-offset (struct-copy textinput input [cursor-pos (string-length value)]))
                #f)]

       ;; Ctrl+k (kill to end)
       [(and ctrl? (char? key) (char=? key #\k))
        (define killed (substring value pos))
        (define input2
          (struct-copy textinput input [kill-ring (cons killed (textinput-kill-ring input))]))
        (values (ti-apply-change input2 (substring value 0 pos) pos) #f)]

       ;; Ctrl+u (kill to start)
       [(and ctrl? (char? key) (char=? key #\u))
        (define killed (substring value 0 pos))
        (define input2
          (struct-copy textinput input [kill-ring (cons killed (textinput-kill-ring input))]))
        (values (ti-adjust-offset (ti-apply-change input2 (substring value pos) 0)) #f)]

       ;; Ctrl+w (kill word backward)
       [(and ctrl? (char? key) (char=? key #\w))
        (define b (ti-prev-word-boundary value pos))
        (if b
            (let* ([input2 (struct-copy textinput
                                        input
                                        [kill-ring
                                         (cons (substring value b pos) (textinput-kill-ring input))])]
                   [input3 (ti-apply-change input2
                                            (string-append (substring value 0 b)
                                                           (substring value pos))
                                            b)])
              (values (ti-adjust-offset input3) #f))
            (values input #f))]

       ;; Alt+b (move word left)
       [(and alt? (char? key) (char=? key #\b))
        (define b (ti-prev-word-boundary value pos))
        (if b
            (values (ti-adjust-offset (struct-copy textinput input [cursor-pos b])) #f)
            (values input #f))]

       ;; Alt+f (move word right)
       [(and alt? (char? key) (char=? key #\f))
        (define f (ti-next-word-boundary value pos))
        (if f
            (values (ti-adjust-offset (struct-copy textinput input [cursor-pos f])) #f)
            (values input #f))]

       ;; Ctrl+d (delete at cursor)
       [(and ctrl? (char? key) (char=? key #\d))
        (if (< pos (string-length value))
            (values (ti-apply-change input
                                     (string-append (substring value 0 pos)
                                                    (substring value (add1 pos)))
                                     pos)
                    #f)
            (values input #f))]

       ;; Ctrl+y (yank)
       [(and ctrl? (char? key) (char=? key #\y))
        (define txt
          (and (not (null? (textinput-kill-ring input))) (first (textinput-kill-ring input))))
        (if txt
            (values (ti-apply-change input
                                     (string-append (substring value 0 pos) txt (substring value pos))
                                     (+ pos (string-length txt)))
                    #f)
            (values input #f))]

       ;; Ctrl+z (undo)
       [(and ctrl? (char? key) (char=? key #\z))
        (define snap
          (and (not (null? (textinput-undo-stack input))) (first (textinput-undo-stack input))))
        (if snap
            (values (ti-adjust-offset (struct-copy textinput
                                                   input
                                                   [undo-stack (rest (textinput-undo-stack input))]
                                                   [redo-stack
                                                    (cons (cons (textinput-value input)
                                                                (textinput-cursor-pos input))
                                                          (textinput-redo-stack input))]
                                                   [value (car snap)]
                                                   [cursor-pos (cdr snap)]))
                    #f)
            (values input #f))]

       ;; Alt+z (redo)
       [(and alt? (char? key) (char=? key #\z))
        (define snap
          (and (not (null? (textinput-redo-stack input))) (first (textinput-redo-stack input))))
        (if snap
            (values (ti-adjust-offset (struct-copy textinput
                                                   input
                                                   [redo-stack (rest (textinput-redo-stack input))]
                                                   [undo-stack
                                                    (cons (cons (textinput-value input)
                                                                (textinput-cursor-pos input))
                                                          (textinput-undo-stack input))]
                                                   [value (car snap)]
                                                   [cursor-pos (cdr snap)]))
                    #f)
            (values input #f))]

       ;; Regular character input
       [(and (char? key) (char-graphic? key))
        (define cl (textinput-char-limit input))
        (define allowed (or (zero? cl) (< (string-length value) cl)))
        (if allowed
            (let ([input2 (ti-apply-change
                           input
                           (string-append (substring value 0 pos) (string key) (substring value pos))
                           (add1 pos))])
              (values (ti-adjust-offset input2) #f))
            (values input #f))]

       [else (values input #f)])]

    [else (values input #f)]))

(define (textinput-view input)
  (define value (textinput-value input))
  (define cursor-pos (textinput-cursor-pos input))
  (define placeholder (textinput-placeholder input))
  (define prompt-str (textinput-prompt input))
  (define focused? (textinput-focused? input))
  (define w (max 1 (textinput-width input)))
  (define is-placeholder?
    (and (zero? (string-length value)) (not (zero? (string-length placeholder)))))
  (define base (if is-placeholder? placeholder value))
  (define masked
    (if (and (eq? (textinput-echo-mode input) 'password)
             (> (string-length base) 0)
             (not is-placeholder?))
        (make-string (string-length base) (textinput-echo-char input))
        base))

  (define clamped-cursor (min (string-length masked) cursor-pos))
  (define input2 (ti-adjust-offset input))
  (define off (textinput-offset input2))
  (define start off)
  (define end (min (string-length masked) (+ start w)))
  (define visible (substring masked start end))
  (define cursor-in-window (- clamped-cursor start))

  (define reverse-style (make-style #:reverse #t))
  (define placeholder-style (make-style #:foreground fg-bright-black))

  ;; Build the content image with cursor
  (define content-img
    (if (and focused? (>= cursor-in-window 0) (<= cursor-in-window (string-length visible)))
        (let* ([before (substring visible 0 cursor-in-window)]
               [cursor-char (if (< cursor-in-window (string-length visible))
                                (string (string-ref visible cursor-in-window))
                                " ")]
               [after (if (< cursor-in-window (string-length visible))
                          (substring visible (add1 cursor-in-window))
                          "")])
          (hcat 'top (text before) (styled reverse-style (text cursor-char)) (text after)))
        (text visible)))

  ;; Style placeholder
  (define display-img
    (if is-placeholder?
        (styled placeholder-style content-img)
        content-img))

  (hcat 'top (text prompt-str) display-img))

;;; Public operations (return new textinput)

(define (textinput-focus input)
  (struct-copy textinput input [focused? #t]))

(define (textinput-blur input)
  (struct-copy textinput input [focused? #f]))

(define (textinput-set-value input value)
  (struct-copy textinput input [value value] [cursor-pos (string-length value)]))

(define (textinput-reset input)
  (struct-copy textinput input [value ""] [cursor-pos 0]))

;;; Internal helpers

(define (char-graphic? ch)
  (and (char? ch) (>= (char->integer ch) 32) (not (= (char->integer ch) 127))))

(define (char-alphanumeric? ch)
  (or (char-alphabetic? ch) (char-numeric? ch)))
