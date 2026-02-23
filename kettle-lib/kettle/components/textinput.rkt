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
         "../private/style.rkt")

(provide (struct-out textinput)
         make-textinput
         textinput-init
         textinput-update
         textinput-view
         textinput-focus!
         textinput-blur!
         textinput-set-value!
         textinput-reset!)

;;; Text input model
(struct textinput
  ([value #:mutable]
   [cursor-pos #:mutable]
   placeholder
   prompt
   width
   [offset #:mutable]
   char-limit
   [focused? #:mutable]
   echo-mode
   echo-char
   validator
   transform
   on-change
   keymap
   [kill-ring #:mutable]
   [undo-stack #:mutable]
   [redo-stack #:mutable])
  #:transparent)

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
  (textinput value 0 placeholder prompt width 0 char-limit
             #t echo-mode echo-char validator transform on-change keymap
             '() '() '()))

;;; Internal helpers

(define (ti-push-undo! input)
  (set-textinput-undo-stack!
   input
   (cons (cons (textinput-value input) (textinput-cursor-pos input))
         (textinput-undo-stack input)))
  (set-textinput-redo-stack! input '()))

(define (ti-apply-change! input new-value [new-cursor #f])
  (define transform (textinput-transform input))
  (define validator (textinput-validator input))
  (define val* (if transform (transform new-value) new-value))
  (when (or (not validator) (validator val*))
    (ti-push-undo! input)
    (set-textinput-value! input val*)
    (when new-cursor (set-textinput-cursor-pos! input new-cursor))
    (define cb (textinput-on-change input))
    (when cb (cb input val*)))
  input)

(define (ti-prev-word-boundary s pos)
  (define i (max 0 (min pos (string-length s))))
  (if (> i 0)
      (begin
        (set! i (sub1 i))
        (let loop ()
          (when (and (> i 0) (char-alphanumeric? (string-ref s i)))
            (set! i (sub1 i))
            (loop)))
        (when (and (> i 0) (not (char-alphanumeric? (string-ref s i))))
          (set! i (add1 i)))
        i)
      #f))

(define (ti-next-word-boundary s pos)
  (define i (max 0 (min pos (string-length s))))
  (define n (string-length s))
  (if (< i n)
      (begin
        (let loop ()
          (when (and (< i n) (char-alphanumeric? (string-ref s i)))
            (set! i (add1 i))
            (loop)))
        i)
      #f))

(define (ti-adjust-offset! input)
  (define w (max 1 (textinput-width input)))
  (define pos (textinput-cursor-pos input))
  (define off (textinput-offset input))
  (cond
    [(< pos off) (set-textinput-offset! input pos)]
    [(>= pos (+ off w)) (set-textinput-offset! input (add1 (- pos w)))])
  input)

;;; Component operations

(define (textinput-init input)
  (values input #f))

(define (textinput-update input msg)
  (cond
    ;; Custom keymap
    [(and (textinput-keymap input) ((textinput-keymap input) input msg))
     (values input #f)]

    ;; Paste message
    [(paste-msg? msg)
     (define text (paste-msg-text msg))
     (define value (textinput-value input))
     (define pos (textinput-cursor-pos input))
     (define cl (textinput-char-limit input))
     (define new-val (string-append (substring value 0 pos) text (substring value pos)))
     (define trimmed (if (and (> cl 0) (> (string-length new-val) cl))
                         (substring new-val 0 cl)
                         new-val))
     (define new-pos (min (string-length trimmed) (+ pos (string-length text))))
     (ti-apply-change! input trimmed new-pos)
     (ti-adjust-offset! input)
     (values input #f)]

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
        (when (> pos 0)
          (ti-apply-change! input
                            (string-append (substring value 0 (sub1 pos))
                                           (substring value pos))
                            (sub1 pos)))
        (ti-adjust-offset! input)
        (values input #f)]

       ;; Alt+Backspace (delete word backward)
       [(and (eq? key 'backspace) alt?)
        (define b (ti-prev-word-boundary value pos))
        (when b
          (ti-apply-change! input
                            (string-append (substring value 0 b) (substring value pos))
                            b)
          (ti-adjust-offset! input))
        (values input #f)]

       ;; Delete
       [(and (eq? key 'delete) (not alt?))
        (when (< pos (string-length value))
          (ti-apply-change! input
                            (string-append (substring value 0 pos)
                                           (substring value (add1 pos)))
                            pos))
        (ti-adjust-offset! input)
        (values input #f)]

       ;; Alt+d (delete word forward)
       [(and alt? (char? key) (char=? key #\d))
        (define f (ti-next-word-boundary value pos))
        (when f
          (ti-apply-change! input
                            (string-append (substring value 0 pos) (substring value f))
                            pos))
        (values input #f)]

       ;; Left arrow or Ctrl+b
       [(or (eq? key 'left) (and ctrl? (char? key) (char=? key #\b)))
        (when (> pos 0)
          (set-textinput-cursor-pos! input (sub1 pos))
          (ti-adjust-offset! input))
        (values input #f)]

       ;; Right arrow or Ctrl+f
       [(or (eq? key 'right) (and ctrl? (char? key) (char=? key #\f)))
        (when (< pos (string-length value))
          (set-textinput-cursor-pos! input (add1 pos))
          (ti-adjust-offset! input))
        (values input #f)]

       ;; Home or Ctrl+a
       [(or (eq? key 'home) (and ctrl? (char? key) (char=? key #\a)))
        (set-textinput-cursor-pos! input 0)
        (ti-adjust-offset! input)
        (values input #f)]

       ;; End or Ctrl+e
       [(or (eq? key 'end) (and ctrl? (char? key) (char=? key #\e)))
        (set-textinput-cursor-pos! input (string-length value))
        (ti-adjust-offset! input)
        (values input #f)]

       ;; Ctrl+k (kill to end)
       [(and ctrl? (char? key) (char=? key #\k))
        (define killed (substring value pos))
        (set-textinput-kill-ring! input (cons killed (textinput-kill-ring input)))
        (ti-apply-change! input (substring value 0 pos) pos)
        (values input #f)]

       ;; Ctrl+u (kill to start)
       [(and ctrl? (char? key) (char=? key #\u))
        (define killed (substring value 0 pos))
        (set-textinput-kill-ring! input (cons killed (textinput-kill-ring input)))
        (ti-apply-change! input (substring value pos) 0)
        (ti-adjust-offset! input)
        (values input #f)]

       ;; Ctrl+w (kill word backward)
       [(and ctrl? (char? key) (char=? key #\w))
        (define b (ti-prev-word-boundary value pos))
        (when b
          (set-textinput-kill-ring! input
                                    (cons (substring value b pos)
                                          (textinput-kill-ring input)))
          (ti-apply-change! input
                            (string-append (substring value 0 b) (substring value pos))
                            b)
          (ti-adjust-offset! input))
        (values input #f)]

       ;; Alt+b (move word left)
       [(and alt? (char? key) (char=? key #\b))
        (define b (ti-prev-word-boundary value pos))
        (when b
          (set-textinput-cursor-pos! input b)
          (ti-adjust-offset! input))
        (values input #f)]

       ;; Alt+f (move word right)
       [(and alt? (char? key) (char=? key #\f))
        (define f (ti-next-word-boundary value pos))
        (when f
          (set-textinput-cursor-pos! input f)
          (ti-adjust-offset! input))
        (values input #f)]

       ;; Ctrl+d (delete at cursor)
       [(and ctrl? (char? key) (char=? key #\d))
        (when (< pos (string-length value))
          (ti-apply-change! input
                            (string-append (substring value 0 pos)
                                           (substring value (add1 pos)))
                            pos))
        (values input #f)]

       ;; Ctrl+y (yank)
       [(and ctrl? (char? key) (char=? key #\y))
        (define text (and (not (null? (textinput-kill-ring input)))
                          (first (textinput-kill-ring input))))
        (when text
          (ti-apply-change! input
                            (string-append (substring value 0 pos) text (substring value pos))
                            (+ pos (string-length text))))
        (values input #f)]

       ;; Ctrl+z (undo)
       [(and ctrl? (char? key) (char=? key #\z))
        (define snap (and (not (null? (textinput-undo-stack input)))
                          (first (textinput-undo-stack input))))
        (when snap
          (set-textinput-undo-stack! input (rest (textinput-undo-stack input)))
          (set-textinput-redo-stack!
           input
           (cons (cons (textinput-value input) (textinput-cursor-pos input))
                 (textinput-redo-stack input)))
          (set-textinput-value! input (car snap))
          (set-textinput-cursor-pos! input (cdr snap))
          (ti-adjust-offset! input))
        (values input #f)]

       ;; Alt+z (redo)
       [(and alt? (char? key) (char=? key #\z))
        (define snap (and (not (null? (textinput-redo-stack input)))
                          (first (textinput-redo-stack input))))
        (when snap
          (set-textinput-redo-stack! input (rest (textinput-redo-stack input)))
          (set-textinput-undo-stack!
           input
           (cons (cons (textinput-value input) (textinput-cursor-pos input))
                 (textinput-undo-stack input)))
          (set-textinput-value! input (car snap))
          (set-textinput-cursor-pos! input (cdr snap))
          (ti-adjust-offset! input))
        (values input #f)]

       ;; Regular character input
       [(and (char? key) (char-graphic? key))
        (define cl (textinput-char-limit input))
        (define allowed (or (zero? cl) (< (string-length value) cl)))
        (when allowed
          (ti-apply-change! input
                            (string-append (substring value 0 pos)
                                           (string key)
                                           (substring value pos))
                            (add1 pos))
          (ti-adjust-offset! input))
        (values input #f)]

       [else (values input #f)])]

    [else (values input #f)]))

(define (textinput-view input)
  (define value (textinput-value input))
  (define cursor-pos (textinput-cursor-pos input))
  (define placeholder (textinput-placeholder input))
  (define prompt (textinput-prompt input))
  (define focused? (textinput-focused? input))
  (define w (max 1 (textinput-width input)))
  (define is-placeholder? (and (zero? (string-length value))
                                (not (zero? (string-length placeholder)))))
  (define base (if is-placeholder? placeholder value))
  (define masked
    (if (and (eq? (textinput-echo-mode input) 'password)
             (> (string-length base) 0)
             (not is-placeholder?))
        (make-string (string-length base) (textinput-echo-char input))
        base))

  (define clamped-cursor (min (string-length masked) cursor-pos))
  (ti-adjust-offset! input)
  (define off (textinput-offset input))
  (define start off)
  (define end (min (string-length masked) (+ start w)))
  (define visible (substring masked start end))
  (define cursor-in-window (- clamped-cursor start))

  ;; Apply reverse video to character at cursor
  (define display-plain
    (if (and focused? (>= cursor-in-window 0) (<= cursor-in-window (string-length visible)))
        (let* ([cursor-char (if (< cursor-in-window (string-length visible))
                                (string (string-ref visible cursor-in-window))
                                " ")]
               [reversed-char (format "\e[7m~a\e[27m" cursor-char)])
          (string-append
           (substring visible 0 cursor-in-window)
           reversed-char
           (if (< cursor-in-window (string-length visible))
               (substring visible (add1 cursor-in-window))
               "")))
        visible))

  ;; Style placeholder in gray
  (define display-text
    (if is-placeholder?
        (render-styled (make-style #:foreground fg-bright-black) display-plain)
        display-plain))

  (string-append prompt display-text))

;;; Helper functions

(define (textinput-focus! input)
  (set-textinput-focused?! input #t))

(define (textinput-blur! input)
  (set-textinput-focused?! input #f))

(define (textinput-set-value! input value)
  (set-textinput-value! input value)
  (set-textinput-cursor-pos! input (string-length value)))

(define (textinput-reset! input)
  (set-textinput-value! input "")
  (set-textinput-cursor-pos! input 0))

;;; Internal helpers

(define (char-graphic? ch)
  (and (char? ch)
       (>= (char->integer ch) 32)
       (not (= (char->integer ch) 127))))

(define (char-alphanumeric? ch)
  (or (char-alphabetic? ch) (char-numeric? ch)))
