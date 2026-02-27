#lang racket/base

;; textinput.rkt -- Demonstrates manual text input handling.
;; Adapted from cl-tuition examples/textinput.lisp
;; https://github.com/atgreen/cl-tuition/blob/master/examples/textinput.lisp
;;
;; Key differences from cl-tuition:
;; - cl-tuition manually manages a text buffer, cursor position, and render logic.
;; - Kettle provides a ready-made textinput component (see textinput-form.rkt),
;;   but this example shows how to build basic input from scratch using raw
;;   key-msg pattern matching, mirroring the cl-tuition approach.
;; - Pattern matching on key symbols ('backspace, 'left, 'right, 'home, 'end)
;;   replaces cl-tuition's (cond ((eq key :backspace) ...) dispatching.
;;
;; Type text, enter to "submit", q at empty to quit.

(require racket/match
         racket/string
         kettle/run
         kettle/image
         kettle/style
         kettle/program)

(struct input-state (buffer cursor submitted) #:transparent)

(define (input-on-key st km)
  (match km
    ;; After submission, any key quits
    [(key-msg _ _ _)
     #:when (input-state-submitted st)
     (cmd st (quit-cmd))]
    ;; Submit on enter
    [(key-msg 'enter _ _)
     (struct-copy input-state st [submitted #t])]
    ;; Quit on ctrl-c
    [(key-msg _ _ #t)
     (cmd st (quit-cmd))]
    ;; Quit on q when buffer is empty
    [(key-msg #\q _ _)
     #:when (string=? (input-state-buffer st) "")
     (cmd st (quit-cmd))]
    ;; Backspace
    [(key-msg 'backspace _ _)
     (define buf (input-state-buffer st))
     (define pos (input-state-cursor st))
     (if (> pos 0)
         (struct-copy input-state st
                      [buffer (string-append (substring buf 0 (sub1 pos))
                                             (substring buf pos))]
                      [cursor (sub1 pos)])
         st)]
    ;; Delete
    [(key-msg 'delete _ _)
     (define buf (input-state-buffer st))
     (define pos (input-state-cursor st))
     (if (< pos (string-length buf))
         (struct-copy input-state st
                      [buffer (string-append (substring buf 0 pos)
                                             (substring buf (add1 pos)))])
         st)]
    ;; Left arrow
    [(key-msg 'left _ _)
     (define pos (input-state-cursor st))
     (if (> pos 0)
         (struct-copy input-state st [cursor (sub1 pos)])
         st)]
    ;; Right arrow
    [(key-msg 'right _ _)
     (define pos (input-state-cursor st))
     (if (< pos (string-length (input-state-buffer st)))
         (struct-copy input-state st [cursor (add1 pos)])
         st)]
    ;; Home
    [(key-msg 'home _ _)
     (struct-copy input-state st [cursor 0])]
    ;; End
    [(key-msg 'end _ _)
     (struct-copy input-state st [cursor (string-length (input-state-buffer st))])]
    ;; Normal character input
    [(key-msg (? char? ch) _ _)
     (define buf (input-state-buffer st))
     (define pos (input-state-cursor st))
     (struct-copy input-state st
                  [buffer (string-append (substring buf 0 pos)
                                         (string ch)
                                         (substring buf pos))]
                  [cursor (add1 pos)])]
    [_ st]))

(define (input-view st)
  (cond
    [(input-state-submitted st)
     (vcat 'left
           ""
           (hcat 'top (text "You typed: ")
                      (styled (make-style #:foreground fg-bright-green #:bold #t)
                              (input-state-buffer st)))
           ""
           "Press any key to quit.")]
    [else
     (define buf (input-state-buffer st))
     (define pos (input-state-cursor st))
     ;; Display cursor position with underscore
     (define before (substring buf 0 pos))
     (define cursor-ch (if (< pos (string-length buf))
                           (string (string-ref buf pos))
                           " "))
     (define after (if (< pos (string-length buf))
                       (substring buf (add1 pos))
                       ""))
     (vcat 'left
           ""
           "  Type something:"
           ""
           (hcat 'top
                 (text "  > ")
                 (text before)
                 (styled (make-style #:reverse #t) cursor-ch)
                 (text after))
           ""
           (styled (make-style #:foreground fg-bright-black)
                   "  enter submit • arrows/home/end move • backspace delete • q quit"))]))

(module+ main
  (run (input-state "" 0 #f)
       #:on-key input-on-key
       #:to-view input-view))
