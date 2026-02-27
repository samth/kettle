#lang racket/base

;; chat.rkt -- Chat room demo with textarea and viewport.
;; Adapted from cl-tuition examples/chat.lisp
;; https://github.com/atgreen/cl-tuition/blob/master/examples/chat.lisp
;;
;; Key differences from cl-tuition:
;; - Uses Kettle's textarea and viewport components (gen:kettle-model) with
;;   extract-update-result delegation, vs. cl-tuition's CLOS slot mutation.
;; - Kettle's viewport-set-content and viewport-goto-bottom return new structs;
;;   cl-tuition mutates the viewport in place.
;; - Window-size-msg handling rebuilds textarea/viewport with struct-copy instead
;;   of mutating width/height slots.
;; - Message history uses Racket lists with cons/reverse rather than CL push.
;;
;; Type a message and press Enter to send. Ctrl-C or Escape to quit.

(require racket/match
         racket/string
         racket/format
         kettle/program
         kettle/image
         kettle/style
         (prefix-in ta: kettle/components/textarea)
         (prefix-in vp: kettle/components/viewport))

(struct chat (viewport textarea messages width height)
  #:transparent)

(define (make-chat)
  (chat
   (vp:make-viewport #:width 60 #:height 10
                     #:content "Welcome to the chat room!\nType a message and press Enter to send.")
   (ta:textarea-focus
    (ta:make-textarea #:width 60 #:height 3 #:placeholder "Send a message..."))
   '()
   80
   24))

(define sender-style (make-style #:foreground fg-magenta #:bold #t))

(define (chat-update c msg)
  (match msg
    ;; Quit
    [(key-msg _ _ #t) (cmd c (quit-cmd))]
    [(key-msg 'escape _ _) (cmd c (quit-cmd))]
    ;; Enter: send message
    [(key-msg 'enter _ _)
     (define text-val (ta:textarea-value (chat-textarea c)))
     (cond
       [(or (not text-val) (string=? (string-trim text-val) ""))
        c]
       [else
        (define new-msgs (append (chat-messages c)
                                 (list (format "You: ~a" text-val))))
        (define new-vp
          (vp:viewport-goto-bottom
           (vp:viewport-set-content
            (chat-viewport c)
            (string-join new-msgs "\n"))))
        (struct-copy chat c
                     [viewport new-vp]
                     [textarea (ta:textarea-focus (ta:make-textarea #:width (chat-width c)
                                                                    #:height 3
                                                                    #:placeholder "Send a message..."))]
                     [messages new-msgs])])]
    ;; Window resize
    [(window-size-msg w h)
     (define ta-height 3)
     (define gap 2)
     (define vp-height (max 3 (- h ta-height gap)))
     (struct-copy chat c
                  [width w]
                  [height h]
                  [viewport (vp:make-viewport #:width w #:height vp-height
                                              #:content (if (null? (chat-messages c))
                                                            "Welcome to the chat room!\nType a message and press Enter to send."
                                                            (string-join (chat-messages c) "\n")))]
                  [textarea (ta:textarea-focus
                             (ta:make-textarea #:width w #:height ta-height
                                               #:placeholder "Send a message..."))])]
    ;; Delegate to textarea
    [_
     (define-values (new-ta sub-cmd)
       (extract-update-result (update (chat-textarea c) msg)))
     (define new-c (struct-copy chat c [textarea new-ta]))
     (if sub-cmd (cmd new-c sub-cmd) new-c)]))

(define (chat-view c)
  (vcat 'left
        (view (chat-viewport c))
        ""
        (view (chat-textarea c))))

(struct chat-wrapper (c)
  #:transparent
  #:methods gen:kettle-model
  [(define (init cw) cw)
   (define (update cw msg)
     (define result (chat-update (chat-wrapper-c cw) msg))
     (if (cmd? result)
         (cmd (chat-wrapper (cmd-model result)) (cmd-value result))
         (chat-wrapper result)))
   (define (view cw) (chat-view (chat-wrapper-c cw)))])

(module+ main
  (program-run (make-program (chat-wrapper (make-chat)) #:alt-screen #t)))
