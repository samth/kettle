#lang racket/base

;; textinput-form.rkt -- Multi-field registration form using textinput component.
;; Adapted from cl-tuition examples/textinput-component.lisp
;; https://github.com/atgreen/cl-tuition/blob/master/examples/textinput-component.lisp
;;
;; Key differences from cl-tuition:
;; - Uses Kettle's textinput component (gen:kettle-model) for each field,
;;   delegating update and rendering via extract-update-result + struct-copy.
;; - cl-tuition uses CLOS setf mutation to update component state; Kettle uses
;;   immutable struct-copy returning a new form struct each time.
;; - Tab/Shift-Tab cycling through fields is done with pattern matching on
;;   key-msg symbols ('tab, 'backtab) vs. cl-tuition's (eq key :tab).
;; - Password echo-mode handled by the component's #:echo-mode 'password.
;;
;; Tab/Shift-Tab to cycle fields, Enter to submit, Ctrl-C to quit.

(require racket/match
         racket/format
         kettle/program
         kettle/image
         kettle/style
         (prefix-in ti: kettle/components/textinput))

(struct form (name-input email-input password-input active submitted? data)
  #:transparent)

(define (make-form)
  (form
   (ti:textinput-focus
    (ti:make-textinput #:prompt "Name:     " #:placeholder "Enter your name"
                       #:width 30 #:char-limit 50))
   (ti:make-textinput #:prompt "Email:    " #:placeholder "you@example.com"
                      #:width 30 #:char-limit 100)
   (ti:make-textinput #:prompt "Password: " #:placeholder "password"
                      #:width 30 #:echo-mode 'password #:echo-char #\*)
   'name #f #f))

(define (get-active-input f)
  (case (form-active f)
    [(name) (form-name-input f)]
    [(email) (form-email-input f)]
    [(password) (form-password-input f)]))

(define (set-active-input f new-input)
  (case (form-active f)
    [(name) (struct-copy form f [name-input new-input])]
    [(email) (struct-copy form f [email-input new-input])]
    [(password) (struct-copy form f [password-input new-input])]))

(define (focus-field f field)
  (define blur-current
    (set-active-input f (ti:textinput-blur (get-active-input f))))
  (define new-f (struct-copy form blur-current [active field]))
  (set-active-input new-f (ti:textinput-focus (get-active-input new-f))))

(define next-fields
  #hasheq((name . email) (email . password) (password . name)))
(define prev-fields
  #hasheq((name . password) (password . email) (email . name)))

(define (form-update f msg)
  (cond
    [(form-submitted? f) (cmd f (quit-cmd))]
    [else
     (match msg
       [(key-msg _ _ #t) (cmd f (quit-cmd))]
       [(key-msg 'tab _ _) (focus-field f (hash-ref next-fields (form-active f)))]
       [(key-msg 'backtab _ _) (focus-field f (hash-ref prev-fields (form-active f)))]
       [(key-msg 'enter _ _)
        (struct-copy form f
                     [submitted? #t]
                     [data (list (ti:textinput-value (form-name-input f))
                                 (ti:textinput-value (form-email-input f))
                                 (ti:textinput-value (form-password-input f)))])]
       [_
        (define active-input (get-active-input f))
        (define-values (new-input sub-cmd)
          (extract-update-result (update active-input msg)))
        (define new-f (set-active-input f new-input))
        (if sub-cmd (cmd new-f sub-cmd) new-f)])]))

(define (form-view f)
  (cond
    [(form-submitted? f)
     (match-define (list name email _pw) (form-data f))
     (vcat 'left
           ""
           (bold "Form Submitted!")
           ""
           (hcat 'top (text "Name:  ") (text name))
           (hcat 'top (text "Email: ") (text email))
           ""
           "Press any key to exit.")]
    [else
     (vcat 'left
           ""
           (bold "Registration Form")
           "================"
           ""
           (view (form-name-input f))
           (view (form-email-input f))
           (view (form-password-input f))
           ""
           (styled (make-style #:foreground fg-bright-black)
                   "Tab: next field • Shift+Tab: prev field • Enter: submit • Ctrl-C: quit"))]))

(struct form-wrapper (f)
  #:transparent
  #:methods gen:kettle-model
  [(define (init fw) fw)
   (define (update fw msg)
     (define result (form-update (form-wrapper-f fw) msg))
     (if (cmd? result)
         (cmd (form-wrapper (cmd-model result)) (cmd-value result))
         (form-wrapper result)))
   (define (view fw) (form-view (form-wrapper-f fw)))])

(module+ main
  (program-run (make-program (form-wrapper (make-form)) #:alt-screen #t)))
