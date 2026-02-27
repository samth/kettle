#lang racket/base

;; datepicker.rkt -- Interactive calendar date picker.
;; Adapted from cl-tuition examples/datepicker.lisp
;; https://github.com/atgreen/cl-tuition/blob/master/examples/datepicker.lisp
;;
;; Key differences from cl-tuition:
;; - Uses Kettle's datepicker component (kettle/components/datepicker) which
;;   implements gen:kettle-model with immutable structs, vs. cl-tuition's CLOS
;;   datepicker class with mutable slots.
;; - Delegation uses extract-update-result instead of multiple-value-bind on
;;   CLOS update-message return values.
;; - Date display uses Racket's ~a formatting instead of CL's decode-universal-time.
;; - The component uses struct-copy for navigation state changes instead of setf.
;;
;; Navigate with arrows/hjkl, [/] for months, {/} for years.
;; Enter/Space to select, Escape to clear, Home for today.
;; Press q to quit.

(require racket/match
         racket/format
         kettle/program
         kettle/image
         kettle/style
         (prefix-in dp: kettle/components/datepicker))

(define-kettle-program datepicker-demo
  #:fields ([picker (dp:make-datepicker)])
  #:update (lambda (self msg)
    (match msg
      ;; Quit on q or ctrl-c
      [(key-msg #\q _ _) (cmd self (quit-cmd))]
      [(key-msg _ _ #t)  (cmd self (quit-cmd))]
      ;; Delegate to datepicker
      [_ (define new-picker (dp:datepicker-update (datepicker-demo-picker self) msg))
         (struct-copy datepicker-demo self [picker new-picker])]))
  #:view (lambda (self)
    (define picker (datepicker-demo-picker self))
    (define-values (sy sm sd) (dp:datepicker-selected-date picker))
    (define selected-str
      (if sy
          (format "Selected: ~a/~a/~a" sm sd sy)
          "No date selected"))
    (vcat 'left
          ""
          (styled (make-style #:foreground fg-bright-cyan #:bold #t)
                  "  Date Picker Demo")
          ""
          (dp:datepicker-view picker)
          ""
          (text (format "  ~a" selected-str))
          ""
          "  Controls:"
          "  Arrow keys / hjkl: Navigate days/weeks"
          "  [ / ]: Previous/next month"
          "  { / }: Previous/next year"
          "  Enter/Space: Select date"
          "  Escape: Clear selection"
          "  Home: Go to today"
          "  q: Quit")))

(module+ main
  (program-run (make-program (make-datepicker-demo) #:alt-screen #t)))
