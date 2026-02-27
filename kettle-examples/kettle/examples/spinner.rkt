#lang racket/base

;; spinner.rkt -- Spinner animation using the spinner component.
;; Adapted from cl-tuition examples/spinner.lisp and spinner-component.lisp
;; https://github.com/atgreen/cl-tuition/blob/master/examples/spinner.lisp
;; https://github.com/atgreen/cl-tuition/blob/master/examples/spinner-component.lisp
;;
;; Key differences from cl-tuition:
;; - Uses Kettle's built-in spinner component (kettle/components/spinner) which
;;   implements gen:kettle-model, so init/update/view are delegated automatically.
;; - cl-tuition's spinner.lisp manually manages frame advancement via tick-msg and
;;   sleep-based command loops; Kettle's spinner component handles its own tick
;;   subscription internally.
;; - The spinner-component.lisp version in cl-tuition is closer to this approach
;;   but still requires manual delegation of spinner-tick-msg; in Kettle, the
;;   component subscription system handles this transparently.
;;
;; Press q to quit.

(require racket/match
         kettle
         (prefix-in sp: kettle/components/spinner))

;; Import generic functions under a prefix to avoid shadowing inside #:methods
(require (only-in kettle
                  [init kettle:init]
                  [view kettle:view]))

(struct spinner-demo (spinner)
  #:transparent
  #:methods gen:kettle-model
  [(define (init sd)
     (define-values (new-sp init-cmd)
       (extract-update-result (kettle:init (spinner-demo-spinner sd))))
     (define new-sd (struct-copy spinner-demo sd [spinner new-sp]))
     (if init-cmd (cmd new-sd init-cmd) new-sd))
   (define (update sd msg)
     (match msg
       [(key-msg #\q _ _) (cmd sd (quit-cmd))]
       [(key-msg _ _ #t)  (cmd sd (quit-cmd))]
       [_ (delegate sd spinner-demo-spinner
                    (lambda (p new-sp) (struct-copy spinner-demo p [spinner new-sp]))
                    msg)]))
   (define (view sd)
     (hcat 'top
           (text "   ")
           (kettle:view (spinner-demo-spinner sd))
           (text " Loading forever...press q to quit")))])

(module+ main
  (define sd (spinner-demo (sp:make-spinner #:frames sp:spinner-dot)))
  (program-run (make-program sd #:alt-screen #t)))
