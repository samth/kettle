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

(struct spinner-demo (spinner)
  #:transparent
  #:methods gen:kettle-model
  [(define (init sd) sd)
   (define (update sd msg)
     (match msg
       [(key-msg #\q _ _) (cmd sd (quit-cmd))]
       [(key-msg _ _ #t)  (cmd sd (quit-cmd))]
       [_ (define-values (new-sp sub-cmd)
            (extract-update-result (update (spinner-demo-spinner sd) msg)))
          (define new-sd (struct-copy spinner-demo sd [spinner new-sp]))
          (if sub-cmd (cmd new-sd sub-cmd) new-sd)]))
   (define (subscriptions sd)
     (subscriptions (spinner-demo-spinner sd)))
   (define (view sd)
     (hcat 'top
           (text "   ")
           (view (spinner-demo-spinner sd))
           (text " Loading forever...press q to quit")))])

(module+ main
  (define sd (spinner-demo (sp:make-spinner #:frames sp:spinner-dot)))
  (program-run (make-program sd #:alt-screen #t)))
