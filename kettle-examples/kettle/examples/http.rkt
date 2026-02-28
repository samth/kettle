#lang racket/base

;; http.rkt -- Asynchronous HTTP request demo.
;; Adapted from cl-tuition examples/http.lisp
;; https://github.com/atgreen/cl-tuition/blob/master/examples/http.lisp
;;
;; Key differences from cl-tuition:
;; - Uses Racket's built-in net/url for real HTTP requests instead of
;;   cl-tuition's simulated (sleep 0.5) + fake 200 response.
;; - The async command is a lambda returning a msg struct, same pattern as
;;   cl-tuition, but Racket's thread-based I/O makes it genuinely concurrent.
;; - Auto-quits after displaying the result, same as cl-tuition.
;;
;; Fetches a URL and displays the HTTP status code.

(require racket/match
         racket/format
         net/url
         kettle/program
         kettle/image
         kettle/style)

(struct http-result msg (code) #:transparent)
(struct http-error-result msg (err) #:transparent)

(define target-url "https://www.racket-lang.org/")

(define-kettle-program http-demo
  #:fields ([status #f] [error-msg #f])
  #:init (lambda (self)
    ;; Launch async HTTP request
    (cmd self
         (lambda ()
           (with-handlers ([exn:fail?
                            (lambda (e)
                              (http-error-result (exn-message e)))])
             (define url (string->url target-url))
             (define port (head-impure-port url))
             (define first-line (read-line port))
             (close-input-port port)
             ;; Parse status code from "HTTP/1.1 200 OK"
             (define code
               (and first-line
                    (let ([m (regexp-match #rx"HTTP/[^ ]+ ([0-9]+)" first-line)])
                      (and m (string->number (second m))))))
             (http-result (or code 0))))))
  #:update (lambda (self msg)
    (match msg
      [(key-msg #\q _ _) (cmd self (quit-cmd))]
      [(key-msg _ _ #t)  (cmd self (quit-cmd))]
      [(key-msg 'escape _ _) (cmd self (quit-cmd))]
      [(http-result code)
       (cmd (struct-copy http-demo self [status code]) (quit-cmd))]
      [(http-error-result err)
       (struct-copy http-demo self [error-msg err])]
      [_ self]))
  #:view (lambda (self)
    (cond
      [(http-demo-error-msg self)
       (vcat 'left
             ""
             (hcat 'top (text (format "Checking ~a ... " target-url))
                        (styled (make-style #:foreground fg-red)
                                (format "error: ~a" (http-demo-error-msg self))))
             ""
             "")]
      [(http-demo-status self)
       (define code (http-demo-status self))
       (define status-text
         (cond [(= code 200) "OK"]
               [(= code 301) "Moved Permanently"]
               [(= code 404) "Not Found"]
               [(= code 500) "Internal Server Error"]
               [else "Unknown"]))
       (vcat 'left
             ""
             (hcat 'top (text (format "Checking ~a ... " target-url))
                        (styled (make-style #:foreground fg-bright-green #:bold #t)
                                (format "~a ~a" code status-text)))
             ""
             "")]
      [else
       (vcat 'left
             ""
             (text (format "Checking ~a ..." target-url))
             ""
             "")])))

(module+ main
  (program-run (make-program (make-http-demo))))
