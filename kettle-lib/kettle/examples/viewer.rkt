#lang racket/base

;; viewer.rkt -- Scrollable text viewer.
;; Demonstrates the viewport component, command-line args, styled borders.
;; Usage: racket viewer.rkt <filename>
;; Or: some-command | racket viewer.rkt

(require racket/file
         racket/port
         racket/format
         kettle/program
         kettle/image
         kettle/style
         kettle/border
         (prefix-in vp: kettle/components/viewport))

(provide (struct-out file-viewer)
         make-file-viewer)

(struct file-viewer (viewport filename)
  #:transparent
  #:methods gen:tea-model
  [(define (init fv)
     (values fv #f))
   (define (update fv msg)
     (file-viewer-update fv msg))
   (define (view fv)
     (file-viewer-view fv))])

(define (make-file-viewer content filename #:width [w 80] #:height [h 24])
  (file-viewer (make-viewport-from-content content w (- h 3)) ;; reserve 3 lines for chrome
               filename))

(define (make-viewport-from-content content w h)
  (vp:make-viewport #:width w #:height h #:content content))

(define (file-viewer-update fv msg)
  (cond
    ;; q quits
    [(and (key-msg? msg) (char? (key-msg-key msg)) (char=? (key-msg-key msg) #\q))
     (values fv (quit-cmd))]

    ;; Window resize
    [(window-size-msg? msg)
     (define w (window-size-msg-width msg))
     (define h (window-size-msg-height msg))
     (define content (vp:viewport-content (file-viewer-viewport fv)))
     (values (struct-copy file-viewer fv [viewport (make-viewport-from-content content w (- h 3))])
             #f)]

    ;; Delegate everything else to viewport
    [else
     (define-values (new-vp cmd) (update (file-viewer-viewport fv) msg))
     (values (struct-copy file-viewer fv [viewport new-vp]) cmd)]))

(define (file-viewer-view fv)
  (define vp (file-viewer-viewport fv))
  (define pct (vp:viewport-scroll-percent vp))
  (define total (vp:viewport-total-lines vp))
  (define fname (file-viewer-filename fv))
  (define header-style (make-style #:bold #t #:reverse #t))
  (define footer-style (make-style #:faint #t))

  (vcat 'left
        (styled header-style (text (format " ~a  (~a lines) " fname total)))
        (view vp)
        (styled footer-style
                (text (format " ~a%  j/k:scroll  space/b:page  g/G:top/bottom  q:quit"
                              (~r (* 100 pct) #:precision '(= 0)))))))

(module+ main
  (define args (current-command-line-arguments))
  (define-values (content filename)
    (cond
      [(> (vector-length args) 0)
       (define path (vector-ref args 0))
       (values (file->string path) path)]
      [else (values (port->string (current-input-port)) "<stdin>")]))
  (define p (make-program (make-file-viewer content filename) #:alt-screen #t))
  (program-run p))
