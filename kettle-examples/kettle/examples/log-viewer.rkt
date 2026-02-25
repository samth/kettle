#lang racket/base

;; log-viewer.rkt -- High-performance log file viewer.
;; Optimized for large files (100K+ lines) with O(viewport-height) scroll operations.
;; Uses vectors for O(1) line access instead of lists.
;; Usage: racket log-viewer.rkt <filename>
;; Or: some-command | racket log-viewer.rkt

(require racket/format
         racket/list
         racket/port
         racket/string
         racket/file
         kettle/program
         kettle/image
         kettle/style)

(provide (struct-out log-viewer)
         make-log-viewer
         make-log-viewer-from-string)

(struct log-viewer
        (lines ; vector of strings (immutable after load)
         line-count ; cached (vector-length lines)
         max-line-w ; cached max line width
         filename ; display name
         y-offset ; scroll position (0-indexed)
         x-offset ; horizontal scroll position
         width
         height ; terminal dimensions
         gutter-w) ; line number gutter width (digits + 1 for space)
  #:transparent
  #:methods gen:tea-model
  [(define (init lv)
     (values lv #f))
   (define (update lv msg)
     (log-viewer-update lv msg))
   (define (view lv)
     (log-viewer-view lv))])

;; Compute gutter width: enough digits for the line count, plus 1 for separator space
(define (compute-gutter-w line-count)
  (+ 1 (string-length (number->string (max 1 line-count)))))

;; Compute max line width from a vector of strings
(define (compute-max-line-w lines-vec)
  (for/fold ([maxw 0]) ([line (in-vector lines-vec)])
    (max maxw (string-length line))))

(define (make-log-viewer lines-vec filename #:width [w 80] #:height [h 24])
  (define lc (vector-length lines-vec))
  (define maxw (compute-max-line-w lines-vec))
  (define gw (compute-gutter-w lc))
  (log-viewer lines-vec lc maxw filename 0 0 w h gw))

(define (make-log-viewer-from-string content filename #:width [w 80] #:height [h 24])
  (define lines-vec (list->vector (string-split content "\n" #:trim? #f)))
  (make-log-viewer lines-vec filename #:width w #:height h))

;;; Scrolling helpers -- all O(1)

(define (max-y-offset lv)
  (define view-h (- (log-viewer-height lv) 2)) ; reserve header + footer
  (max 0 (- (log-viewer-line-count lv) view-h)))

(define (clamp-y lv offset)
  (max 0 (min offset (max-y-offset lv))))

(define (max-x-offset lv)
  (define content-w (- (log-viewer-width lv) (log-viewer-gutter-w lv) 3)) ; gutter + " | "
  (max 0 (- (log-viewer-max-line-w lv) content-w)))

(define (clamp-x lv offset)
  (max 0 (min offset (max-x-offset lv))))

(define (scroll-percent lv)
  (define maxoff (max-y-offset lv))
  (if (<= maxoff 0)
      100
      (quotient (* 100 (log-viewer-y-offset lv)) maxoff)))

;;; Update

(define (log-viewer-update lv msg)
  (cond
    ;; Key messages
    [(key-msg? msg)
     (define key (key-msg-key msg))
     (cond
       ;; Quit
       [(and (char? key) (char=? key #\q)) (values lv (quit-cmd))]

       ;; Down / j
       [(or (eq? key 'down) (and (char? key) (char=? key #\j)))
        (values (struct-copy log-viewer lv [y-offset (clamp-y lv (add1 (log-viewer-y-offset lv)))])
                #f)]

       ;; Up / k
       [(or (eq? key 'up) (and (char? key) (char=? key #\k)))
        (values (struct-copy log-viewer lv [y-offset (clamp-y lv (sub1 (log-viewer-y-offset lv)))])
                #f)]

       ;; Page down / space
       [(or (and (char? key) (char=? key #\space)) (eq? key 'page-down))
        (define page-h (- (log-viewer-height lv) 2))
        (values
         (struct-copy log-viewer lv [y-offset (clamp-y lv (+ (log-viewer-y-offset lv) page-h))])
         #f)]

       ;; Page up / b
       [(or (and (char? key) (char=? key #\b)) (eq? key 'page-up))
        (define page-h (- (log-viewer-height lv) 2))
        (values
         (struct-copy log-viewer lv [y-offset (clamp-y lv (- (log-viewer-y-offset lv) page-h))])
         #f)]

       ;; Top / g
       [(and (char? key) (char=? key #\g)) (values (struct-copy log-viewer lv [y-offset 0]) #f)]

       ;; Bottom / G
       [(and (char? key) (char=? key #\G))
        (values (struct-copy log-viewer lv [y-offset (max-y-offset lv)]) #f)]

       ;; Left / h
       [(or (eq? key 'left) (and (char? key) (char=? key #\h)))
        (values (struct-copy log-viewer lv [x-offset (clamp-x lv (- (log-viewer-x-offset lv) 4))])
                #f)]

       ;; Right / l
       [(or (eq? key 'right) (and (char? key) (char=? key #\l)))
        (values (struct-copy log-viewer lv [x-offset (clamp-x lv (+ (log-viewer-x-offset lv) 4))])
                #f)]

       [else (values lv #f)])]

    ;; Window resize
    [(window-size-msg? msg)
     (define w (window-size-msg-width msg))
     (define h (window-size-msg-height msg))
     (define new-lv (struct-copy log-viewer lv [width w] [height h]))
     ;; Re-clamp offsets for new size
     (values (struct-copy log-viewer
                          new-lv
                          [y-offset (clamp-y new-lv (log-viewer-y-offset new-lv))]
                          [x-offset (clamp-x new-lv (log-viewer-x-offset new-lv))])
             #f)]

    [else (values lv #f)]))

;;; View -- O(viewport-height)

(define (pad-line-number n gutter-w)
  (~a (add1 n) #:min-width (sub1 gutter-w) #:align 'right))

(define (visible-portion line x-offset content-w)
  (define len (string-length line))
  (define start (min x-offset len))
  (define end (min (+ start content-w) len))
  (if (>= start len)
      ""
      (substring line start end)))

(define (log-viewer-view lv)
  (define w (log-viewer-width lv))
  (define h (log-viewer-height lv))
  (define view-h (- h 2)) ; header + footer
  (define lines (log-viewer-lines lv))
  (define line-count (log-viewer-line-count lv))
  (define y-off (log-viewer-y-offset lv))
  (define x-off (log-viewer-x-offset lv))
  (define gw (log-viewer-gutter-w lv))
  (define content-w (max 1 (- w gw 3))) ; " | " separator

  (define visible-start y-off)
  (define visible-end (min (+ visible-start view-h) line-count))
  (define actual-visible (- visible-end visible-start))

  (define header-style (make-style #:bold #t #:reverse #t))
  (define footer-style (make-style #:faint #t))
  (define gutter-style (make-style #:faint #t))

  (define fname (log-viewer-filename lv))
  (define pct (scroll-percent lv))

  ;; Header
  (define header-text
    (let ([s (format " ~a  (~a lines) " fname line-count)])
      (if (> (string-length s) w)
          (substring s 0 w)
          s)))

  ;; Content lines -- O(view-h)
  (define line-images
    (for/list ([i (in-range visible-start visible-end)])
      (define line (vector-ref lines i)) ; O(1)
      (define num-str (pad-line-number i gw))
      (define content (visible-portion line x-off content-w))
      (hcat 'top (styled gutter-style (text num-str)) (text " | ") (text content))))

  ;; Padding for short files
  (define pad-images (make-list (max 0 (- view-h actual-visible)) (text "")))

  ;; Footer
  (define footer-text
    (let ([s (format " ~a%  j/k:scroll  space/b:page  g/G:top/bottom  h/l:horiz  q:quit" pct)])
      (if (> (string-length s) w)
          (substring s 0 w)
          s)))

  (apply vcat
         'left
         (styled header-style (text header-text))
         (append line-images pad-images (list (styled footer-style (text footer-text))))))

(module+ main
  (define args (current-command-line-arguments))
  (define-values (content filename)
    (cond
      [(> (vector-length args) 0)
       (define path (vector-ref args 0))
       (values (file->string path) path)]
      [else (values (port->string (current-input-port)) "<stdin>")]))
  (define lv (make-log-viewer-from-string content filename))
  (define p (make-program lv #:alt-screen #t))
  (program-run p))
