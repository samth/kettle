#lang racket/base

;; components/paginator.rkt
;;
;; SPDX-License-Identifier: MIT
;;
;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;; Racket port
;;
;; Paginator component for pagination UI

(require racket/string
         "../private/protocol.rkt")

(provide (struct-out paginator)
         make-paginator

         ;; Display types
         paginator-arabic
         paginator-dots

         ;; Operations
         paginator-init
         paginator-update
         paginator-view
         paginator-prev-page!
         paginator-next-page!
         paginator-set-total-pages!
         paginator-get-slice-bounds
         paginator-items-on-page
         paginator-on-first-page?
         paginator-on-last-page?)

;;; Display type constants
(define paginator-arabic 'arabic)
(define paginator-dots 'dots)

;;; Paginator model
(struct paginator
  (type
   [page #:mutable]
   per-page
   [total-pages #:mutable]
   active-dot
   inactive-dot
   arabic-format)
  #:transparent)

(define (make-paginator #:type [type 'arabic]
                        #:per-page [per-page 10]
                        #:total-pages [total-pages 1]
                        #:active-dot [active-dot "•"]
                        #:inactive-dot [inactive-dot "○"]
                        #:arabic-format [arabic-fmt "~a/~a"])
  (paginator type 0 per-page total-pages active-dot inactive-dot arabic-fmt))

;;; Helper functions

(define (paginator-set-total-pages! pg items)
  (when (>= items 1)
    (define per-page (paginator-per-page pg))
    (define n (ceiling (/ items per-page)))
    (set-paginator-total-pages! pg (exact-ceiling n)))
  (paginator-total-pages pg))

(define (paginator-get-slice-bounds pg len)
  "Returns (values start end)."
  (define page (paginator-page pg))
  (define per-page (paginator-per-page pg))
  (define start (* page per-page))
  (define end (min (+ start per-page) len))
  (values start end))

(define (paginator-items-on-page pg total-items)
  (if (< total-items 1)
      0
      (let-values ([(start end) (paginator-get-slice-bounds pg total-items)])
        (- end start))))

(define (paginator-prev-page! pg)
  (when (> (paginator-page pg) 0)
    (set-paginator-page! pg (sub1 (paginator-page pg))))
  pg)

(define (paginator-next-page! pg)
  (unless (paginator-on-last-page? pg)
    (set-paginator-page! pg (add1 (paginator-page pg))))
  pg)

(define (paginator-on-first-page? pg)
  (= (paginator-page pg) 0))

(define (paginator-on-last-page? pg)
  (= (paginator-page pg) (sub1 (paginator-total-pages pg))))

;;; Rendering

(define (paginator-dots-view pg)
  (define total (paginator-total-pages pg))
  (define current (paginator-page pg))
  (define active (paginator-active-dot pg))
  (define inactive (paginator-inactive-dot pg))
  (apply string-append
         (for/list ([i (in-range total)])
           (if (= i current) active inactive))))

(define (paginator-arabic-view pg)
  (format (paginator-arabic-format pg)
          (add1 (paginator-page pg))
          (paginator-total-pages pg)))

;;; TEA protocol

(define (paginator-init pg)
  (values pg #f))

(define (paginator-update pg msg)
  (cond
    [(key-msg? msg)
     (define key (key-msg-key msg))
     (cond
       [(or (eq? key 'right)
            (eq? key 'page-down)
            (and (char? key) (char=? key #\l)))
        (values (paginator-next-page! pg) #f)]
       [(or (eq? key 'left)
            (eq? key 'page-up)
            (and (char? key) (char=? key #\h)))
        (values (paginator-prev-page! pg) #f)]
       [else (values pg #f)])]
    [else (values pg #f)]))

(define (paginator-view pg)
  (case (paginator-type pg)
    [(dots) (paginator-dots-view pg)]
    [else (paginator-arabic-view pg)]))
