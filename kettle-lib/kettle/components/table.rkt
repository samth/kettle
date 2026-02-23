#lang racket/base

;; components/table.rkt
;;
;; SPDX-License-Identifier: MIT
;;
;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;; Racket port
;;
;; Table component - table rendering

(require racket/string
         racket/list
         "../private/style.rkt"
         "../private/borders.rkt")

(provide (struct-out table)
         make-table
         table-add-row!
         table-set-widths!
         table-render)

;;; Table model
(struct table
  ([headers #:mutable]
   [rows #:mutable]
   [border* #:mutable]
   [widths #:mutable]
   header-style
   row-style
   border-color)
  #:transparent)

(define (make-table #:headers [headers '()]
                    #:rows [rows '()]
                    #:border [bdr #f]
                    #:border-style [border-style #f]
                    #:header-style [hs #f]
                    #:row-style [rs #f]
                    #:border-color [bc #f])
  (table (or headers '())
         (or rows '())
         (or bdr border-style border-normal)
         #f
         hs rs bc))

(define (table-add-row! tbl row)
  (set-table-rows! tbl (append (table-rows tbl) (list row))))

(define (table-set-widths! tbl widths)
  (set-table-widths! tbl widths))

(define (calculate-column-widths tbl)
  (define headers (table-headers tbl))
  (define rows (table-rows tbl))
  (define num-cols (max (length headers)
                        (if (null? rows) 0 (length (first rows)))))
  (define widths (make-list num-cols 0))

  ;; Measure headers
  (set! widths
        (for/list ([w (in-list widths)]
                   [i (in-naturals)])
          (if (< i (length headers))
              (max w (visible-length (format "~a" (list-ref headers i))))
              w)))

  ;; Measure rows
  (for ([row (in-list rows)])
    (set! widths
          (for/list ([w (in-list widths)]
                     [i (in-naturals)])
            (if (< i (length row))
                (max w (visible-length (format "~a" (list-ref row i))))
                w))))

  widths)

(define (pad-cell text width)
  (define text-str (format "~a" text))
  (define isolated (bidi-isolate-ltr text-str))
  (define vlen (visible-length text-str))
  (define padding (max 0 (- width vlen)))
  (string-append isolated (make-string padding #\space)))

(define (render-row cells widths bdr left-glyph right-glyph separator)
  ;; Unicode bidi isolates for separators
  (define lri (string (integer->char #x2066)))
  (define pdi (string (integer->char #x2069)))
  (define sep (string-append lri (or separator " ") pdi))
  (define l-left (string-append lri left-glyph pdi))
  (define l-right (string-append lri right-glyph pdi))

  (define out (open-output-string))
  (display (string-append l-left " ") out)
  (for ([cell (in-list cells)]
        [width (in-list widths)]
        [i (in-naturals)])
    (display (pad-cell cell width) out)
    (when (< i (sub1 (length widths)))
      (display (string-append " " sep " ") out)))
  (display (string-append " " l-right) out)
  (get-output-string out))

(define (table-render tbl)
  (define bdr (table-border* tbl))
  (define headers (table-headers tbl))
  (define rows (table-rows tbl))
  (define widths (or (table-widths tbl) (calculate-column-widths tbl)))
  (define result '())

  (define b-left (border-left bdr))
  (define b-right (border-right bdr))
  (define b-top (border-top bdr))
  (define b-bottom (border-bottom bdr))
  (define b-tl (border-top-left bdr))
  (define b-tr (border-top-right bdr))
  (define b-bl (border-bottom-left bdr))
  (define b-br (border-bottom-right bdr))
  (define b-ml (border-middle-left bdr))
  (define b-mr (border-middle-right bdr))

  ;; Top border
  (define top-segments
    (for/list ([w (in-list widths)])
      (make-string (+ w 2) (string-ref b-top 0))))
  (set! result
        (append result
                (list (string-append b-tl (string-join top-segments " ") b-tr))))

  ;; Headers
  (when (not (null? headers))
    (set! result
          (append result
                  (list (render-row headers widths bdr b-left b-right b-left))))

    ;; Header separator
    (define sep-segments
      (for/list ([w (in-list widths)])
        (make-string (+ w 2) (string-ref b-top 0))))
    (set! result
          (append result
                  (list (string-append b-ml (string-join sep-segments " ") b-mr)))))

  ;; Data rows
  (for ([row (in-list rows)])
    (set! result
          (append result
                  (list (render-row row widths bdr b-left b-right b-left)))))

  ;; Bottom border
  (define bottom-segments
    (for/list ([w (in-list widths)])
      (make-string (+ w 2) (string-ref b-bottom 0))))
  (set! result
        (append result
                (list (string-append b-bl (string-join bottom-segments " ") b-br))))

  (string-join result "\n"))
