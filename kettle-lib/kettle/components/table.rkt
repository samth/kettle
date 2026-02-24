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
         "../private/borders.rkt"
         "../private/image.rkt")

(provide (struct-out table)
         make-table
         table-add-row
         table-set-widths
         table-render)

;;; Table model
(struct table (headers rows border* widths header-style row-style border-color) #:transparent)

(define (make-table #:headers [headers '()]
                    #:rows [rows '()]
                    #:border [bdr #f]
                    #:border-style [border-style #f]
                    #:header-style [hs #f]
                    #:row-style [rs #f]
                    #:border-color [bc #f])
  (table (or headers '()) (or rows '()) (or bdr border-style border-normal) #f hs rs bc))

(define (table-add-row tbl row)
  (struct-copy table tbl [rows (append (table-rows tbl) (list row))]))

(define (table-set-widths tbl widths)
  (struct-copy table tbl [widths widths]))

(define (calculate-column-widths tbl)
  (define headers (table-headers tbl))
  (define rows (table-rows tbl))
  (define num-cols
    (max (length headers)
         (if (null? rows)
             0
             (length (first rows)))))

  ;; Measure headers
  (define header-widths
    (for/list ([i (in-range num-cols)])
      (if (< i (length headers))
          (visible-length (format "~a" (list-ref headers i)))
          0)))

  ;; Measure rows
  (for/fold ([widths header-widths]) ([row (in-list rows)])
    (for/list ([w (in-list widths)]
               [i (in-naturals)])
      (if (< i (length row))
          (max w (visible-length (format "~a" (list-ref row i))))
          w))))

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
  (define top-line (string-append b-tl (string-join top-segments " ") b-tr))

  ;; Headers
  (define header-lines
    (if (null? headers)
        '()
        (let ()
          (define header-row (render-row headers widths bdr b-left b-right b-left))
          (define sep-segments
            (for/list ([w (in-list widths)])
              (make-string (+ w 2) (string-ref b-top 0))))
          (define sep-line (string-append b-ml (string-join sep-segments " ") b-mr))
          (list header-row sep-line))))

  ;; Data rows
  (define data-lines
    (for/list ([row (in-list rows)])
      (render-row row widths bdr b-left b-right b-left)))

  ;; Bottom border
  (define bottom-segments
    (for/list ([w (in-list widths)])
      (make-string (+ w 2) (string-ref b-bottom 0))))
  (define bottom-line (string-append b-bl (string-join bottom-segments " ") b-br))

  (apply vcat 'left (map text (append (list top-line) header-lines data-lines (list bottom-line)))))
