#lang racket/base

;; components/datepicker.rkt
;;
;; SPDX-License-Identifier: MIT
;;
;; Interactive calendar date picker component.
;; Ported from cl-tuition's datepicker component (tui.datepicker).

(require racket/match
         racket/format
         racket/string
         racket/math
         kettle/program
         kettle/image
         kettle/style)

(provide (struct-out datepicker)
         make-datepicker
         datepicker-update
         datepicker-view
         datepicker-selected-date
         datepicker-focus
         datepicker-blur)

;;; ---------- Date arithmetic utilities ----------

;; Day of week for a given date (0=Sunday, 6=Saturday)
;; Uses Tomohiko Sakamoto's algorithm
(define (day-of-week year month day)
  (define t-table #(0 3 2 5 0 3 5 1 4 6 2 4))
  (define y (if (< month 3) (sub1 year) year))
  (modulo (+ y
             (quotient y 4)
             (- (quotient y 100))
             (quotient y 400)
             (vector-ref t-table (sub1 month))
             day)
          7))

;; Days in a given month (handles leap years)
(define (days-in-month year month)
  (cond
    [(memv month '(1 3 5 7 8 10 12)) 31]
    [(memv month '(4 6 9 11)) 30]
    [(= month 2)
     (if (or (and (zero? (modulo year 4))
                  (not (zero? (modulo year 100))))
             (zero? (modulo year 400)))
         29 28)]))

;; Add days to a date, returning (values year month day)
(define (add-days year month day n)
  (define d (+ day n))
  (cond
    [(> d (days-in-month year month))
     (define overflow (- d (days-in-month year month)))
     (define m (add1 month))
     (if (> m 12)
         (add-days (add1 year) 1 overflow 0)
         (add-days year m overflow 0))]
    [(< d 1)
     (define m (sub1 month))
     (if (< m 1)
         (add-days (sub1 year) 12 (+ (days-in-month (sub1 year) 12) d) 0)
         (add-days year m (+ (days-in-month year m) d) 0))]
    [else (values year month d)]))

;; Add months to a date
(define (add-months year month day n)
  (define total (+ (sub1 month) n))
  (define new-month (add1 (modulo total 12)))
  (define new-year (+ year (quotient total 12)))
  (when (< total 0)
    (set! new-month (add1 (modulo total 12)))
    (set! new-year (+ year (sub1 (quotient (sub1 total) 12)))))
  ;; Clamp day to valid range
  (define max-day (days-in-month new-year new-month))
  (values new-year new-month (min day max-day)))

;; Get today's date as (values year month day)
(define (today)
  (define s (current-seconds))
  (define d (seconds->date s))
  (values (date-year d) (date-month d) (date-day d)))

;; Compare two dates
(define (same-day? y1 m1 d1 y2 m2 d2)
  (and (= y1 y2) (= m1 m2) (= d1 d2)))

;;; ---------- Datepicker model ----------

(struct datepicker
  (year month day                    ; focused date
   sel-year sel-month sel-day        ; selected date (#f if none)
   focused?                          ; whether component has focus
   show-header?                      ; show month/year header
   highlight-today?)                 ; highlight today's date
  #:transparent
  #:methods gen:kettle-model
  [(define (init dp) dp)
   (define (update dp msg) (datepicker-update dp msg))
   (define (view dp) (datepicker-view dp))])

(define (make-datepicker #:focused? [focused? #t]
                         #:show-header? [show-header? #t]
                         #:highlight-today? [highlight-today? #t])
  (define-values (y m d) (today))
  (datepicker y m d #f #f #f focused? show-header? highlight-today?))

(define (datepicker-selected-date dp)
  (if (datepicker-sel-year dp)
      (values (datepicker-sel-year dp)
              (datepicker-sel-month dp)
              (datepicker-sel-day dp))
      (values #f #f #f)))

(define (datepicker-focus dp)
  (struct-copy datepicker dp [focused? #t]))

(define (datepicker-blur dp)
  (struct-copy datepicker dp [focused? #f]))

;;; ---------- Navigation ----------

(define (dp-move-day dp n)
  (define-values (ny nm nd)
    (add-days (datepicker-year dp) (datepicker-month dp) (datepicker-day dp) n))
  (struct-copy datepicker dp [year ny] [month nm] [day nd]))

(define (dp-move-month dp n)
  (define-values (ny nm nd)
    (add-months (datepicker-year dp) (datepicker-month dp) (datepicker-day dp) n))
  (struct-copy datepicker dp [year ny] [month nm] [day nd]))

(define (dp-go-today dp)
  (define-values (y m d) (today))
  (struct-copy datepicker dp [year y] [month m] [day d]))

(define (dp-select dp)
  (struct-copy datepicker dp
               [sel-year (datepicker-year dp)]
               [sel-month (datepicker-month dp)]
               [sel-day (datepicker-day dp)]))

(define (dp-clear-selection dp)
  (struct-copy datepicker dp [sel-year #f] [sel-month #f] [sel-day #f]))

;;; ---------- Update ----------

(define (datepicker-update dp msg)
  (if (not (datepicker-focused? dp))
      dp
      (match msg
        ;; Arrow keys / vim keys
        [(key-msg 'left _ _)      (dp-move-day dp -1)]
        [(key-msg #\h _ _)        (dp-move-day dp -1)]
        [(key-msg 'right _ _)     (dp-move-day dp 1)]
        [(key-msg #\l _ _)        (dp-move-day dp 1)]
        [(key-msg 'up _ _)        (dp-move-day dp -7)]
        [(key-msg #\k _ _)        (dp-move-day dp -7)]
        [(key-msg 'down _ _)      (dp-move-day dp 7)]
        [(key-msg #\j _ _)        (dp-move-day dp 7)]
        ;; Month navigation
        [(key-msg #\[ _ _)        (dp-move-month dp -1)]
        [(key-msg #\] _ _)        (dp-move-month dp 1)]
        ;; Year navigation
        [(key-msg #\{ _ _)        (dp-move-month dp -12)]
        [(key-msg #\} _ _)        (dp-move-month dp 12)]
        ;; Home = today
        [(key-msg 'home _ _)      (dp-go-today dp)]
        ;; Enter/space = select
        [(key-msg 'enter _ _)     (dp-select dp)]
        [(key-msg #\space _ _)    (dp-select dp)]
        ;; Escape = clear selection
        [(key-msg 'escape _ _)    (dp-clear-selection dp)]
        [_ dp])))

;;; ---------- View ----------

(define month-names
  #("" "January" "February" "March" "April" "May" "June"
    "July" "August" "September" "October" "November" "December"))

(define day-names "Su Mo Tu We Th Fr Sa")

(define cursor-style (make-style #:underline #t #:bold #t))
(define selected-style (make-style #:reverse #t))
(define selected-cursor-style (make-style #:reverse #t #:underline #t #:bold #t))
(define today-style (make-style #:bold #t))
(define outside-style (make-style #:faint #t))
(define header-style (make-style #:foreground fg-bright-cyan #:bold #t))

(define (datepicker-view dp)
  (define fy (datepicker-year dp))
  (define fm (datepicker-month dp))
  (define fd (datepicker-day dp))

  (define-values (ty tm td) (today))

  (define sy (datepicker-sel-year dp))
  (define sm (datepicker-sel-month dp))
  (define sd (datepicker-sel-day dp))

  ;; Calendar starts on the Sunday of the week containing the 1st
  (define first-dow (day-of-week fy fm 1))
  (define-values (start-y start-m start-d)
    (add-days fy fm 1 (- first-dow)))

  ;; Build 6 weeks × 7 days
  (define rows
    (for/list ([week (in-range 6)])
      (apply hcat 'top
        (for/list ([col (in-range 7)])
          (define offset (+ (* week 7) col))
          (define-values (cy cm cd)
            (add-days start-y start-m start-d offset))
          (define day-str (~a cd #:width 2 #:align 'right))
          (define is-cursor? (same-day? cy cm cd fy fm fd))
          (define is-selected? (and sy (same-day? cy cm cd sy sm sd)))
          (define is-today? (and (datepicker-highlight-today? dp)
                                 (same-day? cy cm cd ty tm td)))
          (define is-outside? (not (= cm fm)))
          (define day-style
            (cond
              [(and is-selected? is-cursor?) selected-cursor-style]
              [is-selected? selected-style]
              [is-cursor? cursor-style]
              [is-today? today-style]
              [is-outside? outside-style]
              [else #f]))
          (define cell (if day-style (styled day-style day-str) (text day-str)))
          (if (< col 6)
              (hcat 'top cell (text " "))
              cell)))))

  (define cal-body (apply vcat 'left rows))

  (if (datepicker-show-header? dp)
      (let* ([hdr (styled header-style
                          (format "~a ~a"
                                  (vector-ref month-names fm) fy))]
             [dn (text day-names)])
        (vcat 'left
              (pad 2 0 0 0 hdr)
              (pad 2 0 0 0 dn)
              (pad 2 0 0 0 cal-body)))
      (vcat 'left
            (pad 2 0 0 0 (text day-names))
            (pad 2 0 0 0 cal-body))))
