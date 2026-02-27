#lang racket/base

;; components/datepicker.rkt
;;
;; SPDX-License-Identifier: MIT
;;
;; Interactive calendar date picker component.
;; Ported from cl-tuition's datepicker component (tui.datepicker).
;;
;; Uses the gregor package for date representation and arithmetic.

(require racket/match
         racket/format
         racket/string
         (only-in gregor
                  today
                  ->year ->month ->day ->wday
                  +days -days +months
                  days-in-month
                  date=?)
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

;;; ---------- Datepicker model ----------

(struct datepicker
  (cursor                            ; gregor date: the focused date
   selected                          ; gregor date or #f: selected date
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
  (datepicker (today) #f focused? show-header? highlight-today?))

(define (datepicker-selected-date dp)
  (datepicker-selected dp))

(define (datepicker-focus dp)
  (struct-copy datepicker dp [focused? #t]))

(define (datepicker-blur dp)
  (struct-copy datepicker dp [focused? #f]))

;;; ---------- Navigation ----------

(define (dp-move-day dp n)
  (struct-copy datepicker dp
    [cursor (+days (datepicker-cursor dp) n)]))

(define (dp-move-month dp n)
  (struct-copy datepicker dp
    [cursor (+months (datepicker-cursor dp) n)]))

(define (dp-go-today dp)
  (struct-copy datepicker dp [cursor (today)]))

(define (dp-select dp)
  (struct-copy datepicker dp [selected (datepicker-cursor dp)]))

(define (dp-clear-selection dp)
  (struct-copy datepicker dp [selected #f]))

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
  (define cursor (datepicker-cursor dp))
  (define fy (->year cursor))
  (define fm (->month cursor))

  (define td (today))
  (define sel (datepicker-selected dp))

  ;; First of month: subtract (day-1) days from cursor
  (define first-of-month (-days cursor (sub1 (->day cursor))))
  ;; Calendar starts on the Sunday of the week containing the 1st
  (define first-dow (->wday first-of-month))
  (define start (-days first-of-month first-dow))

  ;; Build 6 weeks × 7 days
  (define rows
    (for/list ([week (in-range 6)])
      (apply hcat 'top
        (for/list ([col (in-range 7)])
          (define offset (+ (* week 7) col))
          (define cell-date (+days start offset))
          (define cd (->day cell-date))
          (define cm (->month cell-date))
          (define day-str (~a cd #:width 2 #:align 'right))
          (define is-cursor? (date=? cell-date cursor))
          (define is-selected? (and sel (date=? cell-date sel)))
          (define is-today? (and (datepicker-highlight-today? dp)
                                 (date=? cell-date td)))
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
              (pad hdr #:left 2)
              (pad dn #:left 2)
              (pad cal-body #:left 2)))
      (vcat 'left
            (pad (text day-names) #:left 2)
            (pad cal-body #:left 2))))
