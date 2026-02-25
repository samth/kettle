#lang racket/base

;; components/list-view.rkt
;;
;; SPDX-License-Identifier: MIT
;;
;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;; Racket port
;;
;; List component - reusable scrollable list

(require racket/list
         "../private/protocol.rkt"
         "../private/image.rkt")

(provide (struct-out list-view)
         make-list-view
         list-view-move-up
         list-view-move-down
         list-view-get-selected
         list-view-set-items)

;;; List model -- implements gen:tea-model
(struct list-view (items selected height offset)
  #:transparent
  #:methods gen:tea-model
  [(define (init lv)
     lv)
   (define (update lv msg)
     (if (key-msg? msg)
         (let ([key (key-msg-key msg)])
           (cond
             [(or (eq? key 'up) (and (char? key) (char=? key #\k)))
              (list-view-move-up lv)]
             [(or (eq? key 'down) (and (char? key) (char=? key #\j)))
              (list-view-move-down lv)]
             [else lv]))
         lv))
   (define (view lv)
     (define items (list-view-items lv))
     (define sel (list-view-selected lv))
     (define h (list-view-height lv))
     (define off (list-view-offset lv))
     (define start (min off (max 0 (- (length items) h))))
     (define end (min (length items) (+ off h)))
     (define visible-items (take (drop items start) (- end start)))
     (apply vcat
            'left
            (for/list ([item (in-list visible-items)]
                       [i (in-naturals start)])
              (text (format "~a ~a" (if (= i sel) ">" " ") item)))))])

(define (make-list-view #:items [items '()] #:height [height 10])
  (list-view items 0 height 0))

;;; Functional update helpers (return new list-view)

(define (list-view-move-up lv)
  (define sel (list-view-selected lv))
  (define off (list-view-offset lv))
  (if (> sel 0)
      (let* ([new-sel (sub1 sel)]
             [new-off (if (< new-sel off) new-sel off)])
        (struct-copy list-view lv [selected new-sel] [offset new-off]))
      lv))

(define (list-view-move-down lv)
  (define items (list-view-items lv))
  (define sel (list-view-selected lv))
  (define off (list-view-offset lv))
  (define h (list-view-height lv))
  (if (< sel (sub1 (length items)))
      (let* ([new-sel (add1 sel)]
             [max-visible (+ off h)]
             [new-off (if (>= new-sel max-visible)
                          (add1 off)
                          off)])
        (struct-copy list-view lv [selected new-sel] [offset new-off]))
      lv))

(define (list-view-get-selected lv)
  (define items (list-view-items lv))
  (define sel (list-view-selected lv))
  (if (and (>= sel 0) (< sel (length items)))
      (list-ref items sel)
      #f))

(define (list-view-set-items lv new-items)
  (struct-copy list-view lv [items new-items] [selected 0] [offset 0]))
