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
         "../private/protocol.rkt")

(provide (struct-out list-view)
         make-list-view
         list-view-init
         list-view-update
         list-view-render
         list-view-move-up!
         list-view-move-down!
         list-view-get-selected
         list-view-set-items!)

;;; List model
(struct list-view
  ([items #:mutable]
   [selected #:mutable]
   height
   [offset #:mutable])
  #:transparent)

(define (make-list-view #:items [items '()]
                        #:height [height 10])
  (list-view items 0 height 0))

;;; Component operations

(define (list-view-init lv)
  (values lv #f))

(define (list-view-update lv msg)
  (if (key-msg? msg)
      (let ([key (key-msg-key msg)])
        (cond
          ;; Move up
          [(or (eq? key 'up) (and (char? key) (char=? key #\k)))
           (list-view-move-up! lv)
           (values lv #f)]
          ;; Move down
          [(or (eq? key 'down) (and (char? key) (char=? key #\j)))
           (list-view-move-down! lv)
           (values lv #f)]
          [else (values lv #f)]))
      (values lv #f)))

(define (list-view-render lv)
  (define items (list-view-items lv))
  (define selected (list-view-selected lv))
  (define h (list-view-height lv))
  (define off (list-view-offset lv))
  (define start (min off (max 0 (- (length items) h))))
  (define end (min (length items) (+ off h)))
  (define visible-items (take (drop items start) (- end start)))

  (apply string-append
         (for/list ([item (in-list visible-items)]
                    [i (in-naturals start)])
           (format "~a ~a\n"
                   (if (= i selected) ">" " ")
                   item))))

;;; Helper functions

(define (list-view-move-up! lv)
  (when (> (list-view-selected lv) 0)
    (set-list-view-selected! lv (sub1 (list-view-selected lv)))
    (when (< (list-view-selected lv) (list-view-offset lv))
      (set-list-view-offset! lv (list-view-selected lv)))))

(define (list-view-move-down! lv)
  (define items (list-view-items lv))
  (when (< (list-view-selected lv) (sub1 (length items)))
    (set-list-view-selected! lv (add1 (list-view-selected lv)))
    (define max-visible (+ (list-view-offset lv) (list-view-height lv)))
    (when (>= (list-view-selected lv) max-visible)
      (set-list-view-offset! lv (add1 (list-view-offset lv))))))

(define (list-view-get-selected lv)
  (define items (list-view-items lv))
  (define selected (list-view-selected lv))
  (if (and (>= selected 0) (< selected (length items)))
      (list-ref items selected)
      #f))

(define (list-view-set-items! lv items)
  (set-list-view-items! lv items)
  (set-list-view-selected! lv 0)
  (set-list-view-offset! lv 0))
