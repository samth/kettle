#lang racket/base

;; components/zone-manager.rkt
;;
;; SPDX-License-Identifier: MIT
;;
;; Mouse zone tracking for Kettle TUI programs.
;; Ported from cl-tuition's zone manager (tuition/zone.lisp).
;;
;; Unlike cl-tuition which embeds invisible markers in rendered strings and
;; scans them after layout, Kettle's image-tree renderer makes that approach
;; impractical. Instead, this module provides an explicit zone registry:
;; programs register named rectangular zones with known screen positions,
;; then query which zone a mouse event falls in.

(require racket/match
         kettle/program)

(provide (struct-out zone-info)
         (struct-out zone-manager)
         make-zone-manager
         zone-register
         zone-register-many
         zone-clear
         zone-clear-all
         zone-get
         zone-in-bounds?
         zone-hit
         zone-new-prefix)

;;; ---------- Zone info ----------

;; A named rectangular region on screen.
;; x, y are 0-based column/row; w, h are width/height in cells.
(struct zone-info (id x y w h) #:transparent)

;;; ---------- Zone manager ----------

;; Immutable hash-based zone registry.
(struct zone-manager (zones prefix-counter) #:transparent)

(define (make-zone-manager)
  (zone-manager (hasheq) 0))

;; Register a zone. Returns updated zone-manager.
(define (zone-register zm id x y w h)
  (struct-copy zone-manager zm
               [zones (hash-set (zone-manager-zones zm) id (zone-info id x y w h))]))

;; Register multiple zones at once. specs is a list of (id x y w h).
(define (zone-register-many zm specs)
  (define zones (zone-manager-zones zm))
  (for/fold ([z zones]) ([spec (in-list specs)])
    (match-define (list id x y w h) spec)
    (hash-set z id (zone-info id x y w h)))
  (struct-copy zone-manager zm
               [zones (for/fold ([z zones]) ([spec (in-list specs)])
                        (match-define (list id x y w h) spec)
                        (hash-set z id (zone-info id x y w h)))]))

;; Remove a zone by ID. Returns updated zone-manager.
(define (zone-clear zm id)
  (struct-copy zone-manager zm
               [zones (hash-remove (zone-manager-zones zm) id)]))

;; Remove all zones. Returns updated zone-manager.
(define (zone-clear-all zm)
  (struct-copy zone-manager zm [zones (hasheq)]))

;; Look up a zone by ID. Returns zone-info or #f.
(define (zone-get zm id)
  (hash-ref (zone-manager-zones zm) id #f))

;; Check if a point (mouse-x, mouse-y) is inside a zone.
;; mouse-x and mouse-y are 0-based screen coordinates.
;; Also accepts a mouse-event struct directly.
(define (zone-in-bounds? zi x-or-event [maybe-y #f])
  (and zi
       (let-values ([(mx my)
                     (if maybe-y
                         (values x-or-event maybe-y)
                         (values (mouse-event-x x-or-event)
                                 (mouse-event-y x-or-event)))])
         (and (>= mx (zone-info-x zi))
              (< mx (+ (zone-info-x zi) (zone-info-w zi)))
              (>= my (zone-info-y zi))
              (< my (+ (zone-info-y zi) (zone-info-h zi)))))))

;; Find the first zone containing the given point.
;; Returns the zone ID or #f.
(define (zone-hit zm x-or-event [maybe-y #f])
  (define-values (mx my)
    (if maybe-y
        (values x-or-event maybe-y)
        (values (mouse-event-x x-or-event)
                (mouse-event-y x-or-event))))
  (for/first ([(id zi) (in-hash (zone-manager-zones zm))]
              #:when (zone-in-bounds? zi mx my))
    id))

;; Generate a unique prefix string for zone IDs.
;; Each call returns a different prefix.
(define zone-prefix-counter 0)

(define (zone-new-prefix)
  (set! zone-prefix-counter (add1 zone-prefix-counter))
  (format "z~a-" zone-prefix-counter))
