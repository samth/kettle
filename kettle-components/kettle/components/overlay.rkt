#lang racket/base

;; components/overlay.rkt
;;
;; SPDX-License-Identifier: MIT
;;
;; String-based text compositing (overlay) for terminal UIs.
;; Ported from cl-tuition's overlay module (tuition/overlay.lisp),
;; inspired by github.com/rmhubbert/bubbletea-overlay.
;;
;; These utilities work at the string level (legacy layout API), compositing
;; a foreground text block on top of a background text block at a specified
;; position. For image-tree based overlays, use Kettle's zcat directly.

(require racket/match
         racket/string
         racket/format
         racket/math
         kettle/style)

(provide composite
         overlay-centered
         overlay-at)

;;; ---------- Position constants ----------
;; Position can be:
;;   'left / 'top     = 0
;;   'center / 'middle = centered
;;   'right / 'bottom = aligned to end
;;   an exact integer  = absolute position
;;   a real 0.0-1.0   = fractional position

;;; ---------- ANSI-aware string slicing ----------

;; Take the first N visible columns from a string, preserving ANSI codes.
(define (ansi-take-columns str n)
  (define len (string-length str))
  (define out '())
  (define cols 0)
  (let loop ([i 0] [cols 0] [out '()])
    (cond
      [(>= i len) (apply string-append (reverse out))]
      [(>= cols n) (apply string-append (reverse out))]
      ;; ANSI escape sequence
      [(and (< (add1 i) len)
            (char=? (string-ref str i) #\u001B)
            (char=? (string-ref str (add1 i)) #\[))
       ;; Scan to end of CSI sequence
       (define end
         (let esc-loop ([j (+ i 2)])
           (cond
             [(>= j len) j]
             [(and (>= (char->integer (string-ref str j)) #x40)
                   (<= (char->integer (string-ref str j)) #x7E))
              (add1 j)]
             [else (esc-loop (add1 j))])))
       (loop end cols (cons (substring str i end) out))]
      ;; Normal character
      [else
       (define ch (string-ref str i))
       (define cw (char-display-width ch))
       (if (> (+ cols cw) n)
           (apply string-append (reverse out))
           (loop (add1 i) (+ cols cw) (cons (string ch) out)))])))

;; Drop the first N visible columns from a string, preserving ANSI codes.
(define (ansi-drop-columns str n)
  (define len (string-length str))
  (let loop ([i 0] [cols 0] [pending-ansi '()])
    (cond
      [(>= i len) ""]
      [(>= cols n)
       (string-append (apply string-append (reverse pending-ansi))
                      (substring str i))]
      ;; ANSI escape sequence -- pass through but remember for prefix
      [(and (< (add1 i) len)
            (char=? (string-ref str i) #\u001B)
            (char=? (string-ref str (add1 i)) #\[))
       (define end
         (let esc-loop ([j (+ i 2)])
           (cond
             [(>= j len) j]
             [(and (>= (char->integer (string-ref str j)) #x40)
                   (<= (char->integer (string-ref str j)) #x7E))
              (add1 j)]
             [else (esc-loop (add1 j))])))
       (loop end cols (cons (substring str i end) pending-ansi))]
      ;; Normal character
      [else
       (define ch (string-ref str i))
       (define cw (char-display-width ch))
       (loop (add1 i) (+ cols cw) '())])))

;;; ---------- Position calculation ----------

(define (calculate-position pos fg-size bg-size offset)
  (define base
    (cond
      [(or (eq? pos 'left) (eq? pos 'top)) 0]
      [(or (eq? pos 'center) (eq? pos 'middle))
       (exact-floor (/ (- bg-size fg-size) 2))]
      [(or (eq? pos 'right) (eq? pos 'bottom))
       (- bg-size fg-size)]
      [(and (real? pos) (not (exact-integer? pos)))
       (exact-floor (* pos bg-size))]
      [(exact-integer? pos) pos]
      [else 0]))
  (+ base offset))

;;; ---------- Composite ----------

;; Composite foreground text on top of background text.
(define (composite fg bg
                   #:x-position [x-pos 'center]
                   #:y-position [y-pos 'middle]
                   #:x-offset [x-off 0]
                   #:y-offset [y-off 0])
  (define fg-lines (split-string-by-newline fg))
  (define bg-lines (split-string-by-newline bg))

  (define fg-w (if (null? fg-lines) 0
                   (apply max (map visible-length fg-lines))))
  (define fg-h (length fg-lines))
  (define bg-w (if (null? bg-lines) 0
                   (apply max (map visible-length bg-lines))))
  (define bg-h (length bg-lines))

  (define start-x (calculate-position x-pos fg-w bg-w x-off))
  (define start-y (calculate-position y-pos fg-h bg-h y-off))

  (define result
    (for/list ([row (in-range bg-h)])
      (define bg-line (if (< row (length bg-lines)) (list-ref bg-lines row) ""))
      (define fg-row (- row start-y))
      (cond
        ;; Not in foreground region
        [(or (< fg-row 0) (>= fg-row fg-h)) bg-line]
        ;; In foreground region
        [else
         (define fg-line (list-ref fg-lines fg-row))
         (define fg-line-w (visible-length fg-line))
         ;; Handle partial off-screen left
         (define actual-x (max 0 start-x))
         (define fg-trimmed
           (if (< start-x 0)
               (ansi-drop-columns fg-line (- start-x))
               fg-line))
         (define fg-trimmed-w (visible-length fg-trimmed))
         ;; Build composite line: left-bg + fg + right-bg
         (define left-part (ansi-take-columns bg-line actual-x))
         (define left-w (visible-length left-part))
         ;; Pad left if needed
         (define left-padded
           (if (< left-w actual-x)
               (string-append left-part (make-string (- actual-x left-w) #\space))
               left-part))
         ;; Right part: skip past the foreground region
         (define right-start (+ actual-x fg-trimmed-w))
         (define right-part (ansi-drop-columns bg-line right-start))
         (string-append left-padded
                        (ansi-reset)
                        fg-trimmed
                        (ansi-reset)
                        right-part)])))

  (string-join result "\n"))

;; Convenience: center foreground on background.
(define (overlay-centered fg bg)
  (composite fg bg #:x-position 'center #:y-position 'middle))

;; Convenience: place foreground at absolute (x, y) on background.
(define (overlay-at fg bg x y)
  (composite fg bg #:x-position x #:y-position y))
