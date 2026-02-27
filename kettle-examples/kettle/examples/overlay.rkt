#lang racket/base

;; overlay.rkt -- Text compositing / overlay demo.
;; Adapted from cl-tuition examples/overlay.lisp
;; https://github.com/atgreen/cl-tuition/blob/master/examples/overlay.lisp
;;
;; Key differences from cl-tuition:
;; - Uses Kettle's overlay component (kettle/components/overlay) which provides
;;   string-level composite, overlay-centered, and overlay-at functions.
;; - Kettle also offers zcat in the image tree for image-level overlays; this
;;   example demonstrates string-level compositing for legacy layout compatibility.
;; - This is a non-interactive demo (prints to stdout), same as cl-tuition.
;; - Uses Racket's for loops and string-append instead of CL's with-output-to-string
;;   and format directives.
;;
;; Run to see overlay demos printed to stdout.

(require racket/format
         racket/string
         racket/math
         kettle/style
         kettle/border
         kettle/layout
         (prefix-in ov: kettle/components/overlay))

;;; Build a colorful background grid
(define (build-background width height)
  (string-join
   (for/list ([y (in-range height)])
     (apply string-append
            (for/list ([x (in-range width)])
              (define r (exact-floor (* 255 (/ x width))))
              (define g (exact-floor (* 255 (/ y height))))
              (define b (exact-floor (* 128 (+ (/ x width) (/ y height)))))
              (colored "  " #:bg (color-rgb r g b)))))
   "\n"))

;;; Build a simple list UI as background
(define (build-list-background width height)
  (define items '("Documents" "Downloads" "Pictures" "Music" "Videos"
                  "Projects" "Desktop" "Applications" "Library" "System"))
  (define header (render-styled (make-style #:bold #t #:foreground fg-bright-cyan)
                                "File Browser"))
  (define lines
    (cons header
          (for/list ([item (in-list items)]
                     [i (in-naturals)])
            (if (= i 2)
                (render-styled (make-style #:foreground fg-black #:background bg-cyan)
                               (format "  ~a" (~a item #:width (- width 6))))
                (render-styled (make-style #:foreground fg-white)
                               (format "  ~a" item))))))
  (define content (string-join lines "\n"))
  (render-styled (make-style #:background bg-bright-black #:width width #:height height)
                 content))

;;; Build a dialog box
(define (build-dialog)
  (define title (render-styled (make-style #:bold #t #:foreground fg-bright-yellow)
                               "Confirm Delete"))
  (define body (render-styled (make-style #:foreground fg-white)
                              "Are you sure you want to delete\nthis file? Cannot be undone."))
  (define ok-btn (render-styled (make-style #:foreground fg-black #:background bg-bright-green)
                                "   OK   "))
  (define cancel-btn (render-styled (make-style #:foreground fg-white #:background bg-red)
                                    " Cancel "))
  (define buttons (join-horizontal top ok-btn "  " cancel-btn))
  (define content (join-vertical left title "" body "" buttons))
  (define styled-content (render-styled (make-style #:background bg-blue) content))
  (render-border styled-content border-rounded #:fg-color fg-bright-white))

;;; Build a tooltip
(define (build-tooltip text)
  (render-styled (make-style #:background bg-yellow #:foreground fg-black) (format " ~a " text)))

;;; Demo 1: Centered dialog overlay
(define (demo-static)
  (displayln "\n=== Demo 1: Centered Dialog Overlay ===\n")
  (define bg (build-list-background 60 15))
  (define dialog (build-dialog))
  (displayln (ov:composite dialog bg
                           #:x-position 'center
                           #:y-position 'middle))
  (newline))

;;; Demo 2: Multiple overlays
(define (demo-multiple)
  (displayln "\n=== Demo 2: Multiple Overlays with Offsets ===\n")
  (define bg (build-list-background 70 18))
  (define dialog (build-dialog))
  (define tooltip (build-tooltip "Press Enter to confirm"))
  (define with-dialog (ov:composite dialog bg
                                    #:x-position 'center
                                    #:y-position 'middle))
  (define result (ov:composite tooltip with-dialog
                               #:x-position 'right
                               #:y-position 'bottom
                               #:x-offset -2
                               #:y-offset -1))
  (displayln result)
  (newline))

;;; Demo 3: Corner positioning
(define (demo-corners)
  (displayln "\n=== Demo 3: Corner Positioning ===\n")
  (define bg (build-background 40 12))
  (define tl (build-tooltip "Top-Left"))
  (define tr (build-tooltip "Top-Right"))
  (define bl (build-tooltip "Bottom-Left"))
  (define br (build-tooltip "Bottom-Right"))
  (define center-tip (build-tooltip "CENTER"))
  (define r1 (ov:composite tl bg #:x-position 'left #:y-position 'top))
  (define r2 (ov:composite tr r1 #:x-position 'right #:y-position 'top))
  (define r3 (ov:composite bl r2 #:x-position 'left #:y-position 'bottom))
  (define r4 (ov:composite br r3 #:x-position 'right #:y-position 'bottom))
  (define result (ov:overlay-centered center-tip r4))
  (displayln result)
  (newline))

;;; Demo 4: Absolute positioning
(define (demo-absolute)
  (displayln "\n=== Demo 4: Absolute Positioning ===\n")
  (define bg (build-background 50 10))
  (define marker (build-tooltip "X"))
  (define r1 (ov:overlay-at marker bg 5 2))
  (define r2 (ov:overlay-at marker r1 20 4))
  (define r3 (ov:overlay-at marker r2 35 6))
  (define result (ov:overlay-at marker r3 45 8))
  (displayln result)
  (newline))

;;; Demo 5: Convenience functions
(define (demo-convenience)
  (displayln "\n=== Demo 5: Convenience Functions ===\n")
  (define bg (build-list-background 50 12))
  (define dialog (render-border
                  (render-styled (make-style #:background bg-magenta
                                             #:foreground fg-white)
                                 " overlay-centered ")
                  border-double
                  #:fg-color fg-bright-magenta))
  (displayln "Using overlay-centered:")
  (displayln (ov:overlay-centered dialog bg))
  (newline)
  (displayln "Using overlay-at (10, 3):")
  (displayln (ov:overlay-at dialog bg 10 3))
  (newline))

;;; Main
(module+ main
  (displayln "\n========================================")
  (displayln "       Kettle Overlay Demo")
  (displayln "========================================")
  (displayln "\nDemonstrating text compositing inspired by")
  (displayln "github.com/rmhubbert/bubbletea-overlay\n")

  (demo-static)
  (demo-multiple)
  (demo-corners)
  (demo-absolute)
  (demo-convenience)

  (displayln "========================================")
  (displayln "              Demo Complete")
  (displayln "========================================\n"))
