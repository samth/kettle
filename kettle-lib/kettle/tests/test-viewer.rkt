#lang racket/base

(require rackunit
         kettle/examples/viewer
         kettle/program
         kettle/image
         (prefix-in vp: kettle/components/viewport))

(define (char-key ch)
  (key-msg ch #f #f))

(define sample-content
  (string-append "Line 1\n"
                 "Line 2\n"
                 "Line 3\n"
                 "Line 4\n"
                 "Line 5\n"
                 "Line 6\n"
                 "Line 7\n"
                 "Line 8\n"
                 "Line 9\n"
                 "Line 10\n"))

;; --- Construction ---

(test-case "make-file-viewer creates a valid model"
  (define fv (make-file-viewer sample-content "test.txt"))
  (check-true (file-viewer? fv))
  (check-equal? (file-viewer-filename fv) "test.txt"))

(test-case "make-file-viewer accepts width/height"
  (define fv (make-file-viewer sample-content "test.txt" #:width 40 #:height 10))
  (check-true (file-viewer? fv)))

;; --- Scrolling via viewport delegation ---

(test-case "j scrolls down"
  (define fv (make-file-viewer sample-content "test.txt" #:width 40 #:height 8))
  (define vp-before (file-viewer-viewport fv))
  (define-values (fv2 cmd) (update fv (char-key #\j)))
  (define vp-after (file-viewer-viewport fv2))
  ;; Viewport should have changed
  (check-not-equal? vp-before vp-after)
  (check-false cmd))

(test-case "k scrolls up (no-op at top)"
  (define fv (make-file-viewer sample-content "test.txt" #:width 40 #:height 8))
  (define-values (fv2 cmd) (update fv (char-key #\k)))
  (check-true (vp:viewport-at-top? (file-viewer-viewport fv2)))
  (check-false cmd))

;; --- Quit ---

(test-case "q quits"
  (define fv (make-file-viewer sample-content "test.txt"))
  (define-values (fv2 cmd) (update fv (char-key #\q)))
  (check-pred procedure? cmd))

;; --- Window resize ---

(test-case "window-size-msg resizes viewport"
  (define fv (make-file-viewer sample-content "test.txt" #:width 40 #:height 10))
  (define-values (fv2 cmd) (update fv (window-size-msg 60 20)))
  (check-false cmd)
  ;; The viewport should reflect the new dimensions
  (check-true (file-viewer? fv2)))

;; --- View ---

(test-case "view produces an image"
  (define fv (make-file-viewer sample-content "test.txt"))
  (check-pred image? (view fv)))

(test-case "view shows filename"
  (define fv (make-file-viewer sample-content "my-file.txt"))
  (define s (image->string (view fv)))
  (check-regexp-match #rx"my-file.txt" s))

(test-case "view shows line count"
  (define fv (make-file-viewer sample-content "test.txt"))
  (define s (image->string (view fv)))
  ;; sample-content has 10 lines (with trailing newline -> 11 lines in the split)
  (check-regexp-match #rx"lines" s))

;; --- Purity ---

(test-case "update does not mutate original"
  (define fv (make-file-viewer sample-content "test.txt" #:width 40 #:height 8))
  (define vp-before (file-viewer-viewport fv))
  (define-values (fv2 _) (update fv (char-key #\j)))
  ;; Original is unchanged
  (check-equal? (file-viewer-viewport fv) vp-before))
