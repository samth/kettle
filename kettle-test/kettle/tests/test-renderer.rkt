#lang racket/base

;; test-renderer.rkt -- Unit tests for the renderer and image->string pipeline.
;; Tests that style attributes are correctly translated to ANSI output.

(require rackunit
         racket/string
         kettle)

(provide (all-defined-out))

;;; ============================================================
;;; Style rendering through image->string
;;; ============================================================

(test-case "renderer: plain text"
  (define s (image->string (text "hello")))
  (check-true (string-contains? s "hello")))

(test-case "renderer: bold text"
  (define s (image->string (styled (make-style #:bold #t) "test")))
  ;; SGR 1 = bold
  (check-true (string-contains? s "\e[") "should contain ANSI escape")
  (check-true (string-contains? s "1") "should contain bold SGR code")
  (check-true (string-contains? s "test")))

(test-case "renderer: reverse style swaps fg and bg"
  ;; reverse with default colors (fg=7 white, bg=0 black) should produce
  ;; fg=0 (30) and bg=7 (47) in the output
  (define s (image->string (styled (make-style #:reverse #t) "rev")))
  (check-true (string-contains? s "30") "reversed fg should be 30 (black)")
  (check-true (string-contains? s "47") "reversed bg should be 47 (white)"))

(test-case "renderer: reverse style with custom colors"
  ;; fg=red (1 -> SGR 31), bg=blue (4 -> SGR 44)
  ;; reversed: fg=blue (4 -> SGR 34), bg=red (1 -> SGR 41)
  (define s (image->string (styled (make-style #:foreground fg-red
                                               #:background fg-blue
                                               #:reverse #t)
                                   "x")))
  (check-true (string-contains? s "34") "reversed: blue fg should be 34")
  (check-true (string-contains? s "41") "reversed: red bg should be 41"))

(test-case "renderer: non-reverse keeps normal fg/bg"
  ;; fg=red (1 -> SGR 31), bg=blue (4 -> SGR 44), no reverse
  (define s (image->string (styled (make-style #:foreground fg-red
                                               #:background fg-blue)
                                   "x")))
  (check-true (string-contains? s "31") "normal: red fg should be 31")
  (check-true (string-contains? s "44") "normal: blue bg should be 44"))

(test-case "renderer: italic text"
  (define s (image->string (styled (make-style #:italic #t) "it")))
  ;; SGR 3 = italic
  (check-true (string-contains? s "3") "should contain italic SGR code"))

(test-case "renderer: underline text"
  (define s (image->string (styled (make-style #:underline #t) "ul")))
  ;; ubuf underline produces 'single
  (check-true (string-contains? s "4") "should contain underline SGR code"))

(test-case "renderer: faint text uses faint attribute"
  ;; faint style should produce SGR 2
  (define s (image->string (styled (make-style #:faint #t) "dim")))
  (check-true (string-contains? s "dim")))

(test-case "renderer: combined bold+reverse"
  ;; bold+reverse: should have bold SGR and swapped colors
  (define s (image->string (styled (make-style #:bold #t #:reverse #t) "br")))
  (check-true (string-contains? s "30") "bold+reverse: swapped fg should be 30")
  (check-true (string-contains? s "47") "bold+reverse: swapped bg should be 47")
  (check-true (string-contains? s "1") "bold+reverse: should have bold code"))

;;; ============================================================
;;; Grapheme cluster support
;;; ============================================================

;; visible-length

(test-case "visible-length: plain ASCII"
  (check-equal? (visible-length "hello") 5))

(test-case "visible-length: CJK wide characters"
  (check-equal? (visible-length "日本") 4))

(test-case "visible-length: combining marks"
  ;; "café" = c-a-f-e-\u0301 — the accent doesn't add width
  (check-equal? (visible-length "cafe\u0301") 4)
  ;; "résumé" = r-e-\u0301-s-u-m-e-\u0301
  (check-equal? (visible-length "re\u0301sume\u0301") 6)
  ;; naïve = n-a-\u0131-\u0308-v-e (dotless i + combining diaeresis)
  (check-equal? (visible-length "na\u0131\u0308ve") 5))

(test-case "visible-length: ZWJ emoji sequences"
  ;; 👨‍👩‍👧‍👦 = man ZWJ woman ZWJ girl ZWJ boy — one cluster, width 2
  (check-equal? (visible-length "👨\u200D👩\u200D👧\u200D👦") 2)
  ;; 👩‍💻 = woman ZWJ laptop — one cluster, width 2
  (check-equal? (visible-length "👩\u200D💻") 2))

(test-case "visible-length: skin-tone modifiers"
  ;; 👋🏽 = wave + medium skin tone — one cluster, width 2
  (check-equal? (visible-length "👋🏽") 2)
  ;; 👍🏿 = thumbs up + dark skin tone
  (check-equal? (visible-length "👍🏿") 2))

(test-case "visible-length: mixed text with grapheme clusters"
  ;; "Hi 👋🏽!" = H(1) i(1) space(1) wave-skin(2) !(1) = 6
  (check-equal? (visible-length "Hi 👋🏽!") 6)
  ;; "café 👨‍👩‍👧‍👦" = c(1) a(1) f(1) é(1) space(1) family(2) = 7
  (check-equal? (visible-length "cafe\u0301 👨\u200D👩\u200D👧\u200D👦") 7))

(test-case "visible-length: ANSI escapes with grapheme clusters"
  ;; ANSI codes should be skipped, grapheme clusters counted correctly
  (check-equal? (visible-length "\x1b[31mcafe\u0301\x1b[0m") 4)
  (check-equal? (visible-length "\x1b[1m👨\u200D👩\u200D👧\u200D👦\x1b[22m") 2))

;; truncate-text

(test-case "truncate-text: no truncation returns original string"
  (define s "hello")
  (check-eq? (truncate-text s 10) s "should return same string object"))

(test-case "truncate-text: combining marks not split"
  ;; "café" = width 4, truncate to 3 should give "caf…" not "café" split mid-cluster
  (check-equal? (truncate-text "cafe\u0301" 4 #:ellipsis "…") "caf…")
  ;; truncate to 5 should return whole string (width 4 + 1 for ellipsis fits in 5)
  (check-equal? (truncate-text "cafe\u0301" 5 #:ellipsis "…") "cafe\u0301"))

(test-case "truncate-text: ZWJ emoji not split"
  ;; "👨‍👩‍👧‍👦 hi" has family(2) space(1) h(1) i(1) = 5
  (define s "👨\u200D👩\u200D👧\u200D👦 hi")
  ;; Truncating to 4: family(2) + space(1) fits in budget=3 (4-1 for ellipsis)
  (check-equal? (truncate-text s 4 #:ellipsis "…") "👨\u200D👩\u200D👧\u200D👦 …")
  ;; Truncating to 2: family(2) doesn't fit in budget=1
  (check-equal? (truncate-text s 2 #:ellipsis "…") "…"))

(test-case "truncate-text: skin-tone emoji not split"
  ;; "👋🏽!" has wave-skin(2) !(1) = 3
  ;; Truncating to 3: wave-skin(2) fits in budget=2, !(1) doesn't → "👋🏽…"
  (check-equal? (truncate-text "👋🏽!" 3 #:ellipsis "…") "👋🏽…"))

(test-case "truncate-text: ANSI preserved with grapheme clusters"
  (define s "\x1b[31mcafe\u0301\x1b[0m")
  ;; Width is 4. Truncating to 4 (budget=3) should cut before the é cluster.
  (check-equal? (truncate-text s 4 #:ellipsis "…") "\x1b[31mcaf…"))

;; image dimensions with grapheme clusters

(test-case "text image: width accounts for grapheme clusters"
  (define img (text "cafe\u0301"))
  (check-equal? (image-w img) 4)
  (check-equal? (image-h img) 1))

(test-case "text image: ZWJ emoji width"
  (define img (text "👨\u200D👩\u200D👧\u200D👦"))
  (check-equal? (image-w img) 2)
  (check-equal? (image-h img) 1))

(test-case "text image: skin-tone emoji width"
  (define img (text "👋🏽"))
  (check-equal? (image-w img) 2)
  (check-equal? (image-h img) 1))
