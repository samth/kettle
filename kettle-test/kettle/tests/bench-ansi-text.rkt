#lang racket/base

;; bench-ansi-text.rkt -- ANSI text processing benchmarks.
;;
;; Adapted from Bun's slice-ansi benchmark (bench/snippets/slice-ansi.mjs)
;; which compares Bun.sliceAnsi against npm slice-ansi and cli-truncate.
;;
;; We benchmark Kettle's equivalent operations:
;;   visible-length  — ANSI-aware display width measurement
;;   truncate-text   — ANSI-aware truncation with ellipsis (≈ cli-truncate)
;;   paint-text!     — write ANSI string into cell buffer (the TUI hot path)
;;
;; The JS benchmark covers four tiers:
;;   1. Pure ASCII            — fast path
;;   2. ASCII + ANSI codes    — escape sequence skipping
;;   3. CJK text              — width-2 characters
;;   4. Grapheme clustering   — emoji, ZWJ, combining marks
;;
;; Run: racket -y kettle-test/kettle/tests/bench-ansi-text.rkt

(require racket/format
         racket/string
         racket/list
         kettle)

;;; ---------- Timing infrastructure ----------

(define-syntax-rule (time-ns body ...)
  (let ([start (current-inexact-milliseconds)])
    body ...
    (* (- (current-inexact-milliseconds) start) 1e6)))

;; Run body N times, return median nanoseconds
(define (bench-median n thunk)
  (define times
    (for/list ([_ (in-range n)])
      (time-ns (thunk))))
  (define sorted (sort times <))
  (list-ref sorted (quotient (length sorted) 2)))

;; Run body repeatedly for ~warmup-ms, then measure N iterations
(define (bench name n thunk)
  ;; Warmup
  (for ([_ (in-range (min 100 n))])
    (thunk))
  ;; Measure
  (define median-ns (bench-median n thunk))
  (define per-op-ns median-ns)
  (define per-op-us (/ per-op-ns 1000.0))
  (cond
    [(< per-op-us 1.0) (printf "  ~a: ~ans\n" name (~r per-op-ns #:precision '(= 0)))]
    [(< per-op-us 1000.0) (printf "  ~a: ~aus\n" name (~r per-op-us #:precision '(= 1)))]
    [else (printf "  ~a: ~ams\n" name (~r (/ per-op-us 1000.0) #:precision '(= 2)))]))

;;; ---------- ANSI helpers ----------

(define (red s)
  (format "\x1b[31m~a\x1b[39m" s))
(define (green s)
  (format "\x1b[32m~a\x1b[39m" s))
(define (ansi-bold s)
  (format "\x1b[1m~a\x1b[22m" s))
(define (truecolor r g b s)
  (format "\x1b[38;2;~a;~a;~am~a\x1b[39m" r g b s))

;;; ---------- Fixtures (matching Bun benchmark tiers) ----------

;; Tier 1: Pure ASCII
(define ascii-short "The quick brown fox jumps over the lazy dog.")
(define ascii-long
  (apply string-append (make-list 100 "The quick brown fox jumps over the lazy dog. ")))

;; Tier 2: ASCII + ANSI codes
(define ansi-short
  (format "The ~a ~a fox ~a over the lazy dog." (red "quick") (green "brown") (ansi-bold "jumps")))
(define ansi-medium
  (apply string-append
         (make-list 10
                    (format "The ~a jumps ~a and ~a. "
                            (red "quick brown fox")
                            (green "over the lazy dog")
                            (ansi-bold "runs away")))))
(define ansi-long
  (apply string-append
         (make-list 100
                    (format "The ~a jumps ~a and ~a. "
                            (red "quick brown fox")
                            (green "over the lazy dog")
                            (ansi-bold "runs away")))))
;; Dense ANSI: SGR between every few chars
(define ansi-dense
  (apply string-append
         (make-list
          50
          (string-append (red "ab") (green "cd") (ansi-bold "ef") (truecolor 255 128 64 "gh")))))

;; Tier 3: CJK (width 2, no clustering)
(define cjk (apply string-append (make-list 10 "日本語のテキストをスライスするテストです。全角文字は幅2としてカウントされます。")))
(define cjk-ansi
  (string-append (red "日本語のテキストを") (green "スライスするテスト") (apply string-append (make-list 10 "です。"))))

;; Tier 4: Emoji and grapheme clustering
(define emoji (apply string-append (make-list 10 "Hello 👋 World 🌍! Test 🧪 emoji 😀 slicing 📦!")))
;; ZWJ family emoji
(define zwj (apply string-append (make-list 20 "Family: 👨‍👩‍👧‍👦 and 👩‍💻 technologist! ")))
;; Skin tone modifiers
(define skin-tone (apply string-append (make-list 20 "Wave 👋🏽 handshake 🤝🏻 thumbs 👍🏿 ok 👌🏼!")))
;; Combining marks
(define combining
  (apply string-append (make-list 30 "cafe\u0301 re\u0301sume\u0301 na\u0131\u0308ve pi\u00f1ata ")))

;;; ---------- Benchmark suites ----------

(define ITERS 500)

(define (run-benchmarks)
  (printf "Kettle ANSI Text Processing Benchmarks (~a iterations, median)\n" ITERS)
  (printf "Comparable to Bun's slice-ansi.mjs benchmark\n\n")

  ;; === visible-length (≈ npm string-width) ===
  (printf "visible-length (ANSI-aware width measurement):\n")
  (bench "ascii-short (44 chars)" ITERS (lambda () (visible-length ascii-short)))
  (bench "ascii-long  (4500 chars)" ITERS (lambda () (visible-length ascii-long)))
  (bench "ansi-short  (44 vis + escapes)" ITERS (lambda () (visible-length ansi-short)))
  (bench "ansi-medium (10x repeat)" ITERS (lambda () (visible-length ansi-medium)))
  (bench "ansi-long   (100x repeat)" ITERS (lambda () (visible-length ansi-long)))
  (bench "ansi-dense  (SGR every 2 chars)" ITERS (lambda () (visible-length ansi-dense)))
  (bench "cjk         (wide chars)" ITERS (lambda () (visible-length cjk)))
  (bench "cjk+ansi    (wide + escapes)" ITERS (lambda () (visible-length cjk-ansi)))
  (bench "emoji       (basic emoji)" ITERS (lambda () (visible-length emoji)))
  (bench "zwj         (ZWJ sequences)" ITERS (lambda () (visible-length zwj)))
  (bench "skin-tone   (modified emoji)" ITERS (lambda () (visible-length skin-tone)))
  (bench "combining   (combining marks)" ITERS (lambda () (visible-length combining)))
  (newline)

  ;; === truncate-text (≈ npm cli-truncate / Bun.sliceAnsi with ellipsis) ===
  (printf "truncate-text (ANSI-aware truncation with ellipsis):\n")
  (bench "ascii-short  trunc to 20" ITERS (lambda () (truncate-text ascii-short 20)))
  (bench "ascii-long   trunc to 200" ITERS (lambda () (truncate-text ascii-long 200)))
  (bench "ansi-short   trunc to 30" ITERS (lambda () (truncate-text ansi-short 30)))
  (bench "ansi-long    trunc to 200" ITERS (lambda () (truncate-text ansi-long 200)))
  (bench "ansi-dense   trunc to 100" ITERS (lambda () (truncate-text ansi-dense 100)))
  (bench "cjk          trunc to 100" ITERS (lambda () (truncate-text cjk 100)))
  (bench "cjk+ansi     trunc to 100" ITERS (lambda () (truncate-text cjk-ansi 100)))
  (bench "emoji        trunc to 100" ITERS (lambda () (truncate-text emoji 100)))
  (bench "zwj          trunc to 100" ITERS (lambda () (truncate-text zwj 100)))
  (bench "combining    trunc to 100" ITERS (lambda () (truncate-text combining 100)))
  ;; No-cut case: string already fits
  (bench "ascii-short  no-cut (fits)" ITERS (lambda () (truncate-text ascii-short 100)))
  ;; Ink-style viewport clipping
  (define log-line
    (string-append (ansi-bold "[2024-01-15 12:34:56]")
                   " "
                   (red "ERROR")
                   " Connection to api.example.com timed out after 30s (attempt 3/5)"))
  (bench "ink-clip     80-col viewport" ITERS (lambda () (truncate-text log-line 80)))
  (newline)

  ;; === paint! text images (cell buffer painting — the actual TUI hot path) ===
  ;; This is Kettle's unique operation: walking an ANSI string into a 2D cell grid.
  ;; JS TUI frameworks do this too (ink's output.ts), but it's not what slice-ansi
  ;; measures — slice-ansi produces a new string, while paint! fills a buffer.
  ;; We pre-build text images (which measures width once via visible-length at
  ;; construction), then benchmark just the paint! step.
  (printf "paint! text (ANSI string -> cell buffer, TUI hot path):\n")
  (define buf-wide (make-cell-buffer 200 1))
  (define buf-5000 (make-cell-buffer 5000 1))
  ;; Pre-build text images (width measurement happens here, not in paint!)
  (define img-ascii-short (text ascii-short))
  (define img-ascii-long (text ascii-long))
  (define img-ansi-short (text ansi-short))
  (define img-ansi-medium (text ansi-medium))
  (define img-ansi-long (text ansi-long))
  (define img-ansi-dense (text ansi-dense))
  (define img-cjk (text cjk))
  (define img-cjk-ansi (text cjk-ansi))
  (define img-emoji (text emoji))
  (define img-zwj (text zwj))
  (define img-skin-tone (text skin-tone))
  (define img-combining (text combining))
  (bench "ascii-short  (44 chars)" ITERS (lambda () (paint! buf-wide img-ascii-short 0 0)))
  (bench "ascii-long   (4500 chars)" ITERS (lambda () (paint! buf-5000 img-ascii-long 0 0)))
  (bench "ansi-short   (with escapes)" ITERS (lambda () (paint! buf-wide img-ansi-short 0 0)))
  (bench "ansi-medium  (10x repeat)" ITERS (lambda () (paint! buf-5000 img-ansi-medium 0 0)))
  (bench "ansi-long    (100x repeat)" ITERS (lambda () (paint! buf-5000 img-ansi-long 0 0)))
  (bench "ansi-dense   (SGR every 2)" ITERS (lambda () (paint! buf-5000 img-ansi-dense 0 0)))
  (bench "cjk          (wide chars)" ITERS (lambda () (paint! buf-5000 img-cjk 0 0)))
  (bench "cjk+ansi     (wide + escapes)" ITERS (lambda () (paint! buf-5000 img-cjk-ansi 0 0)))
  (bench "emoji        (basic emoji)" ITERS (lambda () (paint! buf-5000 img-emoji 0 0)))
  (bench "zwj          (ZWJ sequences)" ITERS (lambda () (paint! buf-5000 img-zwj 0 0)))
  (bench "skin-tone    (modified emoji)" ITERS (lambda () (paint! buf-5000 img-skin-tone 0 0)))
  (bench "combining    (combining marks)" ITERS (lambda () (paint! buf-5000 img-combining 0 0)))
  ;; With a parent style applied (common case in styled TUI)
  (define parent-style (make-style #:bold #t #:foreground fg-cyan))
  (define img-ansi-long-styled (styled parent-style img-ansi-long))
  (bench "ansi-long+sty (styled parent)"
         ITERS
         (lambda () (paint! buf-5000 img-ansi-long-styled 0 0)))
  (newline)

  ;; === wrap-text (word wrapping — used in text components) ===
  (printf "wrap-text (ANSI-aware word wrapping):\n")
  (define prose
    (string-append "The quick brown fox jumps over the lazy dog. "
                   "Pack my box with five dozen liquor jugs. "
                   "How vexingly quick daft zebras jump! "
                   "The five boxing wizards jump quickly. "))
  (define prose-long (apply string-append (make-list 20 prose)))
  (define prose-ansi
    (apply
     string-append
     (make-list 20 (format "The ~a jumps over the ~a. " (red "quick brown fox") (green "lazy dog")))))
  (bench "prose 60col     (plain, 176 chars)" ITERS (lambda () (wrap-text prose 60)))
  (bench "prose-long 60col (plain, 3520 chars)" ITERS (lambda () (wrap-text prose-long 60)))
  (bench "prose-ansi 60col (ANSI, 3600+ chars)" ITERS (lambda () (wrap-text prose-ansi 60)))
  (bench "cjk 40col       (wide chars)" ITERS (lambda () (wrap-text cjk 40)))
  (newline)

  ;; === char-display-width (per-character width lookup) ===
  (printf "char-display-width (single character width lookup):\n")
  (bench "ASCII 'A'"
         ITERS
         (lambda ()
           (for ([_ (in-range 1000)])
             (char-display-width #\A))))
  (bench "CJK '日'"
         ITERS
         (lambda ()
           (for ([_ (in-range 1000)])
             (char-display-width #\日))))
  (bench "Emoji '👋'"
         ITERS
         (lambda ()
           (for ([_ (in-range 1000)])
             (char-display-width #\👋))))
  (bench "Combining '\u0301'"
         ITERS
         (lambda ()
           (for ([_ (in-range 1000)])
             (char-display-width #\u0301))))
  (printf "  (times above are for 1000 lookups each)\n")
  (newline)

  ;; === Summary: string sizes and throughput ===
  (printf "Input sizes (string-length / visible-length):\n")
  (for ([pair (list (cons "ascii-short" ascii-short)
                    (cons "ascii-long" ascii-long)
                    (cons "ansi-short" ansi-short)
                    (cons "ansi-medium" ansi-medium)
                    (cons "ansi-long" ansi-long)
                    (cons "ansi-dense" ansi-dense)
                    (cons "cjk" cjk)
                    (cons "cjk+ansi" cjk-ansi)
                    (cons "emoji" emoji)
                    (cons "zwj" zwj)
                    (cons "skin-tone" skin-tone)
                    (cons "combining" combining))])
    (printf "  ~a: ~a bytes / ~a display cols\n"
            (~a (car pair) #:width 12 #:align 'left)
            (~a (string-length (cdr pair)) #:width 6 #:align 'right)
            (visible-length (cdr pair)))))

(module+ main
  (run-benchmarks))

(module+ test
  (require rackunit)
  (run-benchmarks))
