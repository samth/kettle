#lang racket/base

;; log-viewer.rkt -- High-performance log file viewer.
;; Optimized for large files (100K+ lines) with O(viewport-height) scroll operations.
;; Uses vectors for O(1) line access instead of lists.
;; Usage: racket log-viewer.rkt <filename>
;; Or: some-command | racket log-viewer.rkt

(require racket/format
         racket/list
         racket/match
         racket/port
         racket/string
         racket/file
         kettle/program
         kettle/image
         kettle/style)

(provide (struct-out log-viewer)
         make-log-viewer
         make-log-viewer-from-string)

(struct log-viewer
        (lines ; vector of strings (immutable after load)
         line-count ; cached (vector-length lines)
         max-line-w ; cached max line width
         filename ; display name
         y-offset ; scroll position (0-indexed)
         x-offset ; horizontal scroll position
         width
         height ; terminal dimensions
         gutter-w ; line number gutter width (digits + 1 for space)
         ;; Search state
         search-mode ; #f = normal, 'input = typing query, 'active = showing results
         search-query ; current search string
         search-matches ; sorted vector of matching line indices (or #f)
         search-current ; index into search-matches of current match (or #f)
         search-input) ; text being typed in search bar (before Enter)
  #:transparent
  #:methods gen:kettle-model
  [(define (init lv)
     lv)
   (define (update lv msg)
     (log-viewer-update lv msg))
   (define (view lv)
     (log-viewer-view lv))])

;; Compute gutter width: enough digits for the line count, plus 1 for separator space
(define (compute-gutter-w line-count)
  (+ 1 (string-length (number->string (max 1 line-count)))))

;; Compute max line width from a vector of strings
(define (compute-max-line-w lines-vec)
  (for/fold ([maxw 0]) ([line (in-vector lines-vec)])
    (max maxw (string-length line))))

(define (make-log-viewer lines-vec filename #:width [w 80] #:height [h 24])
  (define lc (vector-length lines-vec))
  (define maxw (compute-max-line-w lines-vec))
  (define gw (compute-gutter-w lc))
  (log-viewer lines-vec lc maxw filename 0 0 w h gw
              #f "" #f #f ""))

(define (make-log-viewer-from-string content filename #:width [w 80] #:height [h 24])
  (define lines-vec (list->vector (string-split content "\n" #:trim? #f)))
  (make-log-viewer lines-vec filename #:width w #:height h))

;;; Scrolling helpers -- all O(1)

(define (max-y-offset lv)
  (define view-h (- (log-viewer-height lv) 2)) ; reserve header + footer
  (max 0 (- (log-viewer-line-count lv) view-h)))

(define (clamp-y lv offset)
  (max 0 (min offset (max-y-offset lv))))

(define (max-x-offset lv)
  (define content-w (- (log-viewer-width lv) (log-viewer-gutter-w lv) 3)) ; gutter + " | "
  (max 0 (- (log-viewer-max-line-w lv) content-w)))

(define (clamp-x lv offset)
  (max 0 (min offset (max-x-offset lv))))

(define (scroll-percent lv)
  (define maxoff (max-y-offset lv))
  (if (<= maxoff 0)
      100
      (quotient (* 100 (log-viewer-y-offset lv)) maxoff)))

;;; Search

;; Find all line indices matching query (case-insensitive substring search).
;; Returns a sorted vector of indices.
(define (find-matches lv query)
  (cond
    [(string=? query "") #f]
    [else
     (define q (string-downcase query))
     (list->vector
      (for/list ([i (in-range (log-viewer-line-count lv))]
                 #:when (string-contains? (string-downcase (vector-ref (log-viewer-lines lv) i)) q))
        i))]))

;; Jump to the match at index match-idx within search-matches, scrolling to show it.
(define (jump-to-match lv match-idx)
  (define matches (log-viewer-search-matches lv))
  (cond
    [(or (not matches) (zero? (vector-length matches))) lv]
    [else
     (define idx (modulo match-idx (vector-length matches)))
     (define line-num (vector-ref matches idx))
     (define view-h (- (log-viewer-height lv) 2))
     ;; Center the match in the viewport
     (define target-y (clamp-y lv (max 0 (- line-num (quotient view-h 2)))))
     (struct-copy log-viewer lv
                  [y-offset target-y]
                  [search-current idx])]))

;; Find the next match at or after line-num. Returns match index or 0.
(define (find-match-at-or-after matches line-num)
  (define len (vector-length matches))
  (let loop ([lo 0] [hi len])
    (cond
      [(>= lo hi) (if (< lo len) lo 0)]
      [else
       (define mid (quotient (+ lo hi) 2))
       (if (< (vector-ref matches mid) line-num)
           (loop (add1 mid) hi)
           (loop lo mid))])))

;; Execute search: compute matches and jump to first match at or after current position.
(define (execute-search lv query)
  (define matches (find-matches lv query))
  (define lv2 (struct-copy log-viewer lv
                           [search-mode 'active]
                           [search-query query]
                           [search-matches matches]
                           [search-current #f]
                           [search-input ""]))
  (cond
    [(or (not matches) (zero? (vector-length matches))) lv2]
    [else
     (define start-idx (find-match-at-or-after matches (log-viewer-y-offset lv)))
     (jump-to-match lv2 start-idx)]))

;; Clear search state.
(define (clear-search lv)
  (struct-copy log-viewer lv
               [search-mode #f]
               [search-query ""]
               [search-matches #f]
               [search-current #f]
               [search-input ""]))

;;; Update

(define (log-viewer-update lv msg)
  (define mode (log-viewer-search-mode lv))
  (match msg
    ;; Key messages
    [(key-msg key alt? ctrl?)
     (cond
       ;; === Search input mode ===
       [(eq? mode 'input)
        (match key
          ;; Enter: execute search
          ['enter
           (define query (log-viewer-search-input lv))
           (if (string=? query "")
               (clear-search lv)
               (execute-search lv query))]
          ;; Escape: cancel search
          ['escape (clear-search lv)]
          ;; Backspace: delete char
          ['backspace
           (define inp (log-viewer-search-input lv))
           (if (string=? inp "")
               (clear-search lv)
               (struct-copy log-viewer lv
                            [search-input (substring inp 0 (sub1 (string-length inp)))]))]
          ;; Regular character
          [(? char? ch) #:when (and (>= (char->integer ch) 32) (not ctrl?) (not alt?))
           (struct-copy log-viewer lv
                        [search-input (string-append (log-viewer-search-input lv) (string ch))])]
          [_ lv])]

       ;; === Normal / active search mode ===
       [else
        (match key
          ;; Quit
          [#\q (cmd lv (quit-cmd))]

          ;; Enter search mode
          [#\/
           (struct-copy log-viewer lv
                        [search-mode 'input]
                        [search-input ""])]

          ;; Next match
          [#\n #:when (and (eq? mode 'active) (log-viewer-search-matches lv))
           (define matches (log-viewer-search-matches lv))
           (cond
             [(zero? (vector-length matches)) lv]
             [else
              (define cur (or (log-viewer-search-current lv) -1))
              (jump-to-match lv (add1 cur))])]

          ;; Previous match
          [#\N #:when (and (eq? mode 'active) (log-viewer-search-matches lv))
           (define matches (log-viewer-search-matches lv))
           (cond
             [(zero? (vector-length matches)) lv]
             [else
              (define cur (or (log-viewer-search-current lv) 1))
              (jump-to-match lv (sub1 cur))])]

          ;; Escape: clear search
          ['escape #:when (eq? mode 'active)
           (clear-search lv)]

          ;; Down / j
          [(or 'down #\j)
           (struct-copy log-viewer lv [y-offset (clamp-y lv (add1 (log-viewer-y-offset lv)))])]

          ;; Up / k
          [(or 'up #\k)
           (struct-copy log-viewer lv [y-offset (clamp-y lv (sub1 (log-viewer-y-offset lv)))])]

          ;; Page down / space
          [(or #\space 'page-down)
           (define page-h (- (log-viewer-height lv) 2))
           (struct-copy log-viewer lv [y-offset (clamp-y lv (+ (log-viewer-y-offset lv) page-h))])]

          ;; Page up / b
          [(or #\b 'page-up)
           (define page-h (- (log-viewer-height lv) 2))
           (struct-copy log-viewer lv [y-offset (clamp-y lv (- (log-viewer-y-offset lv) page-h))])]

          ;; Top / g
          [#\g (struct-copy log-viewer lv [y-offset 0])]

          ;; Bottom / G
          [#\G (struct-copy log-viewer lv [y-offset (max-y-offset lv)])]

          ;; Left / h
          [(or 'left #\h)
           (struct-copy log-viewer lv [x-offset (clamp-x lv (- (log-viewer-x-offset lv) 4))])]

          ;; Right / l
          [(or 'right #\l)
           (struct-copy log-viewer lv [x-offset (clamp-x lv (+ (log-viewer-x-offset lv) 4))])]

          [_ lv])])]

    ;; Window resize
    [(window-size-msg w h)
     (define new-lv (struct-copy log-viewer lv [width w] [height h]))
     ;; Re-clamp offsets for new size
     (struct-copy log-viewer
                  new-lv
                  [y-offset (clamp-y new-lv (log-viewer-y-offset new-lv))]
                  [x-offset (clamp-x new-lv (log-viewer-x-offset new-lv))])]

    [_ lv]))

;;; View -- O(viewport-height)

(define (pad-line-number n gutter-w)
  (~a (add1 n) #:min-width (sub1 gutter-w) #:align 'right))

(define (visible-portion line x-offset content-w)
  (define len (string-length line))
  (define start (min x-offset len))
  (define end (min (+ start content-w) len))
  (if (>= start len)
      ""
      (substring line start end)))

;; Check if a line index is in the search matches.
(define (line-matches? lv line-idx)
  (define matches (log-viewer-search-matches lv))
  (and matches
       (> (vector-length matches) 0)
       ;; Binary search
       (let loop ([lo 0] [hi (vector-length matches)])
         (and (< lo hi)
              (let ([mid (quotient (+ lo hi) 2)])
                (define v (vector-ref matches mid))
                (cond
                  [(= v line-idx) #t]
                  [(< v line-idx) (loop (add1 mid) hi)]
                  [else (loop lo mid)]))))))

;; Is this the current match?
(define (current-match? lv line-idx)
  (define matches (log-viewer-search-matches lv))
  (define cur (log-viewer-search-current lv))
  (and matches cur
       (< cur (vector-length matches))
       (= (vector-ref matches cur) line-idx)))

;; Highlight search term occurrences within visible text.
;; Returns an image with the query highlighted.
(define highlight-style (make-style #:reverse #t #:bold #t))
(define current-highlight-style (make-style #:foreground "0" #:background "3" #:bold #t))

(define (highlight-text content query is-current?)
  (if (or (string=? query "") (string=? content ""))
      (text content)
      (let ([q-lower (string-downcase query)]
            [qlen (string-length query)])
        (define parts
          (let loop ([start 0] [acc '()])
            (define pos (string-contains-ci content q-lower start))
            (cond
              [(not pos)
               (reverse (cons (text (substring content start)) acc))]
              [else
               (define before (substring content start pos))
               (define match-str (substring content pos (+ pos qlen)))
               (define sty (if is-current? current-highlight-style highlight-style))
               (loop (+ pos qlen)
                     (cons (styled sty (text match-str))
                           (if (string=? before "")
                               acc
                               (cons (text before) acc))))])))
        (if (= (length parts) 1)
            (car parts)
            (apply hcat 'top parts)))))

;; Case-insensitive string-contains with start offset.
(define (string-contains-ci haystack needle [start 0])
  (define hlen (string-length haystack))
  (define nlen (string-length needle))
  (and (<= (+ start nlen) hlen)
       (let loop ([i start])
         (cond
           [(> (+ i nlen) hlen) #f]
           [(string=? (string-downcase (substring haystack i (+ i nlen))) needle) i]
           [else (loop (add1 i))]))))

(define (log-viewer-view lv)
  (define w (log-viewer-width lv))
  (define h (log-viewer-height lv))
  (define view-h (- h 2)) ; header + footer
  (define lines (log-viewer-lines lv))
  (define line-count (log-viewer-line-count lv))
  (define y-off (log-viewer-y-offset lv))
  (define x-off (log-viewer-x-offset lv))
  (define gw (log-viewer-gutter-w lv))
  (define content-w (max 1 (- w gw 3))) ; " | " separator
  (define mode (log-viewer-search-mode lv))
  (define query (log-viewer-search-query lv))
  (define matches (log-viewer-search-matches lv))
  (define match-count (if (and matches (> (vector-length matches) 0))
                          (vector-length matches)
                          0))

  (define visible-start y-off)
  (define visible-end (min (+ visible-start view-h) line-count))
  (define actual-visible (- visible-end visible-start))

  (define header-style (make-style #:bold #t #:reverse #t))
  (define footer-style (make-style #:faint #t))
  (define gutter-style (make-style #:faint #t))
  (define match-gutter-style (make-style #:foreground "3" #:bold #t))

  (define fname (log-viewer-filename lv))
  (define pct (scroll-percent lv))

  ;; Header
  (define header-text
    (let* ([base (format " ~a  (~a lines)" fname line-count)]
           [search-info (if (and (eq? mode 'active) (> match-count 0))
                            (format "  [~a/~a matches]"
                                    (if (log-viewer-search-current lv)
                                        (add1 (log-viewer-search-current lv))
                                        0)
                                    match-count)
                            (if (and (eq? mode 'active) (= match-count 0))
                                "  [no matches]"
                                ""))]
           [s (string-append base search-info " ")])
      (if (> (string-length s) w)
          (substring s 0 w)
          s)))

  ;; Content lines -- O(view-h)
  (define searching? (and (eq? mode 'active) (not (string=? query ""))))
  (define line-images
    (for/list ([i (in-range visible-start visible-end)])
      (define line (vector-ref lines i)) ; O(1)
      (define num-str (pad-line-number i gw))
      (define content (visible-portion line x-off content-w))
      (define is-match (and searching? (line-matches? lv i)))
      (define is-current (and searching? (current-match? lv i)))
      (define gutter-img (styled (if is-match match-gutter-style gutter-style) (text num-str)))
      (define content-img
        (if (and searching? is-match)
            (highlight-text content query is-current)
            (text content)))
      (hcat 'top gutter-img (text " | ") content-img)))

  ;; Padding for short files
  (define pad-images (make-list (max 0 (- view-h actual-visible)) (text "")))

  ;; Footer
  (define footer-img
    (cond
      ;; Search input mode: show search bar
      [(eq? mode 'input)
       (define input-str (log-viewer-search-input lv))
       (define cursor-style (make-style #:reverse #t))
       (hcat 'top
             (styled footer-style (text "/"))
             (text input-str)
             (styled cursor-style (text " ")))]
      ;; Active search: show search info in footer
      [(eq? mode 'active)
       (define info
         (format " /~a  n/N:next/prev  Esc:clear  ~a%  q:quit" query pct))
       (styled footer-style (text (if (> (string-length info) w) (substring info 0 w) info)))]
      ;; Normal mode
      [else
       (define s
         (format " ~a%  j/k:scroll  space/b:page  g/G:top/bottom  /:search  q:quit" pct))
       (styled footer-style (text (if (> (string-length s) w) (substring s 0 w) s)))]))

  (apply vcat
         'left
         (styled header-style (text header-text))
         (append line-images pad-images (list footer-img))))

(module+ main
  (define args (current-command-line-arguments))
  (define-values (content filename)
    (cond
      [(> (vector-length args) 0)
       (define path (vector-ref args 0))
       (values (file->string path) path)]
      [else (values (port->string (current-input-port)) "<stdin>")]))
  (define lv (make-log-viewer-from-string content filename))
  (define p (make-program lv #:alt-screen #t))
  (program-run p))
