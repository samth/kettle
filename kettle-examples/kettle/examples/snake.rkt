#lang racket/base

;; snake.rkt -- Classic snake game.
;; Original Kettle example (no cl-tuition counterpart).
;;
;; Demonstrates capabilities beyond cl-tuition's example set: real-time game loop
;; via Elm-style subscriptions (every 0.15 tick-msg), window-size-msg for
;; responsive playfield sizing, and define-kettle-program with #:init for
;; one-time random food placement.
;;
;; Demonstrates subscriptions (every), collision detection, random food placement.
;; Arrow keys or hjkl to move, r to restart, q to quit.

(require racket/match
         racket/list
         racket/format
         kettle)

(provide (struct-out snake-game)
         make-snake-game)

(define-kettle-program
 snake-game
 #:fields ([body '((5 . 5) (4 . 5) (3 . 5))] ;; list of (col . row) pairs, head first
           [dir 'right] ;; 'up 'down 'left 'right
           [food #f] ;; (col . row) or #f
           [score 0]
           [state 'playing] ;; 'playing or 'game-over
           [width 40] ;; playfield width
           [height 20]) ;; playfield height
 #:init (lambda (self)
          (if (snake-game-food self)
              self
              (struct-copy snake-game self [food (random-food self)])))
 #:update
 (lambda (self msg)
   (match msg
     ;; Tick: advance the snake
     [(tick-msg _)
      #:when (eq? (snake-game-state self) 'playing)
      (advance self)]

     ;; Direction keys (prevent reversing into self)
     [(key-msg (or 'up #\k) _ _)
      #:when (and (eq? (snake-game-state self) 'playing) (not (eq? (snake-game-dir self) 'down)))
      (struct-copy snake-game self [dir 'up])]

     [(key-msg (or 'down #\j) _ _)
      #:when (and (eq? (snake-game-state self) 'playing) (not (eq? (snake-game-dir self) 'up)))
      (struct-copy snake-game self [dir 'down])]

     [(key-msg (or 'left #\h) _ _)
      #:when (and (eq? (snake-game-state self) 'playing) (not (eq? (snake-game-dir self) 'right)))
      (struct-copy snake-game self [dir 'left])]

     [(key-msg (or 'right #\l) _ _)
      #:when (and (eq? (snake-game-state self) 'playing) (not (eq? (snake-game-dir self) 'left)))
      (struct-copy snake-game self [dir 'right])]

     ;; r restarts
     [(key-msg #\r _ _)
      (init (struct-copy snake-game
                         (make-snake-game)
                         [width (snake-game-width self)]
                         [height (snake-game-height self)]))]

     ;; q quits
     [(key-msg #\q _ _) (cmd self (quit-cmd))]

     ;; Resize
     [(window-size-msg w h)
      ;; Adjust playfield to terminal size (leave room for border + header + footer)
      (define new-w (max 10 (- w 2)))
      (define new-h (max 5 (- h 4)))
      (struct-copy snake-game self [width new-w] [height new-h])]

     [_ self]))
 #:subscriptions (lambda (self)
                   (if (eq? (snake-game-state self) 'playing)
                       (list (every 0.08 tick-msg))
                       '()))
 #:view (lambda (self)
          (define w (snake-game-width self))
          (define h (snake-game-height self))
          (define body (snake-game-body self))
          (define food (snake-game-food self))
          (define sc (snake-game-score self))

          ;; Build each row of the playfield
          (define rows
            (for/list ([r (in-range h)])
              (define row-str
                (list->string (for/list ([c (in-range w)])
                                (define pos (cons c r))
                                (cond
                                  [(and food (equal? pos food)) #\●]
                                  [(and (pair? body) (equal? pos (car body))) #\@]
                                  [(member pos body) #\█]
                                  [else #\space]))))
              (text row-str)))

          ;; Border characters
          (define top-border (text (string-append "╭" (make-string w #\─) "╮")))
          (define bottom-border (text (string-append "╰" (make-string w #\─) "╯")))
          (define bordered-rows
            (for/list ([row (in-list rows)])
              (hcat 'top (text "│") row (text "│"))))

          ;; Header and footer
          (define header (bold (format "Snake  Score: ~a" sc)))
          (define footer
            (if (eq? (snake-game-state self) 'game-over)
                (styled (make-style #:bold #t #:foreground fg-red) "GAME OVER  (r)estart  (q)uit")
                "  arrows/hjkl: move  r: restart  q: quit"))

          (apply vcat 'left header top-border (append bordered-rows (list bottom-border footer)))))

;;; Game logic

(define (advance self)
  (define body (snake-game-body self))
  (define head (car body))
  (define dir (snake-game-dir self))
  (define w (snake-game-width self))
  (define h (snake-game-height self))

  ;; Compute new head position
  (define new-head
    (match dir
      ['up (cons (car head) (sub1 (cdr head)))]
      ['down (cons (car head) (add1 (cdr head)))]
      ['left (cons (sub1 (car head)) (cdr head))]
      ['right (cons (add1 (car head)) (cdr head))]))

  (define nc (car new-head))
  (define nr (cdr new-head))

  ;; Wall collision
  (cond
    [(or (< nc 0) (>= nc w) (< nr 0) (>= nr h)) (struct-copy snake-game self [state 'game-over])]

    ;; Self collision (check against body without last segment,
    ;; since the tail will move away)
    [(member new-head (drop-right body 1)) (struct-copy snake-game self [state 'game-over])]

    ;; Eating food
    [(and (snake-game-food self) (equal? new-head (snake-game-food self)))
     (define new-body (cons new-head body)) ;; grow: don't drop tail
     (define new-self
       (struct-copy snake-game self [body new-body] [score (add1 (snake-game-score self))] [food #f]))
     (struct-copy snake-game new-self [food (random-food new-self)])]

    ;; Normal move
    [else
     (define new-body (cons new-head (drop-right body 1)))
     (struct-copy snake-game self [body new-body])]))

(define (random-food self)
  (define w (snake-game-width self))
  (define h (snake-game-height self))
  (define body (snake-game-body self))
  (define max-attempts (* w h))
  (let loop ([attempts 0])
    (if (>= attempts max-attempts)
        #f ;; board is full
        (let ([pos (cons (random w) (random h))])
          (if (member pos body)
              (loop (add1 attempts))
              pos)))))

(module+ main
  (define p (make-program (make-snake-game) #:alt-screen #t))
  (program-run p))
