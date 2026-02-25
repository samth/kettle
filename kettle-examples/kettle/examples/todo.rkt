#lang racket/base

;; todo.rkt -- Todo list with component composition.
;; Demonstrates gen:kettle-model, delegate, textinput + list-view, struct-copy.

(require racket/list
         racket/match
         kettle/program
         kettle/image
         kettle/style
         (prefix-in ti: kettle/components/textinput)
         (prefix-in lv: kettle/components/list-view))

(provide (struct-out todo)
         make-todo
         todo-items)

(struct todo (input list-view items mode)
  ;; mode is 'insert (typing into textinput) or 'navigate (browsing the list)
  #:transparent
  #:methods gen:kettle-model
  [(define (init t)
     t)
   (define (update t msg)
     (todo-update t msg))
   (define (view t)
     (todo-view t))])

(define (make-todo)
  (todo (ti:make-textinput #:prompt "new: " #:width 40)
        (lv:make-list-view #:items '() #:height 15)
        '()
        'insert))

(define (todo-update t msg)
  (match msg
    ;; C-q quits from any mode
    [(key-msg #\q _ #t) (cmd t (quit-cmd))]

    ;; Tab switches mode
    [(key-msg 'tab _ _)
     (define new-mode (if (eq? (todo-mode t) 'insert) 'navigate 'insert))
     (define new-input
       (if (eq? new-mode 'insert)
           (ti:textinput-focus (todo-input t))
           (ti:textinput-blur (todo-input t))))
     (struct-copy todo t [mode new-mode] [input new-input])]

    ;; In insert mode: Enter adds the item
    [(key-msg 'enter _ _)
     #:when (eq? (todo-mode t) 'insert)
     (define val (ti:textinput-value (todo-input t)))
     (if (string=? val "")
         t
         (let* ([new-items (append (todo-items t) (list val))]
                [new-input (ti:textinput-reset (todo-input t))]
                [new-lv (lv:list-view-set-items (todo-list-view t) new-items)])
           (struct-copy todo t [input new-input] [list-view new-lv] [items new-items])))]

    ;; In insert mode: delegate to textinput
    [_
     #:when (eq? (todo-mode t) 'insert)
     (define-values (new-input sub-cmd) (extract-update-result (update (todo-input t) msg)))
     (if sub-cmd
         (cmd (struct-copy todo t [input new-input]) sub-cmd)
         (struct-copy todo t [input new-input]))]

    ;; In navigate mode: d deletes selected item
    [(key-msg #\d _ _)
     #:when (eq? (todo-mode t) 'navigate)
     (define sel (lv:list-view-selected (todo-list-view t)))
     (define items (todo-items t))
     (if (and (>= sel 0) (< sel (length items)))
         (let* ([new-items (append (take items sel) (drop items (add1 sel)))]
                [new-lv (lv:list-view-set-items (todo-list-view t) new-items)])
           (struct-copy todo t [list-view new-lv] [items new-items]))
         t)]

    ;; In navigate mode: delegate to list-view (up/down)
    [_
     #:when (eq? (todo-mode t) 'navigate)
     (define-values (new-lv sub-cmd) (extract-update-result (update (todo-list-view t) msg)))
     (if sub-cmd
         (cmd (struct-copy todo t [list-view new-lv]) sub-cmd)
         (struct-copy todo t [list-view new-lv]))]

    [_ t]))

(define (todo-view t)
  (define mode-label (if (eq? (todo-mode t) 'insert) "[INSERT]" "[NAVIGATE]"))

  (vcat 'left
        (bold "Todo List")
        ""
        (view (todo-input t))
        ""
        (format "~a items  ~a" (length (todo-items t)) mode-label)
        (text (make-string 40 #\-))
        (view (todo-list-view t))
        ""
        "  tab    switch mode"
        "  enter  add item (insert mode)"
        "  d      delete item (navigate mode)"
        "  C-q    quit"))

(module+ main
  (define p (make-program (make-todo) #:alt-screen #t))
  (program-run p))
