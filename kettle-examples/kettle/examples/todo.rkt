#lang racket/base

;; todo.rkt -- Todo list with component composition.
;; Demonstrates gen:tea-model, delegate, textinput + list-view, struct-copy.

(require racket/list
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
  #:methods gen:tea-model
  [(define (init t)
     (values t #f))
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
  (cond
    ;; C-q quits from any mode
    [(and (key-msg? msg) (char? (key-msg-key msg)) (char=? (key-msg-key msg) #\q) (key-msg-ctrl msg))
     (values t (quit-cmd))]

    ;; Tab switches mode
    [(and (key-msg? msg) (eq? (key-msg-key msg) 'tab))
     (define new-mode (if (eq? (todo-mode t) 'insert) 'navigate 'insert))
     (define new-input
       (if (eq? new-mode 'insert)
           (ti:textinput-focus (todo-input t))
           (ti:textinput-blur (todo-input t))))
     (values (struct-copy todo t [mode new-mode] [input new-input]) #f)]

    ;; In insert mode: Enter adds the item
    [(and (eq? (todo-mode t) 'insert) (key-msg? msg) (eq? (key-msg-key msg) 'enter))
     (define val (ti:textinput-value (todo-input t)))
     (if (string=? val "")
         (values t #f)
         (let* ([new-items (append (todo-items t) (list val))]
                [new-input (ti:textinput-reset (todo-input t))]
                [new-lv (lv:list-view-set-items (todo-list-view t) new-items)])
           (values (struct-copy todo t [input new-input] [list-view new-lv] [items new-items]) #f)))]

    ;; In insert mode: delegate to textinput
    [(eq? (todo-mode t) 'insert)
     (define-values (new-input cmd) (update (todo-input t) msg))
     (values (struct-copy todo t [input new-input]) cmd)]

    ;; In navigate mode: d deletes selected item
    [(and (eq? (todo-mode t) 'navigate)
          (key-msg? msg)
          (char? (key-msg-key msg))
          (char=? (key-msg-key msg) #\d))
     (define sel (lv:list-view-selected (todo-list-view t)))
     (define items (todo-items t))
     (if (and (>= sel 0) (< sel (length items)))
         (let* ([new-items (append (take items sel) (drop items (add1 sel)))]
                [new-lv (lv:list-view-set-items (todo-list-view t) new-items)])
           (values (struct-copy todo t [list-view new-lv] [items new-items]) #f))
         (values t #f))]

    ;; In navigate mode: delegate to list-view (up/down)
    [(eq? (todo-mode t) 'navigate)
     (define-values (new-lv cmd) (update (todo-list-view t) msg))
     (values (struct-copy todo t [list-view new-lv]) cmd)]

    [else (values t #f)]))

(define (todo-view t)
  (define mode-label (if (eq? (todo-mode t) 'insert) "[INSERT]" "[NAVIGATE]"))
  (define header-style (make-style #:bold #t))

  (vcat 'left
        (styled header-style (text "Todo List"))
        (text "")
        (view (todo-input t))
        (text "")
        (text (format "~a items  ~a" (length (todo-items t)) mode-label))
        (text (make-string 40 #\-))
        (view (todo-list-view t))
        (text "")
        (text "  tab    switch mode")
        (text "  enter  add item (insert mode)")
        (text "  d      delete item (navigate mode)")
        (text "  C-q    quit")))

(module+ main
  (define p (make-program (make-todo) #:alt-screen #t))
  (program-run p))
