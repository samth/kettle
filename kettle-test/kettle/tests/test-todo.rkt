#lang racket/base

(require rackunit
         kettle/examples/todo
         kettle/program
         kettle/image
         (only-in kettle/test strip-ansi)
         (prefix-in ti: kettle/components/textinput)
         (prefix-in lv: kettle/components/list-view))

(define (char-key ch #:ctrl [ctrl #f])
  (key-msg ch #f ctrl))

(define (sym-key s)
  (key-msg s #f #f))

;; --- Construction ---

(test-case "initial todo has no items and is in insert mode"
  (define t (make-todo))
  (check-equal? (todo-items t) '())
  (check-equal? (todo-mode t) 'insert))

;; --- Adding items ---

(test-case "typing into textinput and pressing enter adds item"
  (define t (make-todo))
  ;; Type "buy milk" character by character
  (define (type-char t ch)
    (define-values (t2 _) (extract-update-result (update t (char-key ch))))
    t2)
  (define t2 (foldl (lambda (ch acc) (type-char acc ch)) t (string->list "buy milk")))
  ;; Verify the textinput has the value
  (check-equal? (ti:textinput-value (todo-input t2)) "buy milk")
  ;; Press enter to add
  (define-values (t3 cmd) (extract-update-result (update t2 (sym-key 'enter))))
  (check-equal? (todo-items t3) '("buy milk"))
  (check-false cmd)
  ;; Textinput should be reset
  (check-equal? (ti:textinput-value (todo-input t3)) ""))

(test-case "pressing enter with empty input does nothing"
  (define t (make-todo))
  (define-values (t2 cmd) (extract-update-result (update t (sym-key 'enter))))
  (check-equal? (todo-items t2) '())
  (check-false cmd))

;; --- Mode switching ---

(test-case "tab switches from insert to navigate"
  (define t (make-todo))
  (check-equal? (todo-mode t) 'insert)
  (define-values (t2 cmd) (extract-update-result (update t (sym-key 'tab))))
  (check-equal? (todo-mode t2) 'navigate)
  (check-false cmd))

(test-case "tab switches from navigate back to insert"
  (define t (make-todo))
  (define-values (t2 _) (extract-update-result (update t (sym-key 'tab))))
  (define-values (t3 cmd) (extract-update-result (update t2 (sym-key 'tab))))
  (check-equal? (todo-mode t3) 'insert)
  (check-false cmd))

;; --- Deleting items ---

(test-case "d in navigate mode deletes selected item"
  (define t (make-todo))
  ;; Add two items by building state directly
  (define (type-and-enter t str)
    (define t2
      (for/fold ([acc t]) ([ch (in-string str)])
        (define-values (next _) (extract-update-result (update acc (char-key ch))))
        next))
    (define-values (t3 _) (extract-update-result (update t2 (sym-key 'enter))))
    t3)
  (define t2 (type-and-enter t "first"))
  (define t3 (type-and-enter t2 "second"))
  (check-equal? (todo-items t3) '("first" "second"))
  ;; Switch to navigate mode
  (define-values (t4 _) (extract-update-result (update t3 (sym-key 'tab))))
  ;; Delete the selected item (index 0)
  (define-values (t5 cmd) (extract-update-result (update t4 (char-key #\d))))
  (check-equal? (todo-items t5) '("second"))
  (check-false cmd))

;; --- Quit ---

(test-case "C-q quits from any mode"
  (define t (make-todo))
  (define-values (t2 cmd) (extract-update-result (update t (char-key #\q #:ctrl #t))))
  (check-pred procedure? cmd))

;; --- View ---

(test-case "view produces an image"
  (define t (make-todo))
  (check-pred image? (view t)))

(test-case "view shows item count"
  (define t (make-todo))
  (define s (strip-ansi (image->string (view t))))
  (check-regexp-match #rx"0 items" s))

;; --- Purity ---

(test-case "update does not mutate the original model"
  (define t (make-todo))
  (define-values (t2 _) (extract-update-result (update t (sym-key 'tab))))
  (check-equal? (todo-mode t) 'insert)
  (check-equal? (todo-mode t2) 'navigate))
