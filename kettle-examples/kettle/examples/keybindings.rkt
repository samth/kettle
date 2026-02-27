#lang racket/base

;; keybindings.rkt -- Key binding demo with help display.
;; Adapted from cl-tuition examples/keybindings.lisp
;; https://github.com/atgreen/cl-tuition/blob/master/examples/keybindings.lisp
;;
;; Key differences from cl-tuition:
;; - cl-tuition has a first-class keybinding system (make-keybinding,
;;   make-navigation-keybindings, keybinding-matches, keybindings-help) that
;;   auto-generates help text. Kettle does not currently have this abstraction.
;; - Instead, this example demonstrates the same pattern implemented manually
;;   with a keybinding association list, showing that the TEA pattern makes it
;;   straightforward to build such abstractions in user code.
;; - Pattern matching on key-msg replaces CLOS keybinding-matches dispatch.
;;
;; Navigate with arrows/j/k, toggle mode with enter, ? for help, q to quit.

(require racket/match
         racket/format
         racket/string
         kettle/program
         kettle/image
         kettle/style)

;; Keybinding spec: (list key-desc help-text)
(define keybindings
  '(("q/ctrl-c" "quit")
    ("↑/k"      "move up")
    ("↓/j"      "move down")
    ("enter"    "select item")
    ("?/h"      "show help")))

(define (format-keybindings-inline)
  (string-join (map (lambda (kb) (format "~a: ~a" (car kb) (cadr kb)))
                    keybindings)
               " • "))

(define-kettle-program keybindings-demo
  #:fields ([selected 0]
            [items '("Apple" "Banana" "Cherry" "Date" "Elderberry")]
            [mode 'normal])  ;; 'normal, 'help
  #:update (lambda (self msg)
    (match msg
      [(key-msg #\q _ _)           (cmd self (quit-cmd))]
      [(key-msg _ _ #t)            (cmd self (quit-cmd))]
      [(key-msg (or 'up #\k) _ _)
       (define sel (keybindings-demo-selected self))
       (define len (length (keybindings-demo-items self)))
       (struct-copy keybindings-demo self
                    [selected (modulo (sub1 (+ sel len)) len)])]
      [(key-msg (or 'down #\j) _ _)
       (define sel (keybindings-demo-selected self))
       (define len (length (keybindings-demo-items self)))
       (struct-copy keybindings-demo self
                    [selected (modulo (add1 sel) len)])]
      [(key-msg (or #\? #\h) _ _)
       (struct-copy keybindings-demo self
                    [mode (if (eq? (keybindings-demo-mode self) 'help) 'normal 'help)])]
      [(key-msg 'enter _ _)
       (struct-copy keybindings-demo self
                    [mode (if (eq? (keybindings-demo-mode self) 'normal) 'help 'normal)])]
      [_ self]))
  #:view (lambda (self)
    (define items (keybindings-demo-items self))
    (define sel (keybindings-demo-selected self))
    (define mode (keybindings-demo-mode self))

    (apply vcat 'left
           (styled (make-style #:foreground fg-cyan #:bold #t) "Key Binding System Demo")
           ""
           (hcat 'top
                 (text "Mode: ")
                 (styled (make-style #:foreground
                                     (if (eq? mode 'help) fg-green fg-yellow))
                         (format "~a" mode)))
           ""
           "Select an item:"
           (append
            (for/list ([item (in-list items)]
                       [i (in-naturals)])
              (define sel? (= i sel))
              (hcat 'top
                    (text (if sel? "  ► " "    "))
                    (if sel? (bold item) (text item))))
            (list ""
                  (if (eq? mode 'help)
                      ;; Full help
                      (apply vcat 'left
                             (styled (make-style #:foreground fg-bright-blue #:bold #t)
                                     "Available Keys:")
                             ""
                             (for/list ([kb (in-list keybindings)])
                               (hcat 'top
                                     (styled (make-style #:foreground fg-bright-cyan)
                                             (format "  ~a" (~a (car kb) #:width 12)))
                                     (text (cadr kb)))))
                      ;; Inline help
                      (styled (make-style #:foreground fg-bright-black)
                              (format "Keys: ~a" (format-keybindings-inline)))))))))

(module+ main
  (program-run (make-program (make-keybindings-demo) #:alt-screen #t)))
