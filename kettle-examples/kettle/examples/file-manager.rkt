#lang racket/base

;; file-manager.rkt -- Text-based file manager.
;; Adapted from cl-tuition examples/file-manager.lisp
;; https://github.com/atgreen/cl-tuition/blob/master/examples/file-manager.lisp
;;
;; Key differences from cl-tuition:
;; - Uses define-kettle-program with immutable struct-copy updates instead of
;;   CLOS defclass with setf mutation.
;; - Modal state machine (:browse, :delete-confirm, :mkdir, :rename) is
;;   pattern-matched in a single #:update function vs. four separate handler
;;   functions dispatched by mode.
;; - Window-size-msg is handled via struct pattern matching; terminal dimensions
;;   are queried in #:init via get-terminal-size.
;; - File operations use Racket's directory-list, file-size, delete-file/directory
;;   APIs instead of CL's directory, truename, pathname functions.
;;
;; Navigate with j/k/arrows, enter to open, h for parent, q to quit.

(require racket/match
         racket/list
         racket/string
         racket/format
         racket/file
         racket/path
         kettle)

(struct file-item (name path dir? size) #:transparent)

(define (format-size bytes)
  (cond
    [(not bytes) "???"]
    [(< bytes 1024) (format "~aB" bytes)]
    [(< bytes (* 1024 1024)) (~r (/ bytes 1024.0) #:precision '(= 1))]
    [(< bytes (* 1024 1024 1024))
     (string-append (~r (/ bytes (* 1024.0 1024)) #:precision '(= 1)) "MB")]
    [else (string-append (~r (/ bytes (* 1024.0 1024 1024)) #:precision '(= 1)) "GB")]))

(define (load-directory path show-hidden?)
  (with-handlers ([exn:fail? (lambda (e)
                     (list (file-item (format "Error: ~a" (exn-message e)) "" #f #f)))])
    (define entries (directory-list path #:build? #t))
    (define items
      (filter-map
       (lambda (entry)
         (define name (path->string (file-name-from-path entry)))
         (define hidden? (and (> (string-length name) 0)
                              (char=? (string-ref name 0) #\.)))
         (cond
           [(and hidden? (not show-hidden?)) #f]
           [(directory-exists? entry)
            (file-item name (path->string entry) #t #f)]
           [else
            (file-item name (path->string entry) #f
                       (with-handlers ([exn:fail? (lambda (_) #f)])
                         (file-size entry)))]))
       entries))
    (sort items
          (lambda (a b)
            (string<? (string-append (if (file-item-dir? a) "0" "1") (file-item-name a))
                      (string-append (if (file-item-dir? b) "0" "1") (file-item-name b)))))))

(define (parent-directory path)
  (define p (simplify-path (build-path path 'up)))
  (path->string p))

(define-kettle-program file-mgr
  #:fields ([current-dir "."]
            [items '()]
            [selected 0]
            [scroll-offset 0]
            [width 80] [height 24]
            [status ""]
            [error-msg #f]
            [mode 'browse]
            [input-buf ""]
            [show-hidden? #f])
  #:init (lambda (self)
    (define size (get-terminal-size))
    (define w (if size (car size) 80))
    (define h (if size (cdr size) 24))
    (define dir (path->string (current-directory)))
    (define itms (load-directory dir (file-mgr-show-hidden? self)))
    (struct-copy file-mgr self
                 [current-dir dir]
                 [items itms]
                 [width w] [height h]
                 [status (format "Loaded ~a items" (length itms))]))
  #:update (lambda (self msg)
    (match msg
      [(window-size-msg w h)
       (struct-copy file-mgr self [width w] [height h])]
      ;; Browse mode
      [(key-msg #\q _ _) #:when (eq? (file-mgr-mode self) 'browse)
       (cmd self (quit-cmd))]
      [(key-msg _ _ #t) #:when (eq? (file-mgr-mode self) 'browse)
       (cmd self (quit-cmd))]
      [(key-msg (or 'up #\k) _ _) #:when (eq? (file-mgr-mode self) 'browse)
       (struct-copy file-mgr self
                    [selected (max 0 (sub1 (file-mgr-selected self)))]
                    [error-msg #f])]
      [(key-msg (or 'down #\j) _ _) #:when (eq? (file-mgr-mode self) 'browse)
       (struct-copy file-mgr self
                    [selected (min (sub1 (length (file-mgr-items self)))
                                   (add1 (file-mgr-selected self)))]
                    [error-msg #f])]
      [(key-msg 'enter _ _) #:when (eq? (file-mgr-mode self) 'browse)
       (define items (file-mgr-items self))
       (cond
         [(null? items) self]
         [else
          (define item (list-ref items (file-mgr-selected self)))
          (cond
            [(file-item-dir? item)
             (define new-items (load-directory (file-item-path item)
                                              (file-mgr-show-hidden? self)))
             (struct-copy file-mgr self
                          [current-dir (file-item-path item)]
                          [items new-items]
                          [selected 0]
                          [scroll-offset 0]
                          [status (format "~a (~a items)" (file-item-path item)
                                         (length new-items))]
                          [error-msg #f])]
            [else self])])]
      [(key-msg #\h _ _) #:when (eq? (file-mgr-mode self) 'browse)
       (define par (parent-directory (file-mgr-current-dir self)))
       (define new-items (load-directory par (file-mgr-show-hidden? self)))
       (struct-copy file-mgr self
                    [current-dir par]
                    [items new-items]
                    [selected 0]
                    [scroll-offset 0]
                    [status (format "~a (~a items)" par (length new-items))]
                    [error-msg #f])]
      [(key-msg #\r _ _) #:when (eq? (file-mgr-mode self) 'browse)
       (define new-items (load-directory (file-mgr-current-dir self)
                                        (file-mgr-show-hidden? self)))
       (struct-copy file-mgr self
                    [items new-items]
                    [selected (min (file-mgr-selected self)
                                   (max 0 (sub1 (length new-items))))]
                    [status "Refreshed"]
                    [error-msg #f])]
      [(key-msg #\. _ _) #:when (eq? (file-mgr-mode self) 'browse)
       (define new-show (not (file-mgr-show-hidden? self)))
       (define new-items (load-directory (file-mgr-current-dir self) new-show))
       (struct-copy file-mgr self
                    [show-hidden? new-show]
                    [items new-items]
                    [status (if new-show "Showing hidden files" "Hiding hidden files")]
                    [error-msg #f])]
      [(key-msg #\d _ _) #:when (eq? (file-mgr-mode self) 'browse)
       (if (null? (file-mgr-items self)) self
           (struct-copy file-mgr self [mode 'delete-confirm] [error-msg #f]))]
      [(key-msg #\n _ _) #:when (eq? (file-mgr-mode self) 'browse)
       (struct-copy file-mgr self [mode 'mkdir] [input-buf ""] [error-msg #f])]
      ;; Delete confirm mode
      [(key-msg #\y _ _) #:when (eq? (file-mgr-mode self) 'delete-confirm)
       (define item (list-ref (file-mgr-items self) (file-mgr-selected self)))
       (with-handlers ([exn:fail?
                        (lambda (e)
                          (struct-copy file-mgr self
                                       [mode 'browse]
                                       [error-msg (format "Error: ~a" (exn-message e))]))])
         (if (file-item-dir? item)
             (delete-directory/files (file-item-path item))
             (delete-file (file-item-path item)))
         (define new-items (load-directory (file-mgr-current-dir self)
                                          (file-mgr-show-hidden? self)))
         (struct-copy file-mgr self
                      [mode 'browse]
                      [items new-items]
                      [selected (min (file-mgr-selected self)
                                     (max 0 (sub1 (length new-items))))]
                      [status (format "Deleted: ~a" (file-item-name item))]
                      [error-msg #f]))]
      [(key-msg _ _ _) #:when (eq? (file-mgr-mode self) 'delete-confirm)
       (struct-copy file-mgr self [mode 'browse])]
      ;; Mkdir mode
      [(key-msg 'enter _ _) #:when (eq? (file-mgr-mode self) 'mkdir)
       (cond
         [(string=? (file-mgr-input-buf self) "")
          (struct-copy file-mgr self [mode 'browse] [input-buf ""])]
         [else
          (with-handlers ([exn:fail?
                           (lambda (e)
                             (struct-copy file-mgr self
                                          [mode 'browse]
                                          [error-msg (format "Error: ~a" (exn-message e))]))])
            (make-directory (build-path (file-mgr-current-dir self) (file-mgr-input-buf self)))
            (define new-items (load-directory (file-mgr-current-dir self)
                                             (file-mgr-show-hidden? self)))
            (struct-copy file-mgr self
                         [mode 'browse]
                         [items new-items]
                         [input-buf ""]
                         [status (format "Created: ~a" (file-mgr-input-buf self))]
                         [error-msg #f]))])]
      [(key-msg 'escape _ _) #:when (eq? (file-mgr-mode self) 'mkdir)
       (struct-copy file-mgr self [mode 'browse] [input-buf ""])]
      [(key-msg 'backspace _ _) #:when (eq? (file-mgr-mode self) 'mkdir)
       (define buf (file-mgr-input-buf self))
       (if (> (string-length buf) 0)
           (struct-copy file-mgr self [input-buf (substring buf 0 (sub1 (string-length buf)))])
           self)]
      [(key-msg (? char? ch) _ _) #:when (eq? (file-mgr-mode self) 'mkdir)
       (struct-copy file-mgr self [input-buf (string-append (file-mgr-input-buf self) (string ch))])]
      [_ self]))
  #:view (lambda (self)
    (case (file-mgr-mode self)
      [(browse)
       (define avail-h (max 5 (- (file-mgr-height self) 8)))
       ;; Adjust scroll
       (define sel (file-mgr-selected self))
       (define offset
         (let ([off (file-mgr-scroll-offset self)])
           (cond
             [(< sel off) sel]
             [(>= sel (+ off avail-h)) (- sel avail-h -1)]
             [else off])))
       (define items (file-mgr-items self))
       (define visible
         (if (null? items) '()
             (take (drop items (min offset (length items)))
                   (min avail-h (max 0 (- (length items) offset))))))
       (apply vcat 'left
              (bold "  FILE MANAGER")
              (styled (make-style #:foreground fg-bright-black)
                      (format "  ~a" (file-mgr-current-dir self)))
              ""
              (append
               (for/list ([item (in-list visible)]
                          [i (in-naturals offset)])
                 (define sel? (= i sel))
                 (define icon (if (file-item-dir? item) "📁" "📄"))
                 (define name-str
                   (if sel?
                       (styled (make-style #:foreground fg-bright-cyan #:bold #t)
                               (file-item-name item))
                       (text (file-item-name item))))
                 (define size-str
                   (if (file-item-dir? item) ""
                       (format " (~a)" (format-size (file-item-size item)))))
                 (hcat 'top
                       (text (if sel? " > " "   "))
                       (text (format "~a " icon))
                       name-str
                       (styled (make-style #:foreground fg-bright-black) size-str)))
               (list ""
                     (if (file-mgr-error-msg self)
                         (styled (make-style #:foreground fg-red) (format "  ~a" (file-mgr-error-msg self)))
                         (styled (make-style #:foreground fg-bright-green) (format "  ~a" (file-mgr-status self))))
                     ""
                     (styled (make-style #:foreground fg-yellow)
                             "  j/k nav • enter open • h parent • r refresh • . hidden • d delete • n mkdir • q quit"))))]
      [(delete-confirm)
       (define item (list-ref (file-mgr-items self) (file-mgr-selected self)))
       (vcat 'left
             ""
             (styled (make-style #:foreground fg-red #:bold #t) "  DELETE CONFIRMATION")
             ""
             (text (format "  Delete ~a?" (file-item-name item)))
             (text (format "  Type: ~a" (if (file-item-dir? item) "Directory" "File")))
             ""
             (styled (make-style #:foreground fg-yellow) "  y confirm • any other key cancel"))]
      [(mkdir)
       (vcat 'left
             ""
             (styled (make-style #:foreground fg-cyan #:bold #t) "  CREATE DIRECTORY")
             ""
             (hcat 'top (text "  Directory name: ") (bold (file-mgr-input-buf self)))
             ""
             (styled (make-style #:foreground fg-yellow) "  enter confirm • esc cancel"))]
      [else (text "")])))

(module+ main
  (program-run (make-program (make-file-mgr) #:alt-screen #t)))
