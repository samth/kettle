#lang racket/base

;; table.rkt -- Table rendering demonstration.
;; Adapted from cl-tuition examples/table.lisp
;; https://github.com/atgreen/cl-tuition/blob/master/examples/table.lisp
;;
;; Key differences from cl-tuition:
;; - Uses Kettle's table component (kettle/components/table) which provides
;;   make-table and table-render, similar to cl-tuition's tui.render.table.
;; - Border styles are Kettle constants (border-normal, border-ascii) instead of
;;   cl-tuition globals (*border-normal*, *border-ascii*).
;; - The view returns an image tree; table-render returns a string which is
;;   converted via string->image.
;;
;; Press q to quit.

(require racket/match
         kettle/run
         kettle/image
         kettle/style
         kettle/border
         kettle/program
         (prefix-in tbl: kettle/components/table))

(define (table-on-key _ km)
  (match km
    [(key-msg #\q _ _) (cmd #f (quit-cmd))]
    [(key-msg _ _ #t)  (cmd #f (quit-cmd))]
    [_ #f]))

(define (table-view _)
  (define languages-table
    (tbl:make-table #:headers '("Language" "Formal" "Informal")
                    #:rows '(("Chinese" "您好" "你好")
                             ("Japanese" "こんにちは" "やあ")
                             ("Russian" "Здравствуйте" "Привет")
                             ("Spanish" "Hola" "¿Qué tal?"))
                    #:border border-normal))

  (define ascii-table
    (tbl:make-table #:headers '("ID" "Name" "Status")
                    #:rows '(("1" "Alice" "Active")
                             ("2" "Bob" "Inactive")
                             ("3" "Charlie" "Active"))
                    #:border border-ascii))

  (vcat 'left
        (bold "Table Examples")
        ""
        (tbl:table-render languages-table)
        ""
        (tbl:table-render ascii-table)
        ""
        "Press q to quit"))

(module+ main
  (run #f #:on-key table-on-key #:to-view table-view #:alt-screen #t))
