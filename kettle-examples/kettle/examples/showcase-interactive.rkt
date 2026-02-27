#lang racket/base

;; showcase-interactive.rkt -- Interactive tabbed showcase with lists and dialog.
;; Adapted from cl-tuition examples/showcase-interactive.lisp
;; https://github.com/atgreen/cl-tuition/blob/master/examples/showcase-interactive.lisp
;;
;; Key differences from cl-tuition:
;; - cl-tuition's version uses mouse zones (zone-mark / zone-scan) for clickable
;;   tabs, list items, and dialog buttons. Kettle's image-tree renderer doesn't
;;   support invisible zone markers, so this version uses keyboard navigation
;;   (tab switching with [/], list toggling with enter, dialog with y/n).
;; - Immutable struct-copy replaces CLOS setf for all state changes.
;; - Layout uses Kettle's algebraic image tree (vcat, hcat, styled, pad) instead
;;   of cl-tuition's string-level join-horizontal / join-vertical.
;; - Sub-models (tabs, lists, dialog) are plain structs composed in a single
;;   define-kettle-program rather than separate CLOS classes with method dispatch.
;;
;; Press [/] to switch tabs, ↑/↓ to navigate lists, Enter to toggle,
;; y/n in dialog, q to quit.

(require racket/match
         racket/format
         racket/string
         racket/list
         racket/math
         kettle/program
         kettle/image
         kettle/style
         kettle/border)

;; Color theme
(define subtle (parse-hex-color "#383838"))
(define highlight (parse-hex-color "#7D56F4"))
(define special (parse-hex-color "#73F59F"))

(define tab-names '("Kettle" "Styles" "Layouts" "Colors" "Borders" "History"))

;; Checklist item
(struct check-item (name done?) #:transparent)

;; History items
(define history-items
  (list
   (string-append
    "The Romans learned from the Greeks that quinces slowly cooked with honey "
    "would \"set\" when cool. The Apicius gives a recipe for preserving whole "
    "quinces, stems and leaves attached, in a bath of honey diluted with "
    "defrutum: Roman marmalade.")
   (string-append
    "Medieval quince preserves, which went by the French name cotignac, "
    "produced in a clear version and a fruit pulp version, began to lose "
    "their medieval seasoning of spices in the 16th century.")
   (string-append
    "In 1524, Henry VIII received a \"box of marmalade\" from Mr. Hull of "
    "Exeter. This was probably marmelada, a solid quince paste from Portugal, "
    "still made and sold in southern Europe today.")))

(define-kettle-program showcase-model
  #:fields ([active-tab 0]
            [width 80]
            [height 24]
            ;; Sub-model state
            [focus 'list1]      ; 'list1, 'list2, 'dialog
            [list1-cursor 0]
            [list1-items (list (check-item "Grapefruit" #t)
                               (check-item "Yuzu" #f)
                               (check-item "Citron" #f)
                               (check-item "Kumquat" #t)
                               (check-item "Pomelo" #f))]
            [list2-cursor 0]
            [list2-items (list (check-item "Glossier" #t)
                               (check-item "Claire's Boutique" #t)
                               (check-item "Nyx" #f)
                               (check-item "Mac" #f)
                               (check-item "Milk" #f))]
            [dialog-active "confirm"])
  #:update (lambda (self msg)
    (match msg
      ;; Quit
      [(key-msg _ _ #t) (cmd self (quit-cmd))]
      ;; Window resize
      [(window-size-msg w h)
       (struct-copy showcase-model self [width w] [height h])]
      ;; Tab switching with [ and ]
      [(key-msg #\[ _ _)
       (define new-tab (modulo (sub1 (+ (showcase-model-active-tab self) (length tab-names)))
                               (length tab-names)))
       (struct-copy showcase-model self [active-tab new-tab])]
      [(key-msg #\] _ _)
       (define new-tab (modulo (add1 (showcase-model-active-tab self)) (length tab-names)))
       (struct-copy showcase-model self [active-tab new-tab])]
      ;; Tab 0 (Kettle) specific keys
      [_ #:when (= (showcase-model-active-tab self) 0)
       (define foc (showcase-model-focus self))
       (match msg
         ;; Tab key cycles focus
         [(key-msg 'tab _ _)
          (define next (cond [(eq? foc 'list1) 'list2]
                             [(eq? foc 'list2) 'dialog]
                             [else 'list1]))
          (struct-copy showcase-model self [focus next])]
         ;; Up/down navigation
         [(key-msg (or 'up #\k) _ _)
          (cond
            [(eq? foc 'list1)
             (define len (length (showcase-model-list1-items self)))
             (struct-copy showcase-model self
                          [list1-cursor (modulo (sub1 (+ (showcase-model-list1-cursor self) len)) len)])]
            [(eq? foc 'list2)
             (define len (length (showcase-model-list2-items self)))
             (struct-copy showcase-model self
                          [list2-cursor (modulo (sub1 (+ (showcase-model-list2-cursor self) len)) len)])]
            [else self])]
         [(key-msg (or 'down #\j) _ _)
          (cond
            [(eq? foc 'list1)
             (define len (length (showcase-model-list1-items self)))
             (struct-copy showcase-model self
                          [list1-cursor (modulo (add1 (showcase-model-list1-cursor self)) len)])]
            [(eq? foc 'list2)
             (define len (length (showcase-model-list2-items self)))
             (struct-copy showcase-model self
                          [list2-cursor (modulo (add1 (showcase-model-list2-cursor self)) len)])]
            [else self])]
         ;; Enter toggles list item or activates dialog button
         [(key-msg 'enter _ _)
          (cond
            [(eq? foc 'list1)
             (define idx (showcase-model-list1-cursor self))
             (define items (showcase-model-list1-items self))
             (define item (list-ref items idx))
             (define new-item (struct-copy check-item item [done? (not (check-item-done? item))]))
             (define new-items (append (take items idx) (list new-item) (drop items (add1 idx))))
             (struct-copy showcase-model self [list1-items new-items])]
            [(eq? foc 'list2)
             (define idx (showcase-model-list2-cursor self))
             (define items (showcase-model-list2-items self))
             (define item (list-ref items idx))
             (define new-item (struct-copy check-item item [done? (not (check-item-done? item))]))
             (define new-items (append (take items idx) (list new-item) (drop items (add1 idx))))
             (struct-copy showcase-model self [list2-items new-items])]
            [else self])]
         ;; Dialog buttons
         [(key-msg #\y _ _)
          #:when (eq? foc 'dialog)
          (struct-copy showcase-model self [dialog-active "confirm"])]
         [(key-msg #\n _ _)
          #:when (eq? foc 'dialog)
          (struct-copy showcase-model self [dialog-active "cancel"])]
         [_ self])]
      ;; All other tabs: just pass through
      [_ self]))
  #:view (lambda (self)
    (define tab-idx (showcase-model-active-tab self))
    (define tab-name (list-ref tab-names tab-idx))

    ;; Render tabs
    (define tabs-view
      (apply hcat 'bottom
        (for/list ([name (in-list tab-names)]
                   [i (in-naturals)])
          (define active? (= i tab-idx))
          (define tab-style
            (if active?
                (make-style #:foreground highlight #:bold #t)
                (make-style #:foreground subtle)))
          (hcat 'top
                (styled tab-style (format " ~a " name))
                (if (< i (sub1 (length tab-names))) (text " ") empty-image)))))

    ;; Tab content
    (define content
      (cond
        ;; Kettle tab: lists + dialog
        [(= tab-idx 0) (render-kettle-tab self)]
        ;; Styles tab
        [(= tab-idx 1)
         (vcat 'left
               (bold "This is bold text")
               (italic "This is italic text")
               (underline "This is underlined text")
               (styled (make-style #:foreground highlight) "This is colored text")
               (styled (make-style #:foreground special #:background subtle)
                       "  Styled with padding  "))]
        ;; Layouts tab
        [(= tab-idx 2)
         (define (box-item label color)
           (styled (make-style #:foreground fg-white #:background color)
                   (format "  ~a  " label)))
         (vcat 'left
               "Horizontal layout:"
               (hcat 'top
                     (box-item "Box 1" highlight)
                     (text "  ")
                     (box-item "Box 2" special)
                     (text "  ")
                     (box-item "Box 3" subtle))
               ""
               "Vertical layout:"
               (box-item "Box 1" highlight)
               (box-item "Box 2" special)
               (box-item "Box 3" subtle))]
        ;; Colors tab
        [(= tab-idx 3)
         (define colors
           (list (cons "Subtle" subtle)
                 (cons "Highlight" highlight)
                 (cons "Special" special)
                 (cons "Red" fg-red)
                 (cons "Green" fg-green)
                 (cons "Blue" fg-blue)
                 (cons "Yellow" fg-yellow)
                 (cons "Magenta" fg-magenta)
                 (cons "Cyan" fg-cyan)))
         (apply vcat 'left
                (styled (make-style #:foreground fg-bright-white #:bold #t) "Color Palette:")
                ""
                (for/list ([c (in-list colors)])
                  (styled (make-style #:foreground (cdr c))
                          (format "  ~a" (car c)))))]
        ;; Borders tab
        [(= tab-idx 4)
         (vcat 'left
               (string->image (render-border "Normal" border-normal))
               ""
               (string->image (render-border "Rounded" border-rounded))
               ""
               (string->image (render-border "Thick" border-thick))
               ""
               (string->image (render-border "Double" border-double))
               ""
               (string->image (render-border "ASCII" border-ascii)))]
        ;; History tab
        [(= tab-idx 5)
         (apply vcat 'left
                (for/list ([item (in-list history-items)]
                           [i (in-naturals)])
                  (define wrapped (wrap-text item 60))
                  (vcat 'left
                        (styled (make-style #:foreground fg-bright-white
                                            #:background highlight)
                                (format " ~a " wrapped))
                        "")))]))

    (vcat 'left
          tabs-view
          (styled (make-style #:foreground highlight)
                  (make-string (min 80 (showcase-model-width self)) #\─))
          ""
          content
          ""
          (styled (make-style #:foreground fg-bright-black)
                  "[/]: tabs • ↑↓/jk: move • enter: toggle • tab: focus • ctrl-c: quit"))))

;; Render the main "Kettle" tab with two lists and a dialog
(define (render-kettle-tab self)
  (define foc (showcase-model-focus self))

  ;; List renderer
  (define (render-list title items cursor focused?)
    (define header
      (styled (make-style #:foreground subtle)
              (format "─ ~a ─" title)))
    (define item-views
      (for/list ([item (in-list items)]
                 [i (in-naturals)])
        (define sel? (and focused? (= i cursor)))
        (define done? (check-item-done? item))
        (define name (check-item-name item))
        (define mark (if done?
                        (styled (make-style #:foreground special) "✓ ")
                        (text "  ")))
        (define name-view
          (if done?
              (styled (make-style #:foreground fg-bright-black #:strikethrough #t) name)
              (text name)))
        (if sel?
            (styled (make-style #:reverse #t)
                    (hcat 'top mark name-view))
            (hcat 'top mark name-view))))
    (apply vcat 'left header item-views))

  ;; Dialog renderer
  (define (render-dialog active focused?)
    (define question "Are you sure you want\nto eat marmalade?")
    (define confirm-style
      (if (string=? active "confirm")
          (make-style #:foreground (parse-hex-color "#FFF7DB")
                      #:background (parse-hex-color "#F25D94")
                      #:underline #t)
          (make-style #:foreground (parse-hex-color "#FFF7DB")
                      #:background (parse-hex-color "#888B7E"))))
    (define cancel-style
      (if (string=? active "cancel")
          (make-style #:foreground (parse-hex-color "#FFF7DB")
                      #:background (parse-hex-color "#F25D94")
                      #:underline #t)
          (make-style #:foreground (parse-hex-color "#FFF7DB")
                      #:background (parse-hex-color "#888B7E"))))
    (define q-view (text question))
    (define btns (hcat 'top
                       (styled confirm-style "  Yes  ")
                       (text "  ")
                       (styled cancel-style " Maybe ")))
    (define ui (vcat 'left q-view "" btns))
    (define bordered
      (string->image (render-border (format "~a\n\n~a  ~a"
                                            question
                                            (render-styled confirm-style "  Yes  ")
                                            (render-styled cancel-style " Maybe "))
                                    border-rounded
                                    #:fg-color (parse-hex-color "#874BFD"))))
    (if focused?
        (vcat 'left
              (styled (make-style #:foreground highlight #:bold #t) "Dialog (y/n):")
              bordered)
        (vcat 'left
              "Dialog:"
              bordered)))

  (hcat 'top
        (pad (render-list "Citrus Fruits to Try"
                          (showcase-model-list1-items self)
                          (showcase-model-list1-cursor self)
                          (eq? foc 'list1))
             #:right 2)
        (pad (render-list "Lip Gloss Vendors"
                          (showcase-model-list2-items self)
                          (showcase-model-list2-cursor self)
                          (eq? foc 'list2))
             #:right 2)
        (render-dialog (showcase-model-dialog-active self)
                       (eq? foc 'dialog))))

(module+ main
  (program-run (make-program (make-showcase-model) #:alt-screen #t)))
