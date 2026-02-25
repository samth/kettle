#lang scribble/manual
@(require (for-label racket/base
                     racket/generic
                     kettle
                     kettle/run))

@title{Porting from big-bang to Kettle}

Kettle is a terminal UI library for Racket that uses The Elm Architecture
(TEA), the same pattern behind @racket[big-bang]. Where @racket[big-bang]
renders to a graphical canvas, Kettle renders to the terminal. The core
loop is the same: state goes in, events come in, new state comes out, a
view is rendered.

@section{The simplest comparison}

A @racket[big-bang] counter:

@racketblock[
(require 2htdp/universe 2htdp/image)

(big-bang 0
  [on-key (lambda (count ke)
            (cond
              [(key=? ke "up") (add1 count)]
              [(key=? ke "down") (sub1 count)]
              [else count]))]
  [to-draw (lambda (count)
             (text (format "Count: ~a" count) 24 "black"))])
]

The same program in Kettle:

@racketblock[
(require kettle/run kettle/image kettle/program)

(run 0
  #:on-key (lambda (count km)
             (define key (key-msg-key km))
             (cond
               [(and (char? key) (char=? key #\+))
                (add1 count)]
               [(and (char? key) (char=? key #\-))
                (sub1 count)]
               [(and (char? key) (char=? key #\q))
                (cmd count (quit-cmd))]
               [else count]))
  #:to-view (lambda (count)
              (text (format "Count: ~a" count)))
  #:alt-screen #t)
]

@section{Key differences at a glance}

@tabular[#:style 'boxed
         #:sep @hspace[2]
         #:column-properties '(left left left)
         (list (list @bold{big-bang} @bold{Kettle} @bold{Notes})
               (list @racket[(require 2htdp/universe)]
                     @racket[(require kettle/run)]
                     "Simple programs")
               (list @racket[(big-bang init ...)]
                     @racket[(run init ...)]
                     "Same role")
               (list @racket[[on-key handler]]
                     @racket[#:on-key handler]
                     "Keyword args, not clauses")
               (list @racket[[to-draw handler]]
                     @racket[#:to-view handler]
                     "Terminal, not canvas")
               (list @racket[[stop-when pred]]
                     @racket[#:stop-when pred]
                     "Same idea")
               (list @racket[[on-tick handler]]
                     "subscriptions"
                     "More advanced; see below")
               (list "Handler returns state"
                     @elem{state or @racket[(cmd state command)]}
                     "Bare value or cmd wrapper")
               (list "Closes window to quit"
                     @racket[(quit-cmd)]
                     "Must quit explicitly")
               (list "Key events are strings"
                     @elem{@racket[key-msg] structs}
                     "")
               (list @racketmodname[2htdp/image]
                     @racketmodname[kettle/image]
                     "Text-based, not pixel-based"))]

@section{Handlers return a model or a cmd wrapper}

In @racket[big-bang], an event handler returns the new state:

@racketblock[
(code:comment "big-bang")
[on-key (lambda (state ke)
          (add1 state))]
]

In Kettle, a handler returns either the new state directly (when no
command is needed) or wraps it with @racket[cmd] to attach a command:

@racketblock[
(code:comment "Kettle -- no command, return state directly")
#:on-key (lambda (state km)
           (add1 state))
]

The @racket[cmd] wrapper is how you tell the runtime to do things. The
most common command is @racket[(quit-cmd)], which tells the program to
exit:

@racketblock[
(cmd state (quit-cmd))
]

When no command is needed, return the new state directly.

@section{Key events are structs, not strings}

@racket[big-bang] represents keys as strings: @racket["a"], @racket["left"],
@racket["shift"]. Kettle uses @racket[key-msg] structs with three fields:

@itemlist[
  @item{@racket[key] --- a @racket[char?] for printable keys (@racket[#\a],
        @racket[#\space], @racket[#\+]), or a @racket[symbol?] for special
        keys (@racket['up], @racket['down], @racket['left], @racket['right],
        @racket['enter], @racket['tab], @racket['escape], @racket['backspace],
        @racket['home], @racket['end], @racket['f1] through @racket['f12])}
  @item{@racket[alt] --- @racket[#t] if Alt was held}
  @item{@racket[ctrl] --- @racket[#t] if Ctrl was held}
]

Translation examples:

@tabular[#:style 'boxed
         #:sep @hspace[2]
         #:column-properties '(left left)
         (list (list @bold{big-bang} @bold{Kettle})
               (list @racket[(key=? ke "a")]
                     @racket[(and (char? (key-msg-key km)) (char=? (key-msg-key km) #\a))])
               (list @racket[(key=? ke "left")]
                     @racket[(eq? (key-msg-key km) 'left)])
               (list @racket[(key=? ke " ")]
                     @racket[(and (char? (key-msg-key km)) (char=? (key-msg-key km) #\space))]))]

A common pattern is to bind the key first:

@racketblock[
(define key (key-msg-key km))
(cond
  [(eq? key 'up)    ...]
  [(eq? key 'down)  ...]
  [(and (char? key) (char=? key #\q)) ...]
  [else ...])
]

Modifier keys work differently from @racket[big-bang]. In @racket[big-bang],
@racket["shift"] is a separate event. In Kettle, Ctrl and Alt are flags
on the same event. Shift is implicit in the character (you get @racket[#\A]
or @racket[#\+] directly).

@section{Images are text, not pixels}

@racket[big-bang] uses @racketmodname[2htdp/image], where you compose
circles, rectangles, and text on a canvas. Kettle's images are terminal
text. The core building blocks:

@racketblock[
(require kettle/image)

(text "hello")              (code:comment "a single line of text")
(text "")                   (code:comment "an empty line (spacer)")

(vcat 'left                 (code:comment "stack vertically, left-aligned")
  (text "Line 1")
  (text "Line 2"))

(hcat 'top                  (code:comment "place side by side, top-aligned")
  (text "Left ")
  (text "Right"))
]

Translation from @racketmodname[2htdp/image]:

@tabular[#:style 'boxed
         #:sep @hspace[2]
         #:column-properties '(left left)
         (list (list @bold{2htdp/image} @bold{kettle/image})
               (list @racket[(text "hi" 24 "black")]
                     @racket[(text "hi")])
               (list @racket[(above img1 img2)]
                     @racket[(vcat 'left img1 img2)])
               (list @racket[(beside img1 img2)]
                     @racket[(hcat 'top img1 img2)])
               (list @racket[(overlay img1 img2)]
                     @racket[(zcat img1 img2)])
               (list @racket[(empty-scene w h)]
                     @racket[(blank w h)]))]

Alignment argument: @racket[vcat] takes a horizontal alignment
(@racket['left], @racket['center], @racket['right]). @racket[hcat] takes
a vertical alignment (@racket['top], @racket['center], @racket['bottom]).

@subsection{Styling}

There are no colors or fonts in the @racketmodname[2htdp/image] sense.
Instead, Kettle uses terminal styles:

@racketblock[
(require kettle/style)

(styled (make-style #:bold #t)
        (text "Important"))

(styled (make-style #:foreground fg-red #:italic #t)
        (text "Warning"))
]

Available attributes: @racket[#:bold], @racket[#:italic],
@racket[#:underline], @racket[#:reverse], @racket[#:faint],
@racket[#:blink], @racket[#:strikethrough]. Colors: @racket[fg-red],
@racket[bg-blue], etc., plus @racket[(color-256 n)] and
@racket[(color-rgb r g b)].

@section{No automatic quitting}

@racket[big-bang]'s window has a close button, and @racket[stop-when]
halts the program when a predicate is true. Kettle programs run in the
terminal until you explicitly quit. You almost always need a quit key:

@racketblock[
[(and (char? key) (char=? key #\q))
 (cmd state (quit-cmd))]
]

@racket[#:stop-when] also works and is checked after each update, same
as @racket[big-bang]:

@racketblock[
(run 0
  #:on-key (lambda (n km) (add1 n))
  #:stop-when (lambda (n) (> n 10))
  #:to-view (lambda (n) (text (format "~a" n))))
]

@section{on-tick becomes subscriptions}

@racket[big-bang] has a simple @racket[[on-tick handler]] or
@racket[[on-tick handler rate]] clause. Kettle's @racket[run] form
doesn't have a tick handler. For time-based programs, you need
@racket[define-kettle-program] with subscriptions, which is more powerful
but more involved.

Here's a side-by-side of a counting timer.

@racket[big-bang] version:

@racketblock[
(big-bang 0
  [on-tick add1 1]
  [to-draw (lambda (n) (text (number->string n) 24 "black"))]
  [stop-when (lambda (n) (> n 10))])
]

Kettle version:

@racketblock[
(require kettle)

(define-kettle-program
  ticker
  #:fields ([count 0])
  #:update
  (lambda (self msg)
    (cond
      [(tick-msg? msg)
       (define new-self (struct-copy ticker self
                          [count (add1 (ticker-count self))]))
       (if (> (ticker-count self) 10)
           (cmd new-self (quit-cmd))
           new-self)]
      [else self]))
  #:view
  (lambda (self)
    (text (number->string (ticker-count self)))))
]

This uses @racket[tick-msg] messages that arrive from the program
runner's subscription system. The @racket[define-kettle-program] macro
creates a struct (@racket[ticker]) with field accessors
(@racket[ticker-count]) and a constructor (@racket[make-ticker]).

@section{Scaling up: @racket[define-kettle-program]}

@racket[run] is fine for simple programs, just as @racket[big-bang] is
fine for simple programs. When your state gets complex, @racket[big-bang]
users typically define a struct. Kettle provides
@racket[define-kettle-program] which does this for you and wires up the TEA
protocol:

@racketblock[
(define-kettle-program
  my-app
  #:fields ([name "World"]
            [count 0])
  #:update
  (lambda (self msg)
    (code:comment "self is a my-app struct")
    (code:comment "(my-app-name self), (my-app-count self) are field accessors")
    (code:comment "(struct-copy my-app self [count 5]) creates an updated copy")
    self)
  #:view
  (lambda (self)
    (text (format "Hello ~a: ~a" (my-app-name self) (my-app-count self)))))
]

This generates:
@itemlist[
  @item{A struct @racket[my-app] with fields @racket[name] and @racket[count]}
  @item{A constructor @racket[(make-my-app)] with keyword arguments
        @racket[#:name], @racket[#:count]}
  @item{Accessors @racket[my-app-name], @racket[my-app-count]}
  @item{The struct implements @racket[gen:kettle-model], so you can pass it
        to @racket[make-program]}
]

To run it:

@racketblock[
(define p (make-program (make-my-app) #:alt-screen #t))
(program-run p)
]

@section{Scaling further: @racket[gen:kettle-model]}

For maximum control, implement the @racket[gen:kettle-model] generic
interface directly on a struct. This is useful when you want to compose
components:

@racketblock[
(struct my-app (child-component some-data)
  #:transparent
  #:methods gen:kettle-model
  [(define (init self) self)
   (define (update self msg) (my-update self msg))
   (define (view self) (my-view self))])
]

This is analogous to how advanced @racket[big-bang] programs define
custom structs, but here the update/view logic is attached to the struct
itself.

@section{Message types beyond keys}

@racket[big-bang] has @racket[on-key], @racket[on-mouse],
@racket[on-tick]. Kettle has a single @racket[update] function that
receives different message types:

@tabular[#:style 'boxed
         #:sep @hspace[2]
         #:column-properties '(left left left)
         (list (list @bold{Message} @bold{When it arrives} @bold{Key fields})
               (list @racket[key-msg] "Key press" @elem{@racket[key], @racket[alt], @racket[ctrl]})
               (list @racket[tick-msg] "Timer tick" @racket[time])
               (list @racket[window-size-msg] "Terminal resized" @elem{@racket[width], @racket[height]})
               (list @racket[mouse-press-event] "Mouse click" @elem{@racket[x], @racket[y], @racket[button]})
               (list @racket[mouse-release-event] "Mouse release" "same as press")
               (list @racket[mouse-move-event] "Mouse motion" @elem{@racket[x], @racket[y]})
               (list @racket[mouse-scroll-event] "Scroll wheel" @elem{@racket[x], @racket[y], @racket[direction]})
               (list @racket[focus-in-msg] "Terminal gains focus" "(none)")
               (list @racket[focus-out-msg] "Terminal loses focus" "(none)"))]

Use predicates to dispatch: @racket[(key-msg? msg)],
@racket[(tick-msg? msg)], @racket[(window-size-msg? msg)], etc.

Mouse events require enabling mouse tracking:

@racketblock[
(make-program model #:alt-screen #t #:mouse 'cell-motion)
]

@section{Testing}

A major advantage of the TEA pattern is testability---the same advantage
@racket[big-bang] has. Event handlers are pure functions. You can test
them without a terminal by constructing messages directly:

@racketblock[
(require rackunit kettle/program kettle/image)

(code:comment "Construct key messages by hand")
(define (char-key ch) (key-msg ch #f #f))

(code:comment "Test an update handler")
(define-values (new-state c) (extract-update-result (update my-model (char-key #\+))))
(check-equal? new-state expected-state)
(check-false c)

(code:comment "Test a view function")
(check-pred image? (view my-model))
]

@section{Complete example: Porting a big-bang program}

A simple editor in @racket[big-bang]:

@racketblock[
(require 2htdp/universe 2htdp/image)

(define-struct editor [text cursor])

(define (handle-key ed ke)
  (cond
    [(= (string-length ke) 1)
     (make-editor (string-append (editor-text ed) ke)
                  (add1 (editor-cursor ed)))]
    [(key=? ke "\b")
     (if (> (string-length (editor-text ed)) 0)
         (make-editor (substring (editor-text ed) 0
                       (sub1 (string-length (editor-text ed))))
                      (max 0 (sub1 (editor-cursor ed))))
         ed)]
    [else ed]))

(define (render ed)
  (text (editor-text ed) 18 "black"))

(big-bang (make-editor "" 0)
  [on-key handle-key]
  [to-draw render])
]

The same program in Kettle:

@racketblock[
(require kettle/run kettle/image kettle/program)

(struct editor [text cursor] #:transparent)

(define (handle-key ed km)
  (define key (key-msg-key km))
  (cond
    [(and (char? key) (char=? key #\q) (key-msg-ctrl km))
     (cmd ed (quit-cmd))]
    [(and (char? key) (char-graphic? key))
     (editor (string-append (editor-text ed) (string key))
             (add1 (editor-cursor ed)))]
    [(eq? key 'backspace)
     (if (> (string-length (editor-text ed)) 0)
         (editor (substring (editor-text ed) 0
                   (sub1 (string-length (editor-text ed))))
                 (max 0 (sub1 (editor-cursor ed))))
         ed)]
    [else ed]))

(define (render ed)
  (vcat 'left
    (text (editor-text ed))
    (text "")
    (text "  C-q  quit")))

(run (editor "" 0)
  #:on-key handle-key
  #:to-view render
  #:alt-screen #t)
]

The structure is nearly identical. The main changes are:
@itemlist[#:style 'ordered
  @item{Return the new state directly, or @racket[(cmd state command)] to attach a command}
  @item{@racket[key-msg] struct instead of a key-event string}
  @item{@racket[(text str)] instead of @racket[(text str size color)]}
  @item{An explicit quit key, since there is no window close button}
]
