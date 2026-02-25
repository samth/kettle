#lang scribble/manual
@(require (for-label racket/base
                     kettle))

@title{Components}

Reusable TUI components. Each component follows the TEA pattern with
its own init, update, and view functions that can be composed into
larger applications.

Components are available as submodules: @racket[kettle/components/list-view],
@racket[kettle/components/table], @racket[kettle/components/spinner],
@racket[kettle/components/progress], @racket[kettle/components/textinput],
@racket[kettle/components/textarea], @racket[kettle/components/viewport],
and @racket[kettle/components/paginator].

@section{List View}
@defmodule[kettle/components/list-view]

A scrollable list with keyboard navigation (arrow keys and vim-style j/k).

@defproc[(make-list-view [#:items items list? '()]
                         [#:height height exact-positive-integer? 10]) list-view?]{
  Create a scrollable list view.}

@defproc[(list-view-update [lv list-view?] [msg msg?]) (or/c list-view? cmd?)]{
  Handle input for the list view. Returns either an updated list view
  (no command) or @racket[(cmd list-view command)].}

@defproc[(list-view-render [lv list-view?]) string?]{
  Render the list view to a string.}

@defproc[(list-view-get-selected [lv list-view?]) any/c]{
  Get the currently selected item.}

@section{Table}
@defmodule[kettle/components/table]

Table rendering with borders, headers, and auto-calculated column widths.

@defproc[(make-table [#:headers headers list? '()]
                     [#:rows rows (listof list?) '()]
                     [#:border border (or/c border? #f) #f]) table?]{
  Create a table.}

@defproc[(table-render [tbl table?]) string?]{
  Render the table to a string.}

@section{Spinner}
@defmodule[kettle/components/spinner]

Animated spinner with predefined styles: @racket[spinner-line],
@racket[spinner-dot], @racket[spinner-minidot], @racket[spinner-jump],
@racket[spinner-pulse], @racket[spinner-points], @racket[spinner-globe],
@racket[spinner-moon], @racket[spinner-monkey], @racket[spinner-meter],
@racket[spinner-hamburger], @racket[spinner-ellipsis].

@defproc[(make-spinner [#:frames frames (listof string?) spinner-line]
                       [#:fps fps number? 0.1]) spinner?]{
  Create an animated spinner.}

@section{Progress Bar}
@defmodule[kettle/components/progress]

Customizable progress bar with percentage display.

@defproc[(make-progress [#:percent percent (between/c 0.0 1.0) 0.0]
                        [#:width width exact-positive-integer? 40]
                        [#:show-percentage show-pct boolean? #t]) progress?]{
  Create a progress bar.}

@defproc[(progress-set-percent! [pb progress?] [pct (between/c 0.0 1.0)]) void?]{
  Set the progress percentage.}

@defproc[(progress-increment! [pb progress?] [amount number?]) void?]{
  Increment the progress by @racket[amount].}

@section{Text Input}
@defmodule[kettle/components/textinput]

Single-line text input with cursor movement, kill ring, undo/redo,
placeholder text, and password echo mode.

@defproc[(make-textinput [#:value value string? ""]
                         [#:placeholder placeholder string? ""]
                         [#:prompt prompt string? "> "]
                         [#:width width exact-positive-integer? 20]
                         [#:echo-mode echo-mode (or/c 'normal 'password) 'normal]) textinput?]{
  Create a text input. Supports Emacs-style keybindings: Ctrl+a/e for
  home/end, Ctrl+k/u for kill, Ctrl+y for yank, Ctrl+z for undo, Alt+z
  for redo, Alt+b/f for word movement.}

@section{Text Area}
@defmodule[kettle/components/textarea]

Multi-line text editor with line numbers.

@defproc[(make-textarea [#:width width exact-positive-integer? 40]
                        [#:height height exact-positive-integer? 6]
                        [#:placeholder placeholder string? ""]
                        [#:show-line-numbers show-ln boolean? #t]) textarea?]{
  Create a multi-line text area.}

@section{Viewport}
@defmodule[kettle/components/viewport]

Scrollable content viewer with vim-style navigation (j/k, g/G, Ctrl+d/u,
h/l for horizontal scrolling) and mouse wheel support.

@defproc[(make-viewport [#:width width exact-positive-integer? 80]
                        [#:height height exact-positive-integer? 24]
                        [#:content content (or/c string? #f) #f]) viewport?]{
  Create a scrollable viewport.}

@section{Paginator}
@defmodule[kettle/components/paginator]

Pagination UI with arabic numeral or dot display.

@defproc[(make-paginator [#:type type (or/c 'arabic 'dots) 'arabic]
                         [#:per-page per-page exact-positive-integer? 10]
                         [#:total-pages total-pages exact-positive-integer? 1]) paginator?]{
  Create a paginator.}
