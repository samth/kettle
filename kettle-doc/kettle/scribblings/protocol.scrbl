#lang scribble/manual
@(require (for-label racket/base
                     racket/generic
                     kettle))

@title{Core TEA Protocol}

Kettle uses the TEA (The Elm Architecture) pattern. Every application
defines a model that implements the @racket[gen:tea-model] generic
interface.

@section{Generic Interface}

@defthing[gen:tea-model any/c]{
  A generic interface for TEA models. Implementations should define
  @racket[init], @racket[update], and @racket[view].}

@defproc[(tea-model? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] implements @racket[gen:tea-model].}

@defproc[(init [model tea-model?]) (values tea-model? (or/c procedure? #f))]{
  Initialize the model. Returns @racket[(values model cmd)] where
  @racket[cmd] is an optional initial command (or @racket[#f]).}

@defproc[(update [model tea-model?] [message msg?]) (values tea-model? (or/c procedure? #f))]{
  Handle a message and return @racket[(values new-model cmd)].}

@defproc[(view [model tea-model?]) string?]{
  Render the model to a string for terminal display.}

@section{Messages}

@defstruct[msg () #:transparent]{
  Base message type. All messages inherit from this.}

@defstruct[(quit-msg msg) () #:transparent]{
  Signals the program should exit.}

@defstruct[(key-msg msg) ([key (or/c char? symbol?)]
                          [alt boolean?]
                          [ctrl boolean?]) #:transparent]{
  A keyboard input event. The @racket[key] field is either a character
  or a symbol such as @racket['up], @racket['down], @racket['left],
  @racket['right], @racket['enter], @racket['backspace], @racket['escape],
  @racket['tab], @racket['delete], @racket['home], @racket['end],
  @racket['page-up], or @racket['page-down].}

@defstruct[(paste-msg msg) ([text string?]) #:transparent]{
  Text pasted via bracketed paste mode.}

@defstruct[(window-size-msg msg) ([width exact-nonnegative-integer?]
                                  [height exact-nonnegative-integer?]) #:transparent]{
  Terminal window resize event.}

@defstruct[(tick-msg msg) ([time number?]) #:transparent]{
  A timer tick event.}

@defstruct[(suspend-msg msg) () #:transparent]{
  Terminal suspend event.}

@defstruct[(resume-msg msg) () #:transparent]{
  Terminal resume event.}

@defstruct[(focus-in-msg msg) () #:transparent]{
  Terminal gained focus.}

@defstruct[(focus-out-msg msg) () #:transparent]{
  Terminal lost focus.}

@section{Mouse Events}

@defstruct[(mouse-event msg) ([x exact-nonnegative-integer?]
                              [y exact-nonnegative-integer?]
                              [shift boolean?]
                              [alt boolean?]
                              [ctrl boolean?]) #:transparent]{
  Base mouse event with position and modifier keys.}

@defstruct[(mouse-button-event mouse-event) ([button (or/c 'left 'middle 'right #f)]) #:transparent]{
  Mouse event with a button.}

@defstruct[(mouse-press-event mouse-button-event) () #:transparent]{
  Mouse button press.}

@defstruct[(mouse-release-event mouse-button-event) () #:transparent]{
  Mouse button release.}

@defstruct[(mouse-drag-event mouse-button-event) () #:transparent]{
  Mouse drag (motion with button held).}

@defstruct[(mouse-move-event mouse-event) () #:transparent]{
  Mouse motion without a button.}

@defstruct[(mouse-scroll-event mouse-event) ([direction (or/c 'up 'down)]
                                              [count exact-positive-integer?]) #:transparent]{
  Mouse scroll wheel event.}

@defstruct[(mouse-msg msg) ([x exact-nonnegative-integer?]
                            [y exact-nonnegative-integer?]
                            [button (or/c 'left 'middle 'right #f)]
                            [shift boolean?]
                            [alt boolean?]
                            [ctrl boolean?]
                            [action symbol?]) #:transparent]{
  Legacy mouse message (prefer the typed mouse event hierarchy above).}

@section{Commands}

@defproc[(quit-cmd) procedure?]{
  Returns a command that causes the program to quit.}

@defproc[(batch [cmd (or/c procedure? #f)] ...) list?]{
  Combine multiple commands to run concurrently.}

@defproc[(cmd-sequence [cmd (or/c procedure? #f)] ...) list?]{
  Combine multiple commands to run in sequence.}

@defproc[(tick-cmd [duration number?] [fn (or/c procedure? #f) #f]) procedure?]{
  Returns a command that sleeps for @racket[duration] seconds then
  produces a @racket[tick-msg].}

@defstruct[exec-cmd ([program string?]
                     [args (listof string?)]
                     [callback (or/c procedure? #f)]) #:transparent]{
  A command that suspends the TUI, runs an external program, then resumes.}

@defproc[(key-string [km key-msg?]) string?]{
  Extract the key from a @racket[key-msg] as a string.}
