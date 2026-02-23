#lang scribble/manual
@(require (for-label racket/base
                     kettle))

@title{Terminal Control}

Low-level terminal control for raw mode, cursor management, and
terminal capabilities.

@defproc[(enter-raw-mode) void?]{
  Put the terminal in raw mode for TUI applications. Saves the
  original @tt{stty} settings for later restoration.}

@defproc[(exit-raw-mode) void?]{
  Restore the terminal to its original state.}

@defproc[(get-terminal-size) (cons/c exact-nonnegative-integer? exact-nonnegative-integer?)]{
  Get the current terminal size as @racket[(cons width height)].
  Returns @racket[(cons 80 24)] on failure.}

@defproc[(set-terminal-title [title string?]) void?]{
  Set the terminal window title.}

@defproc[(clear-screen [port output-port? (current-output-port)]) void?]{
  Clear the terminal screen.}

@defproc[(hide-cursor [port output-port? (current-output-port)]) void?]{
  Hide the terminal cursor.}

@defproc[(show-cursor [port output-port? (current-output-port)]) void?]{
  Show the terminal cursor.}

@defproc[(enter-alt-screen [port output-port? (current-output-port)]) void?]{
  Switch to the alternate screen buffer.}

@defproc[(exit-alt-screen [port output-port? (current-output-port)]) void?]{
  Switch back from the alternate screen buffer.}

@defproc[(enable-mouse-cell-motion [port output-port? (current-output-port)]) void?]{
  Enable mouse cell-motion tracking (reports button presses and drags).}

@defproc[(enable-mouse-all-motion [port output-port? (current-output-port)]) void?]{
  Enable mouse all-motion tracking (reports all movement).}

@defproc[(disable-mouse [port output-port? (current-output-port)]) void?]{
  Disable mouse tracking.}

@defproc[(enable-bracketed-paste [port output-port? (current-output-port)]) void?]{
  Enable bracketed paste mode.}

@defproc[(disable-bracketed-paste [port output-port? (current-output-port)]) void?]{
  Disable bracketed paste mode.}

@defproc[(enable-focus-events [port output-port? (current-output-port)]) void?]{
  Enable terminal focus reporting.}

@defproc[(disable-focus-events [port output-port? (current-output-port)]) void?]{
  Disable terminal focus reporting.}

@defproc[(suspend-terminal [#:alt-screen alt-screen boolean? #f]
                           [#:mouse mouse (or/c 'cell-motion 'all-motion #f) #f]
                           [#:focus-events focus-events boolean? #f]) procedure?]{
  Suspend the terminal for backgrounding. Returns a thunk that restores
  TUI state when called.}

@defproc[(resume-terminal [restore-fn procedure?]) void?]{
  Resume the terminal using the restore function from @racket[suspend-terminal].}

@defproc[(with-raw-terminal [thunk (-> any)]
                            [#:alt-screen alt-screen boolean? #f]
                            [#:mouse mouse (or/c 'cell-motion 'all-motion #f) #f]
                            [#:focus-events focus-events boolean? #t]) any]{
  Execute @racket[thunk] with the terminal in raw TUI mode. The terminal
  is restored on exit, even if an exception is raised.}

@defproc[(open-tty) list?]{
  Open @tt{/dev/tty} for direct terminal I/O. Returns a list of
  @racket[(input-port output-port raw-input-port)].}
