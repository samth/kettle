#lang scribble/manual
@(require (for-label racket/base
                     rackunit
                     kettle
                     kettle/test
                     kettle/test-tmux))

@title{Testing Support}

Kettle provides two testing libraries for verifying TUI programs: a
headless test harness for fast, deterministic unit and integration
tests, and a tmux-based harness for full end-to-end tests in a real
terminal.

Both libraries are provided by the @tt{kettle-test-lib} package.

@; ============================================================
@section{Headless Testing}

@defmodule[kettle/test]

The headless test harness runs Kettle programs synchronously without a
terminal or threads. It reimplements the TEA loop in a simplified form
that supports sending messages, inspecting model state, and rendering
views as strings.

This is the recommended approach for testing update logic and view
rendering. Tests are fast, deterministic, and don't require tmux or a
pseudo-terminal.

@subsection{Construction}

@defproc[(make-test-program [model kettle-model?]
                            [#:width width exact-positive-integer? 80]
                            [#:height height exact-positive-integer? 24])
         test-program?]{
  Create a test program from a @racket[kettle-model?] value. Runs
  @racket[init] on the model, processes any init commands, sends an
  initial @racket[window-size-msg], and initializes subscriptions.}

@defproc[(make-test-program/run [initial-value any/c]
                                [#:on-key on-key-fn (or/c procedure? #f) #f]
                                [#:on-tick on-tick-fn (or/c procedure? #f) #f]
                                [#:tick-rate tick-rate real? 1]
                                [#:on-msg on-msg-fn (or/c procedure? #f) #f]
                                [#:to-view view-fn (or/c procedure? #f) #f]
                                [#:stop-when stop-fn (or/c procedure? #f) #f]
                                [#:width width exact-positive-integer? 80]
                                [#:height height exact-positive-integer? 24])
         test-program?]{
  Create a test program from @racket[run]-style arguments. Wraps the
  value in an internal @racket[kettle-model?] adapter that dispatches
  key messages to @racket[on-key-fn], tick messages to @racket[on-tick-fn],
  and other messages to @racket[on-msg-fn].}

@subsection{Event Injection}

@defproc[(test-program-send [tp test-program?] [msg msg?]) void?]{
  Send any message to the test program. No-op if the program is already
  done.}

@defproc[(test-program-press [tp test-program?]
                             [key (or/c char? symbol?)]
                             [#:alt alt boolean? #f]
                             [#:ctrl ctrl boolean? #f])
         void?]{
  Simulate a key press by sending a @racket[key-msg].}

@defproc[(test-program-release [tp test-program?]
                               [key (or/c char? symbol?)]
                               [#:alt alt boolean? #f]
                               [#:ctrl ctrl boolean? #f]
                               [#:shift shift boolean? #f])
         void?]{
  Simulate a key release (Kitty keyboard protocol) by sending a
  @racket[key-release-msg].}

@defproc[(test-program-type [tp test-program?] [str string?]) void?]{
  Type a string character by character, sending a @racket[key-msg] for
  each character.}

@defproc[(test-program-resize [tp test-program?]
                              [width exact-positive-integer?]
                              [height exact-positive-integer?])
         void?]{
  Send a @racket[window-size-msg] to simulate a terminal resize.}

@subsection{State Inspection}

@defproc[(test-program-model [tp test-program?]) kettle-model?]{
  Return the current model.}

@defproc[(test-program-value [tp test-program?]) any/c]{
  Return the unwrapped value for @racket[run]-style programs. For
  programs created with @racket[make-test-program], returns the model
  directly.}

@defproc[(test-program-view [tp test-program?]) any/c]{
  Return the current view (typically an @racket[image?]).}

@defproc[(test-program-view-string [tp test-program?]) string?]{
  Render the current view as a plain string. Useful for assertions.}

@defproc[(test-program-done? [tp test-program?]) boolean?]{
  Return @racket[#t] if the program has received a @racket[quit-msg].}

@defproc[(test-program-history [tp test-program?]) list?]{
  Return the history as a list of @racket[(cons model view)] pairs,
  in reverse chronological order (most recent first).}

@defproc[(test-program-subscriptions [tp test-program?]) list?]{
  Return the current subscription specs. Note that subscriptions are
  tracked but not started in the test harness.}

@subsection{Headless Rackunit Checks}

@defproc[(check-test-program-contains [tp test-program?]
                                      [expected-text string?])
         void?]{
  Check that the rendered view contains @racket[expected-text].}

@defproc[(check-test-program-done [tp test-program?]) void?]{
  Check that the program is done (has received @racket[quit-msg]).}

@defproc[(check-test-program-running [tp test-program?]) void?]{
  Check that the program is still running.}

@subsection{Headless Test Example}

@codeblock|{
#lang racket/base
(require rackunit kettle/test)

;; Test a run-style counter
(define tp
  (make-test-program/run
   0
   #:on-key (lambda (count msg)
              (match msg
                [(key-msg #\+ _ _) (add1 count)]
                [(key-msg #\q _ _) (cmd count (quit-cmd))]
                [_ count]))
   #:to-view (lambda (count)
               (format "Count: ~a" count))))

(check-test-program-contains tp "Count: 0")
(test-program-press tp #\+)
(check-test-program-contains tp "Count: 1")
(test-program-press tp #\q)
(check-test-program-done tp)
}|

@; ============================================================
@section{tmux End-to-End Testing}

@defmodule[kettle/test-tmux]

The tmux test harness runs Kettle programs in real pseudo-terminals via
tmux. This enables true end-to-end testing with actual terminal
rendering, input parsing, and ANSI escape sequence handling.

Requires tmux to be installed. Tests using this harness should check
@racket[tmux-available?] and skip when tmux is not present.

@subsection{Session Management}

@defstruct[tmux-session ([name string?]
                         [width exact-positive-integer?]
                         [height exact-positive-integer?])
           #:transparent]{
  Represents a running tmux session.}

@defproc[(tmux-start [module-path path-string?]
                     [#:width width exact-positive-integer? 80]
                     [#:height height exact-positive-integer? 24]
                     [#:session-name session-name (or/c string? #f) #f]
                     [#:startup-delay startup-delay real? 0.1]
                     [#:args extra-args list? '()])
         tmux-session?]{
  Start a Kettle program in a new tmux session. The program is launched
  with @tt{racket -y} inside a bash session, so the tmux session persists
  even if the program exits or errors.

  @racket[extra-args] are passed as command-line arguments to the
  Racket program.}

@defproc[(tmux-kill [session tmux-session?]) void?]{
  Kill a tmux session. Silently ignores errors if the session is already
  dead.}

@defform[(with-tmux-session ([var module-path kw-arg ...] ...)
           body ...+)]{
  Start one or more tmux sessions and execute @racket[body]. All sessions
  are automatically killed on exit (including on exception).}

@defproc[(tmux-available?) boolean?]{
  Return @racket[#t] if tmux is installed and working.}

@subsection{Key Injection}

@defproc[(tmux-send-keys [session tmux-session?] [key-spec string?]) void?]{
  Send a key specification to the tmux session. Uses tmux key names:
  @racket["a"], @racket["C-q"], @racket["Enter"], @racket["Up"],
  @racket["Down"], @racket["Space"], @racket["Tab"], etc.}

@defproc[(tmux-send-raw [session tmux-session?] [raw-string string?]) void?]{
  Send raw bytes to the session, bypassing tmux key interpretation.
  Useful for injecting escape sequences like CSI u (Kitty keyboard
  protocol).}

@defproc[(tmux-type [session tmux-session?]
                    [str string?]
                    [#:delay delay real? 0.01])
         void?]{
  Type a string character by character with a delay between each
  keystroke.}

@subsection{Screen Capture}

@defproc[(tmux-capture [session tmux-session?]
                       [#:trim trim boolean? #f])
         string?]{
  Capture the current contents of the tmux pane. If @racket[trim] is
  @racket[#t], leading and trailing whitespace is removed.}

@subsection{Waiting}

@defproc[(tmux-wait-for [session tmux-session?]
                        [pattern (or/c string? regexp?)]
                        [#:timeout timeout real? 5]
                        [#:interval interval real? 0.05])
         (or/c string? #f)]{
  Poll the tmux pane until @racket[pattern] appears. Returns the
  captured pane contents on success, or @racket[#f] on timeout.

  If @racket[pattern] is a string, it is matched literally (via
  @racket[regexp-quote]).}

@subsection{tmux Rackunit Checks}

@defproc[(check-tmux-contains [session tmux-session?]
                              [expected-text string?])
         void?]{
  Check that the tmux pane contains @racket[expected-text].}

@defproc[(check-tmux-matches [session tmux-session?]
                             [pattern (or/c string? regexp?)])
         void?]{
  Check that the tmux pane matches @racket[pattern].}

@defproc[(check-tmux-not-contains [session tmux-session?]
                                  [text string?])
         void?]{
  Check that the tmux pane does @emph{not} contain @racket[text].}

@defproc[(check-quit-exits [session tmux-session?]
                           [quit-key string?]
                           [marker string?])
         void?]{
  Send @racket[quit-key] and check that @racket[marker] disappears from
  the pane (indicating the TUI exited and the alternate screen was
  restored). Best used with programs that enable @tt{alt-screen}.}

@subsection{DSL Helpers}

@defproc[(send+wait [session tmux-session?]
                    [key string?]
                    [expected string?]
                    [#:timeout timeout real? 5])
         (or/c string? #f)]{
  Send a key and wait for @racket[expected] text to appear. Convenience
  wrapper around @racket[tmux-send-keys] and @racket[tmux-wait-for].}

@defform[(with-e2e-sessions ([id path kw-arg ...] ...)
           body ...+)]{
  Start multiple tmux sessions @emph{in parallel} (each in its own
  thread) and execute @racket[body] with all sessions bound. All sessions
  are automatically killed on exit.

  This is the recommended way to structure e2e test suites, as it
  amortizes program startup time across multiple test subjects.}

@subsection{tmux Test Example}

@codeblock|{
#lang racket/base
(require rackunit
         setup/collects
         kettle/test-tmux)

(define counter-path
  (collection-file-path "counter.rkt" "kettle" "examples"))

(when (tmux-available?)
  (with-e2e-sessions ([s counter-path #:width 80 #:height 24])
    (test-case "counter shows initial count"
      (tmux-wait-for s "Count: 0" #:timeout 10)
      (check-tmux-contains s "Count: 0"))
    (test-case "increment works"
      (send+wait s "+" "Count: 1")
      (check-tmux-contains s "Count: 1"))
    (test-case "quit exits"
      (check-quit-exits s "q" "increment"))))
}|
