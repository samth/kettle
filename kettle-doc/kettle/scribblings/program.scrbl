#lang scribble/manual
@(require (for-label racket/base
                     kettle))

@title{Program}

The main application loop that ties together the TEA model, terminal,
input, and rendering.

@defstruct[program ([model tea-model?]
                    [renderer* renderer?]
                    [msg-channel any/c]
                    [running? boolean?]
                    [input-paused? boolean?]
                    [options list?]
                    [tty-stream any/c]
                    [input-thread (or/c thread? #f)]) #:transparent]{
  A running TUI program.}

@defproc[(make-program [model tea-model?]
                       [#:alt-screen alt-screen boolean? #f]
                       [#:mouse mouse (or/c 'cell-motion 'all-motion #f) #f]) program?]{
  Create a new program with the given initial model.

  @itemlist[
    @item{@racket[alt-screen]: if @racket[#t], use the alternate screen buffer.}
    @item{@racket[mouse]: enable mouse tracking---@racket['cell-motion] for
          button presses and drags, or @racket['all-motion] for all movement.}
  ]}

@defproc[(program-run [p program?]) void?]{
  Run the program's main loop. Blocks until the program exits.}

@defproc[(program-send [p program?] [m msg?]) void?]{
  Send a message to the program's update loop.}

@defproc[(program-quit [p program?]) void?]{
  Quit the program gracefully by sending a @racket[quit-msg].}

@defproc[(program-kill [p program?]) void?]{
  Kill the program immediately.}

@defproc[(program-stop [p program?]) void?]{
  Request the program to stop.}

@defparam[current-program p (or/c program? #f)]{
  The currently running program, set during @racket[program-run].}

@defform[(defprogram name
           #:fields ([field-name field-default] ...)
           #:init init-body
           #:update update-body
           #:view view-body)]{
  Convenience macro for defining a TEA model struct with
  @racket[gen:tea-model] methods.

  @racketblock[
    (defprogram counter
      #:fields ([count 0] [width 80])
      #:init (values model #f)
      #:update
        (cond
          [(and (key-msg? msg) (equal? (key-msg-key msg) #\q))
           (values model (quit-cmd))]
          [else (values model #f)])
      #:view
        (format "Count: ~a" (counter-count model)))
  ]}
