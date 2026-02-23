#lang scribble/manual
@(require (for-label racket/base
                     kettle))

@title{Input Handling}

Reading and parsing keyboard and mouse input from the terminal.

@defproc[(read-key) (or/c msg? #f)]{
  Read a single key from input and return a @racket[key-msg] or mouse
  event. Returns @racket[#f] if no input is available.}

@defproc[(read-all-available-events) (listof msg?)]{
  Read all available input events and return them as a list.}

@defparam[current-input-stream port input-port?]{
  The port to read terminal input from. Defaults to
  @racket[(current-input-port)].}
