#lang scribble/manual
@(require (for-label racket/base
                     kettle))

@title{Renderer}

Efficient terminal rendering engine with differential updates.

@defstruct[renderer ([last-output string?]
                     [output-stream output-port?]) #:transparent]{
  A renderer that tracks the last output to avoid unnecessary redraws.}

@defproc[(make-renderer [output-stream output-port? (current-output-port)]) renderer?]{
  Create a new renderer.}

@defproc[(render! [r renderer?] [view-string string?]) void?]{
  Render the view string to the terminal. Only redraws if the output
  has changed since the last call.}

@defproc[(move-cursor-home [stream output-port? (current-output-port)]) void?]{
  Move the cursor to the home position (1,1).}

@defproc[(clear-to-end-of-screen [stream output-port? (current-output-port)]) void?]{
  Clear from the cursor to the end of the screen.}

@defproc[(move-cursor [row exact-positive-integer?]
                      [col exact-positive-integer?]
                      [stream output-port? (current-output-port)]) void?]{
  Move the cursor to a specific position (1-indexed).}
