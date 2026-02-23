#lang scribble/manual
@(require (for-label racket/base
                     kettle))

@title{Layout}

Utilities for joining and positioning text blocks.

@section{Position Constants}

@defthing[top symbol?]{Vertical alignment: top.}
@defthing[middle symbol?]{Vertical alignment: middle.}
@defthing[bottom symbol?]{Vertical alignment: bottom.}
@defthing[left symbol?]{Horizontal alignment: left.}
@defthing[center symbol?]{Horizontal alignment: center.}
@defthing[right symbol?]{Horizontal alignment: right.}

@section{Joining}

@defproc[(join-horizontal [position (or/c 'top 'middle 'bottom)]
                          [block string?] ...) string?]{
  Join text blocks side-by-side, aligned vertically at @racket[position].}

@defproc[(join-vertical [position (or/c 'left 'center 'right)]
                        [block string?] ...) string?]{
  Stack text blocks vertically, aligned horizontally at @racket[position].}

@section{Placement}

@defproc[(place-horizontal [width exact-nonnegative-integer?]
                           [position (or/c 'left 'center 'right)]
                           [text string?]) string?]{
  Place text horizontally in a space of the given width.}

@defproc[(place-vertical [height exact-nonnegative-integer?]
                         [position (or/c 'top 'middle 'bottom)]
                         [text string?]) string?]{
  Place text vertically in a space of the given height.}

@defproc[(place [width exact-nonnegative-integer?]
                [height exact-nonnegative-integer?]
                [h-pos (or/c 'left 'center 'right)]
                [v-pos (or/c 'top 'middle 'bottom)]
                [text string?]) string?]{
  Place text in a @racket[width] @"×" @racket[height] space.}

@section{Measurement}

@defproc[(block-width [text string?]) exact-nonnegative-integer?]{
  Width of the widest line in a text block.}

@defproc[(block-height [text string?]) exact-nonnegative-integer?]{
  Number of lines in a text block.}
