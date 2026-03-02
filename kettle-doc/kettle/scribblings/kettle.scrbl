#lang scribble/manual
@(require (for-label racket/base
                     racket/generic
                     kettle))

@title{Kettle: Terminal User Interface Library}
@author{Anthony Green}

@defmodule[kettle]

Kettle is a Racket library for building rich Terminal User Interfaces
(TUIs). It implements the TEA (The Elm Architecture) pattern---model,
update, view---for reactive terminal applications. Kettle is a port of
the Common Lisp @tt{cl-tuition} library.

The name ``Kettle'' comes from the fact that a kettle makes TEA.

@table-of-contents[]

@include-section["protocol.scrbl"]
@include-section["terminal.scrbl"]
@include-section["input.scrbl"]
@include-section["style.scrbl"]
@include-section["layout.scrbl"]
@include-section["borders.scrbl"]
@include-section["renderer.scrbl"]
@include-section["program.scrbl"]
@include-section["components.scrbl"]
@include-section["testing.scrbl"]
@include-section["porting-from-big-bang.scrbl"]

@index-section[]
