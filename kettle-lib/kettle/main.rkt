#lang racket/base

;; main.rkt
;;
;; SPDX-License-Identifier: MIT
;;
;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;; Racket port
;;
;; Kettle: A Racket library for building TUIs
;; Port of the Common Lisp cl-tuition library.
;;
;; This module re-exports the full public API.

(require "private/errors.rkt"
         "private/protocol.rkt"
         "private/terminal.rkt"
         "private/input.rkt"
         "private/style.rkt"
         "private/layout.rkt"
         "private/borders.rkt"
         "private/renderer.rkt"
         "private/image.rkt"
         "private/program.rkt"
         "private/subscriptions.rkt"
         "private/layout-constraints.rkt")

;; === Errors ===
(provide (struct-out exn:fail:kettle)
         (struct-out exn:fail:kettle:terminal)
         (struct-out exn:fail:kettle:terminal:operation)
         (struct-out exn:fail:kettle:input)
         current-error-handler
         handle-kettle-error

         ;; === Core Kettle Protocol ===
         gen:kettle-model
         kettle-model?
         init
         update
         view
         subscriptions
         delegate

         ;; === Messages ===
         (struct-out msg)
         (struct-out quit-msg)
         (struct-out key-msg)
         (struct-out key-event-msg)
         (struct-out key-release-msg)
         key-msg-event-type
         key-press?
         key-repeat?
         key-release?
         (struct-out kitty-query-response-msg)
         (struct-out paste-msg)
         (struct-out window-size-msg)
         (struct-out tick-msg)
         (struct-out suspend-msg)
         (struct-out resume-msg)
         (struct-out focus-in-msg)
         (struct-out focus-out-msg)

         ;; === Mouse Events ===
         (struct-out mouse-event)
         (struct-out mouse-button-event)
         (struct-out mouse-press-event)
         (struct-out mouse-release-event)
         (struct-out mouse-drag-event)
         (struct-out mouse-move-event)
         (struct-out mouse-scroll-event)
         (struct-out mouse-msg)

         ;; === Update Result ===
         (struct-out cmd)
         extract-update-result

         ;; === Commands ===
         quit-cmd
         batch
         cmd-sequence
         tick-cmd
         (struct-out exec-cmd)
         key-string

         ;; === Terminal ===
         enter-raw-mode
         exit-raw-mode
         get-terminal-size
         set-terminal-title
         clear-screen
         hide-cursor
         show-cursor
         enter-alt-screen
         exit-alt-screen
         enable-mouse-cell-motion
         enable-mouse-all-motion
         disable-mouse
         enable-bracketed-paste
         disable-bracketed-paste
         enable-focus-events
         disable-focus-events
         suspend-terminal
         resume-terminal
         with-raw-terminal
         open-tty

         ;; === Input ===
         read-key
         read-all-available-events
         current-input-stream

         ;; === Image ===
         image?
         image-w
         image-h
         text
         blank
         char-image
         hcat
         vcat
         zcat
         crop
         pad
         styled
         flex
         hcat*
         vcat*
         newline-image
         empty-image
         string->image
         image-empty?
         ensure-image
         bold
         italic
         underline
         image->string

         ;; Image struct types (for pattern matching)
         image:text
         image:text?
         image:text-str
         image:text-style
         image:blank
         image:blank?
         image:char
         image:char?
         image:hcat
         image:hcat?
         image:hcat-align
         image:hcat-children
         image:vcat
         image:vcat?
         image:vcat-align
         image:vcat-children
         image:zcat
         image:zcat?
         image:zcat-children
         image:crop
         image:crop?
         image:pad
         image:pad?
         image:styled
         image:styled?
         image:styled-style
         image:styled-inner
         image:flex
         image:flex?
         image:flex-sw
         image:flex-mw
         image:flex-sh
         image:flex-mh
         image:flex-inner

         ;; === Styling ===
         ansi-color
         ansi-reset
         color-256
         color-rgb
         parse-hex-color
         (struct-out adaptive-color)
         (struct-out complete-color)
         (struct-out complete-adaptive-color)
         detect-dark-background
         detect-color-support
         resolve-color
         resolve-adaptive-color

         ;; Foreground colors
         fg-black
         fg-red
         fg-green
         fg-yellow
         fg-blue
         fg-magenta
         fg-cyan
         fg-white
         fg-bright-black
         fg-bright-red
         fg-bright-green
         fg-bright-yellow
         fg-bright-blue
         fg-bright-magenta
         fg-bright-cyan
         fg-bright-white

         ;; Background colors
         bg-black
         bg-red
         bg-green
         bg-yellow
         bg-blue
         bg-magenta
         bg-cyan
         bg-white
         bg-bright-black
         bg-bright-red
         bg-bright-green
         bg-bright-yellow
         bg-bright-blue
         bg-bright-magenta
         bg-bright-cyan
         bg-bright-white

         ;; Style
         (struct-out style)
         make-style
         render-styled
         colored

         ;; Reflow
         wrap-text
         truncate-text
         ellipsize
         indent-lines

         ;; Measurement
         text-width
         text-height
         text-size
         visible-length
         char-display-width
         split-string-by-newline

         ;; Bidi
         bidi-isolate
         bidi-isolate-ltr

         ;; === Layout (string-based, legacy) ===
         top
         middle
         bottom
         left
         center
         right
         join-horizontal
         join-vertical
         place-horizontal
         place-vertical
         place
         block-width
         block-height

         ;; === Borders ===
         (struct-out border)
         make-border
         render-border
         render-shadow
         shadow-chars
         border-normal
         border-rounded
         border-thick
         border-double
         border-block
         border-hidden
         border-ascii
         border-markdown

         ;; === Renderer ===
         (struct-out renderer)
         make-renderer
         render!
         render-image!
         move-cursor-home
         clear-to-end-of-screen
         move-cursor

         ;; === Subscriptions ===
         every
         on-resize
         watch-port
         (struct-out sub:every)
         (struct-out sub:resize)
         (struct-out sub:port)
         subscription?
         subscription-key
         start-subscription
         stop-subscription
         diff-subscriptions

         ;; === Layout Constraints ===
         (struct-out constraint)
         fixed
         percent
         fill
         min-size
         max-size
         ratio
         split-horizontal
         split-vertical

         ;; === Program ===
         (struct-out program)
         make-program
         program-send
         program-quit
         program-kill
         program-stop
         program-run
         program-show-fps!
         current-program
         define-kettle-program

         ;; === Renderer (for testing / advanced use) ===
         make-cell-buffer
         cell-buffer->string
         paint!)
