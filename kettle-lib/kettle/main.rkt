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
         "private/program.rkt")

(provide
 ;; === Errors ===
 (struct-out exn:fail:kettle)
 (struct-out exn:fail:kettle:terminal)
 (struct-out exn:fail:kettle:terminal:operation)
 (struct-out exn:fail:kettle:input)
 current-error-handler
 handle-kettle-error

 ;; === Core TEA Protocol ===
 gen:tea-model
 tea-model?
 init
 update
 view

 ;; === Messages ===
 (struct-out msg)
 (struct-out quit-msg)
 (struct-out key-msg)
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
 fg-black fg-red fg-green fg-yellow
 fg-blue fg-magenta fg-cyan fg-white
 fg-bright-black fg-bright-red fg-bright-green fg-bright-yellow
 fg-bright-blue fg-bright-magenta fg-bright-cyan fg-bright-white

 ;; Background colors
 bg-black bg-red bg-green bg-yellow
 bg-blue bg-magenta bg-cyan bg-white
 bg-bright-black bg-bright-red bg-bright-green bg-bright-yellow
 bg-bright-blue bg-bright-magenta bg-bright-cyan bg-bright-white

 ;; Style
 (struct-out style)
 make-style
 render-styled
 bold
 italic
 underline
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
 split-string-by-newline

 ;; === Layout ===
 top middle bottom
 left center right
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
 move-cursor-home
 clear-to-end-of-screen
 move-cursor

 ;; === Program ===
 (struct-out program)
 make-program
 program-send
 program-quit
 program-kill
 program-stop
 program-run
 current-program
 defprogram)
