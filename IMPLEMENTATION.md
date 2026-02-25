# Kettle Implementation Guide

Internal reference for the rendering pipeline, event loop, terminal control, and module organization.

## Module Dependency Graph

```
main.rkt (re-exports everything)
  |
  +-- program.rkt (TEA event loop, program-run)
  |     +-- protocol.rkt (gen:tea-model, messages, commands)
  |     +-- terminal.rkt (raw mode, alt screen, cursor)
  |     +-- input.rkt (keyboard/mouse parsing)
  |     +-- renderer.rkt (cell buffer, paint, diff)
  |     +-- subscriptions.rkt (timers, resize, port watchers)
  |     +-- errors.rkt (exn hierarchy)
  |
  +-- image.rkt (algebraic image tree)
  |     +-- style.rkt (ANSI codes, text measurement)
  |
  +-- layout.rkt (string-based join/place, legacy)
  +-- borders.rkt (box drawing)
  +-- layout-constraints.rkt (split-horizontal, split-vertical)
  +-- run.rkt (big-bang-style wrapper around program-run)

components/ (each implements gen:tea-model):
  textinput.rkt, textarea.rkt, list-view.rkt, table.rkt,
  spinner.rkt, progress.rkt, paginator.rkt, viewport.rkt
```

## TEA Protocol (`protocol.rkt`)

The core generic interface:

```racket
(define-generics tea-model
  (init tea-model)           ; -> model or (cmd model command)
  (update tea-model msg)     ; -> new-model or (cmd new-model command)
  (view tea-model)           ; -> image
  (subscriptions tea-model)) ; -> list of subscription specs
```

All four methods have defaults (identity init, no-op update, empty string view, empty subscriptions).

`init` and `update` return either a bare model (no command) or a `(cmd model command)` wrapper when a command should be executed. The `cmd` struct is transparent with two fields: `model` and `value`. Use `extract-update-result` to destructure the return value into two values:

```racket
(define-values (m c) (extract-update-result (update model msg)))
```

### Messages

All messages inherit from `(struct msg () #:transparent)`:
- `key-msg` (key alt ctrl) -- keyboard input
- `window-size-msg` (width height) -- terminal resize
- `tick-msg` (time) -- timer tick
- `quit-msg` -- request shutdown
- `paste-msg` (text) -- bracketed paste
- `focus-in-msg`, `focus-out-msg` -- terminal focus
- Mouse hierarchy: `mouse-event` -> `mouse-button-event` -> press/release/drag; `mouse-move-event`; `mouse-scroll-event`

### Commands

Commands are the `value` field of a `cmd` wrapper (or absent when a bare model is returned):
- `#f` -- no-op (equivalent to returning a bare model)
- Procedure -- run in a thread, send return value as message (e.g. `quit-cmd` returns a thunk that produces `quit-msg`)
- List -- batch of commands, run concurrently
- `(cons 'sequence ...)` -- run thunks sequentially in one thread
- `exec-cmd` -- suspend terminal, run external program, resume

### Component Composition

`delegate` dispatches a message to a child component:

```racket
(define (delegate parent getter setter msg)
  (define result (update (getter parent) msg))
  (if (cmd? result)
      (cmd (setter parent (cmd-model result)) (cmd-value result))
      (setter parent result)))
```

## Image Type System (`image.rkt`)

Images are an algebraic data type -- immutable trees carrying precomputed `(w h)` dimensions. The base struct is `(struct image (w h))` with subtypes:

**Primitives:**
- `image:text` (str style) -- single line of text, width = `visible-length`
- `image:blank` -- empty rectangle
- `image:char` (ch style) -- single character

**Composition:**
- `image:hcat` (align children) -- horizontal concat, width = sum of children, height = max
- `image:vcat` (align children) -- vertical concat, height = sum of children, width = max
- `image:zcat` (children) -- overlay, front-to-back z-order

**Transforms:**
- `image:crop` (col row inner) -- viewport into inner image
- `image:pad` (left right top bottom inner) -- add whitespace
- `image:styled` (style inner) -- apply ANSI style to subtree
- `image:flex` (sw mw sh mh inner) -- stretchable layout marker

Smart constructors (`text`, `hcat`, `vcat`, etc.) filter out empty images, collapse singletons, and handle multi-line strings (split into `vcat` of `text` lines).

## Rendering Pipeline (`renderer.rkt`)

The pipeline is: **image tree -> paint -> cell buffer -> ANSI string -> diff -> terminal**.

### Cell Buffer

A `cell-buffer` is a width x height grid of `(struct cell (ch style))` backed by a vector-of-vectors. The empty cell is `(cell #\space #f)`.

### Paint Phase (`paint!`)

`paint!` walks the image tree recursively, writing characters into the cell buffer at `(x, y)` coordinates:

- `image:text` -- calls `paint-text!` which handles inline ANSI escape sequences, tracking style overrides
- `image:hcat` -- paints children left-to-right, advancing x by each child's width. Vertical alignment offsets are computed per child
- `image:vcat` -- paints children top-to-bottom, advancing y by each child's height. Horizontal alignment offsets are computed per child
- `image:zcat` -- paints back-to-front (reverse order), so earlier children overlay later ones
- `image:crop` -- paints inner at offset `(x - col, y - row)`, relying on bounds checking in `cell-buffer-set!` for clipping
- `image:pad` -- paints inner at `(x + left, y + top)`
- `image:styled` -- merges style with `merge-style` (outer provides defaults, inner overrides) and continues recursively
- `image:flex` -- passes through to inner (flex is only meaningful during layout constraint solving)

Wide characters (CJK, emoji) occupy 2 cells. The second cell gets a `#\nul` placeholder that is skipped during emission.

### Emission Phase (`cell-buffer->string`)

Iterates row by row, emitting ANSI escape sequences on style transitions:
- Tracks `last-style` per row
- On style change: emit reset, then new style's opening sequence
- Skips `#\nul` placeholders (wide char second cells)
- Resets at end of each row
- Uses `\r\n` between rows (not bare `\n`) because raw mode disables OPOST, so LF alone doesn't return the cursor to column 1

### Diff and Output

`render-image!` compares the new ANSI string against the previous frame. If unchanged, no output is emitted. Otherwise: `move-cursor-home` (ESC[H), write the string, `clear-to-end-of-screen` (ESC[J), flush.

`image->string` does paint + emit without diff, for testing.

## Event Loop (`program.rkt`)

### Startup Sequence

`program-run` does:
1. Open `/dev/tty` (unbuffered input port + append-mode output port)
2. Enter raw mode, hide cursor, clear screen, optionally enter alt screen / enable mouse
3. Start input thread reading from tty
4. Query terminal size via `get-terminal-size`, send `window-size-msg`
5. Call `init` on the model, run init command if any
6. Render initial `view`
7. Start initial subscriptions
8. Enter the event loop

### Message Processing

The event loop blocks on `sync/timeout 0.1` on the async channel. When a message arrives, it drains all pending messages, then processes the batch:

1. Coalesce consecutive scroll events in the same direction (increment count)
2. For each message: call `update`, accumulate commands
3. Run all accumulated commands
4. Render the view once (single `render!` call for the entire batch)
5. Re-evaluate subscriptions via `sync-subscriptions!`

A `quit-msg` sets `running?` to `#f` and exits the loop.

### Command Execution

- Procedures run in their own thread. The return value is sent as a message
- Batch commands are iterated and each dispatched via `run-command`
- Sequences run in a single thread, calling each thunk in order
- `exec-cmd` suspends the terminal (exit raw mode, show cursor, exit alt screen), runs the external program via `system`, then resumes

### Subscriptions

Subscriptions are keyed by identity. After each batch, `sync-subscriptions!` diffs old vs new subscription lists and starts/stops accordingly:
- `sub:every` -- spawns a thread that `sleep`s for the interval, then sends a `tick-msg`
- `sub:resize` -- marker only; resize is handled by the input thread or SIGWINCH
- `sub:port` -- spawns a thread reading from a port

## Terminal Control (`terminal.rkt`)

Uses Racket's built-in `#%terminal` API (from Chez Scheme's expeditor, wrapping `ioctl` and `termios`):

- `terminal-init -1 -1` -- initialize with inherited stdin/stdout (fd 0/1)
- `terminal-raw-mode #t/#f` -- equivalent to `stty raw -echo` / `stty sane`
- `terminal-get-screen-size` -- returns `(cons rows cols)` via `ioctl(TIOCGWINSZ)`

Everything else (alt screen, cursor visibility, mouse tracking, bracketed paste, focus events) is done with ANSI escape sequences written to the tty output port.

`get-terminal-size` returns `(cons width height)` (note: transposed from the `#%terminal` return value which is rows-first). Falls back to `(cons 80 24)` on error.

`with-raw-terminal` wraps a thunk in `dynamic-wind` to ensure cleanup (exit alt screen, disable mouse, show cursor, exit raw mode) even on exceptions.

## Input Parsing (`input.rkt`)

Reads from a configurable `current-input-stream` parameter (set to the tty input port by program.rkt).

`read-key` is non-blocking (uses `byte-ready?`). It handles:
- ASCII control characters (mapped via `ctrl-char-to-key`)
- DEL (127) as backspace
- ESC sequences: CSI (ESC [), SS3 (ESC O), Alt+key (ESC + printable)
- CSI numeric parameters for function keys, modified arrows (shift/alt/ctrl), delete, page up/down
- SGR mouse tracking (ESC [ < Cb;Cx;Cy M/m)
- Bracketed paste (ESC [200~ ... ESC [201~)
- Focus events (ESC [I / ESC [O)

`read-all-available-events` drains all immediately available input into a list.

## Style System (`style.rkt`)

The `style` struct carries: foreground, background, bold?, italic?, underline?, blink?, reverse?, strikethrough?, faint?, padding (4 sides), margin (4 sides), width, height, max-width, max-height, inline?, align.

Colors are ANSI code strings: "31" for red, "38;2;R;G;B" for truecolor. Adaptive colors choose between light/dark background variants. Complete colors degrade gracefully across truecolor -> 256color -> 16color.

`visible-length` computes display width excluding ANSI escapes. `char-display-width` handles combining characters (width 0) and East Asian wide characters (width 2).

`render-styled` applies a style to a plain string: padding, width/alignment, height, ANSI codes. It re-applies outer style codes after any inner resets to handle nested styling.

## Two Entry Points

1. **`define-tea-program`** (macro in `program.rkt`) -- generates a struct implementing `gen:tea-model` with `#:fields`, `#:init`, `#:update`, `#:view`, `#:subscriptions` clauses. Used by stopwatch, todo, viewer examples.

2. **`run`** (in `run.rkt`) -- big-bang-style API. Takes an initial value and `#:on-key`, `#:on-msg`, `#:to-view`, `#:stop-when` handlers. Internally wraps the value in a `run-model` struct that implements `gen:tea-model`. Used by the counter example.

Both ultimately call `program-run`.

## Testing Infrastructure

Three tiers:
- **Unit tests** (`kettle-lib/kettle/tests/`) -- test individual modules directly
- **Integration tests** (`test-integration.rkt`) -- headless TEA loop via `kettle/test` (synchronous, no terminal)
- **E2E tests** (`test-e2e.rkt`) -- full programs in tmux sessions via `kettle/test-tmux`

The headless test harness (`kettle/test`) reimplements the TEA loop synchronously: `make-test-program` constructs a test program, `test-program-send` processes messages, `test-program-view-string` renders to a string for assertions.

The tmux harness (`kettle/test-tmux`) uses `-L kettle-test` for socket isolation to prevent interference from user tmux sessions. `tmux-start` creates a detached session, `tmux-send-keys` injects input, `tmux-capture` reads the pane, `tmux-wait-for` polls until expected output appears.
