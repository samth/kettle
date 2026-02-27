# Kettle Design Notes

How Kettle's design came together, what was borrowed from where, and the
key decisions that shaped the library.

---

## Sources of Inspiration

### The Elm Architecture

Elm is a purely functional language for web front-ends. Its architecture
enforces a strict separation between application logic and side effects
via four functions:

```elm
Browser.element
  { init          : flags -> (model, Cmd msg)
  , update        : msg -> model -> (model, Cmd msg)
  , view          : model -> Html msg
  , subscriptions : model -> Sub msg
  }
```

The runtime manages all side effects. `update` never performs I/O
directly; instead it returns a `Cmd` value describing what to do, and
the runtime executes it and feeds the resulting `Msg` back into
`update`. Subscriptions are declarative: the program returns a
description of what external events to listen to, and the runtime
manages starting and stopping listeners as the description changes.

Kettle takes the full four-function shape. `gen:kettle-model` mirrors
`Browser.element`:

```racket
(define-generics kettle-model
  (init kettle-model)           ; -> model or (cmd model command)
  (update kettle-model msg)     ; -> model or (cmd model command)
  (view kettle-model)           ; -> image
  (subscriptions kettle-model)) ; -> (listof subscription)
```

The `cmd` wrapper (returning both a new model and a side-effect
descriptor from `init`/`update`) is a direct adaptation of Elm's
`(model, Cmd msg)` tuple. The subscription diffing (keyed by identity,
started/stopped automatically each cycle) follows Elm's model.

### Racket's `big-bang` (2htdp/universe)

`big-bang` is Racket's teaching-language event loop. It predates TEA and
comes from the How to Design Programs curriculum:

```racket
(big-bang initial-world
  [to-draw    (-> World Image)]
  [on-tick    (-> World World)]
  [on-key     (-> World Key World)]
  [stop-when  (-> World Boolean)])
```

The "World" can be any Racket value. You only provide the clauses you
need. There is no interface or protocol to implement; just pass handler
functions. The rendering target is `2htdp/image`, an algebraic image
type with composition operators like `beside`, `above`, `overlay`, and
`place-image`.

Kettle's `run` function (in `kettle/run.rkt`) is directly modeled on
`big-bang`:

```racket
(run initial-value
  #:on-key   (-> value key-msg? value)
  #:on-tick  (-> value value)
  #:to-view  (-> value image?)
  #:stop-when (-> value boolean?))
```

The clause-based keyword API, the "model is any value" philosophy, and
the incremental opt-in (start with just `#:to-view`, add handlers as
needed) all come from `big-bang`. Internally, `run` wraps the value in a
`run-model` struct that implements `gen:kettle-model` and calls
`program-run`, bridging the simple API to the full architecture.

### lux and raart

lux (by Jay McCarthy) is a Racket framework that generalizes `big-bang`
by separating the event loop from the rendering backend. A **chaos** is
a rendering/interaction backend; a **word** is the program state. Words
implement the `gen:word` generic interface:

```racket
(struct my-app (state)
  #:methods gen:word
  [(define (word-event w e) ...)
   (define (word-output w)  ...)])

(call-with-chaos (make-charterm-chaos)
  (lambda () (fiat-lux (my-app initial))))
```

raart is lux's companion library for terminal rendering. It provides an
algebraic ASCII art type with composition operators:

- `text`, `char`, `blank` (primitives)
- `happend`, `vappend` (horizontal/vertical concatenation)
- `place-at`, `crop`, `inset` (spatial transforms)
- `fg`, `bg`, `style` (styling wrappers)
- Alignment via `halign/c` (`'left`, `'center`, `'right`) and `valign/c`
  (`'top`, `'center`, `'bottom`)

Kettle borrows from both. The `gen:kettle-model` generic interface
pattern comes from `gen:word`. The image module uses nearly identical
naming and structure to raart:

| raart         | Kettle             |
|---------------|--------------------|
| `text`        | `image:text`       |
| `char`        | `image:char`       |
| `blank`       | `image:blank`      |
| `happend`     | `image:hcat`       |
| `vappend`     | `image:vcat`       |
| `crop`        | `image:crop`       |
| `inset`       | `image:pad`        |
| `fg`/`bg`     | `image:styled`     |
| `halign/c`    | `'left`/`'center`/`'right` alignment arg |

The key difference is that raart images are rendered directly by lux's
chaos, while Kettle's images are painted into a cell buffer (via
tui-ubuf) with dirty tracking for minimal terminal output.

### Nottui

Nottui is an OCaml library for terminal UIs built on the Notty rendering
library. Its distinctive feature is **stretchable dimensions**, a
concept borrowed from TeX: each UI element has a fixed size plus a
stretchable component representing layout strength. When elements are
composed via `hcat`/`vcat`, the available space is divided among
stretchable elements proportionally to their strength.

The `Ui.resize` function controls this:

```ocaml
Ui.resize ~sw:1 ~mw:40 element   (* stretch width 1, max width 40 *)
```

Kettle's `image:flex` struct is directly inspired by this system:

```racket
;; Stretchable layout (Nottui-inspired)
;; sw = stretch width priority, mw = max width (or #f for unbounded)
;; sh = stretch height priority, mh = max height (or #f for unbounded)
(struct image:flex image (sw mw sh mh inner))
```

The `flex` smart constructor marks regions of an image tree as
stretchable with a priority and optional maximum. This is currently used
during layout constraint solving and is passed through during painting
(the paint phase ignores `flex` and paints the inner image directly).

### Bubble Tea

Bubble Tea is a Go TUI framework that adapts TEA to Go's type system.
The core interface:

```go
type Model interface {
    Init() tea.Cmd
    Update(msg tea.Msg) (tea.Model, tea.Cmd)
    View() string
}
```

Commands are `func() tea.Msg` -- thunks that perform I/O and return a
message. `tea.Batch(cmds...)` runs commands concurrently.
`tea.Sequence(cmds...)` runs them in order. The view is a plain string;
the framework handles alternate screen and cursor management.

Kettle adopts several Bubble Tea patterns:

- **Commands as thunks**: A command is a `(-> msg)` procedure run in its
  own thread; the return value is sent as a message. This is the same
  design as `tea.Cmd`.
- **Batch and sequence**: `batch` filters nil commands and runs them
  concurrently. `cmd-sequence` runs thunks serially in one thread. These
  mirror `tea.Batch` and `tea.Sequence`.
- **`exec-cmd`**: Suspends the terminal, runs an external program, and
  resumes. Mirrors Bubble Tea's `tea.Exec`.
- **Message types**: `key-msg`, `window-size-msg`, `tick-msg`,
  `mouse-press-event`, etc. map directly to Bubble Tea's `tea.KeyMsg`,
  `tea.WindowSizeMsg`, `tea.MouseMsg`.
- **Component composition via delegation**: The `delegate` function
  forwards messages to a child model and bubbles commands back up,
  following the same manual delegation pattern Bubble Tea uses.

Where Kettle diverges: the view returns an algebraic image tree rather
than a string, and the message types are a struct hierarchy rather than
Go's `interface{}`.

### Lip Gloss

Lip Gloss is Bubble Tea's companion styling library. It provides a
CSS-inspired declarative API:

```go
style := lipgloss.NewStyle().
    Foreground(lipgloss.Color("#FAFAFA")).
    Background(lipgloss.Color("#7D56F4")).
    Bold(true).
    Padding(0, 1)

rendered := style.Render("Hello")
```

Styles are immutable values with copy semantics. Colors support adaptive
variants (pick light/dark based on terminal background) and complete
variants (degrade gracefully across truecolor/256/16 support).

Kettle's style system is modeled on Lip Gloss. The `make-style`
constructor takes keyword arguments for the same properties:

```racket
(make-style #:foreground fg-bright-cyan
            #:background bg-black
            #:bold #t
            #:padding-left 1)
```

The `adaptive-color` and `complete-color` structs parallel Lip Gloss's
`AdaptiveColor` and `CompleteColor`. The string-level layout functions
`join-horizontal`, `join-vertical`, and `place` in Kettle's `layout.rkt`
directly mirror Lip Gloss's `JoinHorizontal`, `JoinVertical`, and
`Place`. These are the legacy string-based API; the image tree provides
a more structured alternative.

### cl-tuition

cl-tuition (by Anthony Green) is a Common Lisp TUI library that ports
the Charmbracelet ecosystem to CLOS. Kettle began as a Racket port of
cl-tuition.

cl-tuition uses CLOS multiple dispatch for message handling:

```common-lisp
(defmethod tui:update-message ((m my-model) (msg tui:key-msg))
  (cond ((string= (tui:key-msg-key msg) "q")
         (tui:quit-cmd))
        (t (values m nil))))
```

Specializing `update-message` on both the model class and the message
class gives per-message-type methods without a big `cond`/`match`. The
message hierarchy (`key-msg`, `quit-msg`, `window-size-msg`,
`mouse-press-event`, etc.), the command helpers (`quit-cmd`, `batch`,
`cmd-sequence`, `make-exec-cmd`), and the component library (text input,
spinner, progress bar, list view, table) were all carried over from
cl-tuition to Kettle.

Where Kettle diverges from cl-tuition:

- **Racket generics instead of CLOS**: `gen:kettle-model` via
  `define-generics` replaces CLOS generic functions. Racket doesn't have
  multiple dispatch on message type, so Kettle's `update` uses `match`
  on the message struct.
- **Algebraic image tree**: cl-tuition's `view` returns a string (like
  Bubble Tea). Kettle adds a structured image type for composition and
  cell-buffer rendering, drawing on raart and 2htdp/image.
- **`define-kettle-program` macro**: Generates a struct with
  `gen:kettle-model` methods from declarative clauses, replacing the
  CLOS class + defmethod pattern.
- **The `run` entry point**: The big-bang-style simple API has no
  equivalent in cl-tuition.

### Ratatui

Ratatui is a Rust TUI library with a constraint-based layout system.
Constraints describe how to divide space:

```rust
let layout = Layout::vertical([
    Constraint::Length(3),     // fixed 3 rows
    Constraint::Fill(1),       // fill remaining
    Constraint::Length(1),     // fixed 1 row
]);
```

Constraint types: `Length` (fixed), `Percentage`, `Ratio`, `Min`, `Max`,
`Fill` (proportional fill of remaining space).

Kettle's `layout-constraints.rkt` is inspired by Ratatui's system:

```racket
(split-vertical total-height (fixed 3) (fill) (fixed 1))
;; => '(3 20 1)  for total-height = 24
```

Kettle provides `fixed`, `percent`, `ratio`, `fill`, `min-size`, and
`max-size` constraints with the same semantics. The solver is simpler
than Ratatui's Cassowary-based approach: a two-pass algorithm that
allocates fixed/percent/ratio first, then distributes remaining space
equally among fills (with remainder distributed one cell at a time to
the first fills).

---

## Key Design Decisions

### Algebraic image trees instead of string views

The most significant departure from the Bubble Tea / cl-tuition lineage
is that `view` returns an `image` rather than a string. The image tree
carries precomputed dimensions and is inspectable and composable:

```racket
(vcat 'left
  (styled header-style (text "Title"))
  (hcat 'top sidebar content)
  (text "Status"))
```

This decision was motivated by:

1. **Correct composition**: String-based horizontal joining requires
   splitting on newlines and padding to equal width. Image trees compose
   directly: `hcat` sums widths and takes the max height.
2. **Cell-buffer rendering**: The paint phase walks the image tree and
   writes characters with styles into a grid. This enables per-cell
   dirty tracking (via tui-ubuf) for minimal ANSI output, which matters
   at high refresh rates.
3. **Style inheritance**: `image:styled` applies a style to an entire
   subtree. The `merge-style` function during painting lets outer styles
   provide defaults that inner styles override, analogous to CSS
   cascading.
4. **Testing**: `image->string` converts an image to a string for
   assertion-based testing without needing a terminal.

The string-based layout functions (`join-horizontal`, `join-vertical`,
`place` in `layout.rkt`) are retained as a legacy path for simple cases
and for compatibility with cl-tuition-style code.

### Two entry points

Kettle provides two ways to write programs, bridging the gap between
the teaching-language tradition and the production-framework tradition:

1. **`run`** (big-bang-style): Takes any value and keyword handler
   functions. No protocol to implement, no struct to define. Good for
   small programs.

2. **`define-kettle-program`** (TEA-style): Generates a struct
   implementing `gen:kettle-model` with named fields, init, update,
   view, and subscriptions. Good for programs with complex state and
   multiple message types.

Both call `program-run` internally. The `run` function wraps its value
in a `run-model` struct that implements `gen:kettle-model`, so the event
loop only needs to know about one protocol.

### Commands as thunks

Following Bubble Tea rather than Elm, commands are procedures rather
than algebraic data. A command is a `(-> msg)` thunk run in its own
thread:

```racket
(cmd model (lambda () (do-something) (make-result-msg)))
```

This is more pragmatic than Elm's approach (where commands are opaque
data interpreted by the runtime) because Racket doesn't have effect
handlers or a compiler that can optimize effect descriptions. Thunks are
the natural unit of deferred computation in Racket.

The `batch` and `cmd-sequence` combinators provide concurrent and serial
composition, mirroring Bubble Tea.

### Declarative subscriptions with key-based diffing

Subscriptions are evaluated after every update batch. The runtime diffs
the old and new subscription lists by key and starts/stops threads
accordingly. This follows Elm exactly: subscriptions are a pure function
of the model, and the runtime manages the lifecycle.

The subscription types (`every`, `on-resize`, `watch-port`) are simple
struct values. Each running subscription is a thread that sends messages
to the event loop's async channel. An `equal?` short-circuit skips the
diff when the subscription list hasn't changed (the common case).

### Cell buffer via tui-ubuf

Rather than emitting ANSI strings directly, Kettle paints into a
cell buffer (`ubuf` from the `tui-ubuf` library). Each cell holds a
character and a style. tui-ubuf handles dirty tracking, wide character
support (CJK/emoji occupy two cells), and minimal diff output.

The rendering pipeline is:

```
image tree -> paint! -> cell buffer -> display-ubuf-cells -> terminal
```

Between rows, tui-ubuf emits `ESC[0m ESC[K \r\n` (reset attributes,
erase to end of line, newline) to prevent styled content from bleeding
into the right margin.

### Message batching

The event loop drains all pending messages from the async channel before
processing, then processes the entire batch before rendering once. This
prevents redundant renders when multiple events arrive between frames
(common with mouse motion or rapid keyboard input). Consecutive scroll
events in the same direction are coalesced into a single event with an
incremented count.

### Raw terminal via `#%terminal`

Terminal control uses Racket's built-in `#%terminal` API (inherited from
Chez Scheme's expeditor), which wraps `ioctl` and `termios` directly.
This avoids shelling out to `stty` and gives reliable raw mode
enter/exit within `dynamic-wind` for cleanup on exceptions.

Everything else (alternate screen, cursor visibility, mouse tracking,
bracketed paste, focus events, Kitty keyboard protocol) is done with
ANSI escape sequences written to `/dev/tty`.

---

## What Came from Where: Summary

| Feature | Primary source | Adaptation in Kettle |
|---|---|---|
| Model-Update-View loop | Elm | `gen:kettle-model` with `init`, `update`, `view`, `subscriptions` |
| Commands as return values from update | Elm, Bubble Tea | `(cmd model thunk)` wrapper |
| Commands as thunks | Bubble Tea | `(-> msg)` procedures run in threads |
| `batch` / `cmd-sequence` | Bubble Tea | Same semantics |
| `exec-cmd` (suspend TUI, run program) | Bubble Tea | Same semantics |
| Declarative subscriptions | Elm | `every`, `on-resize`, `watch-port`; diffed by key |
| Clause-based simple API | `big-bang` | `run` with `#:on-key`, `#:on-tick`, `#:to-view`, `#:stop-when` |
| Model is any value (simple API) | `big-bang` | `run` accepts any value, no protocol needed |
| Generic interface for model | lux `gen:word` | `gen:kettle-model` via `define-generics` |
| Algebraic image tree | raart, 2htdp/image | `image:text`, `hcat`, `vcat`, `zcat`, `crop`, `pad`, `styled` |
| Stretchable/flexible layout | Nottui | `image:flex` with stretch priority and max bounds |
| Constraint-based space splitting | Ratatui | `fixed`, `percent`, `fill`, `ratio`, `min-size`, `max-size` |
| CSS-like style objects | Lip Gloss | `make-style` with keyword args, `adaptive-color`, `complete-color` |
| String-level layout (`join-horizontal`, etc.) | Lip Gloss | `layout.rkt` (legacy path) |
| Message struct hierarchy | cl-tuition | `key-msg`, `mouse-event`, `window-size-msg`, `tick-msg`, etc. |
| Component library | cl-tuition / Bubbles | `textinput`, `textarea`, `list-view`, `table`, `spinner`, `progress` |
| `delegate` for component composition | Bubble Tea | Forwards messages to child, bubbles commands |
| `define-kettle-program` macro | cl-tuition (CLOS classes) | Replaces `defclass` + `defmethod` with declarative struct generation |
| Cell-buffer rendering | tui-ubuf | Paint image tree into ubuf grid, dirty-tracked output |
| Border rendering | cl-tuition / Lip Gloss | Box-drawing characters with named border styles |
