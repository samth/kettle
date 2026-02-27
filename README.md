# Kettle

A Racket library for building Terminal User Interfaces (TUIs). Ported from the Common Lisp `cl-tuition` library.

Kettle follows the **model-update-view** pattern (inspired by Elm/Haskell Brick): define your state, how it updates in response to messages, and how it renders -- Kettle handles the terminal, event loop, and rendering.

## Examples

### Counter

A minimal program showing the model-update-view pattern. The entire model is a single number.

```bash
racket kettle-examples/kettle/examples/counter.rkt
```

![Counter](doc/gifs/counter.gif)

### Stopwatch

ASCII-art stopwatch with lap recording, styled rendering, and tick-based subscriptions.

```bash
racket kettle-examples/kettle/examples/stopwatch.rkt
```

![Stopwatch](doc/gifs/stopwatch.gif)

### Todo List

Component composition with text input and list view. Shows mode switching and delegated updates.

```bash
racket kettle-examples/kettle/examples/todo.rkt
```

![Todo](doc/gifs/todo.gif)

### Snake

Classic snake game with collision detection, score tracking, and timer subscriptions.

```bash
racket kettle-examples/kettle/examples/snake.rkt
```

![Snake](doc/gifs/snake.gif)

### Text Viewer

Scrollable file viewer using the viewport component with styled borders.

```bash
racket kettle-examples/kettle/examples/viewer.rkt README.md
```

![Viewer](doc/gifs/viewer.gif)

### Process Viewer

A `top`/`htop`-style process viewer with real-time updates, sortable columns, CPU/memory bars, and mouse support.

```bash
racket kettle-examples/kettle/examples/top.rkt
```

![Top](doc/gifs/top.gif)

## Getting Started

```racket
#lang racket/base
(require kettle/run kettle/image kettle/program)

(define (on-key count msg)
  (match msg
    [(key-msg #\q _ _) (cmd count (quit-cmd))]
    [(key-msg #\+ _ _) (add1 count)]
    [_ count]))

(define (to-view count)
  (vcat 'left
    (format "Count: ~a" count)
    "  + increment  q quit"))

(run 0 #:on-key on-key #:to-view to-view)
```

## Installation

```bash
raco pkg install --auto ./kettle-lib ./kettle-doc ./kettle
```

## Package Structure

- **kettle/** -- Meta package (depends on kettle-lib and kettle-doc)
- **kettle-lib/** -- Implementation: `kettle/main.rkt` re-exports the full API
- **kettle-doc/** -- Scribble documentation in `kettle/scribblings/`
- **kettle-examples/** -- Example programs

## Key Modules

| Module | Description |
|--------|-------------|
| `kettle/run` | Big-bang-style entry point |
| `kettle/program` | Event loop, `define-kettle-program`, messages |
| `kettle/image` | Algebraic image tree (text, hcat, vcat, styled, flex) |
| `kettle/style` | ANSI styling, colors (16/256/truecolor) |
| `kettle/border` | Box border rendering |
| `kettle/components/textinput` | Text input component |
| `kettle/components/textarea` | Multi-line text area |
| `kettle/components/list-view` | Navigable list |
| `kettle/components/table` | Table display |
| `kettle/components/viewport` | Scrollable viewport |
| `kettle/components/spinner` | Loading spinner |
| `kettle/components/progress` | Progress bar |

## Testing

```bash
# Run all tests
raco test -y kettle-test/kettle/tests/

# Run e2e tests (requires tmux)
raco test -y kettle-test/kettle/tests/test-e2e.rkt
```

## Recording GIFs

The `record-gif.sh` tool records scripted interactions with a Kettle program as animated GIFs, using asciinema and [agg](https://github.com/asciinema/agg).

```bash
# Record a program with a script
kettle-examples/kettle/examples/record-gif.sh <script-file> <output.gif> [options]
```

See `record-gif.sh --help` and the `.script` files in `kettle-examples/kettle/examples/recordings/` for the scripting format.

## License

MIT
