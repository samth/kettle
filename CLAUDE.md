# Kettle

A Racket library for building Terminal User Interfaces (TUIs). Ported from the Common Lisp `cl-tuition` library.

## Package Structure

Six-package layout:
- `kettle/` - Meta package (depends on kettle-lib and kettle-doc)
- `kettle-lib/` - Implementation: `kettle/main.rkt` re-exports the full API
- `kettle-doc/` - Scribble documentation in `kettle/scribblings/`
- `kettle-test-lib/` - Test support library: `kettle/test` (headless) and `kettle/test-tmux` (tmux e2e)
- `kettle-components/` - Extra components: datepicker, markdown, overlay, zone-manager
- `kettle-examples/` - Example programs ported from cl-tuition and original demos

## Architecture

Kettle pattern: model, update, view via `gen:kettle-model` generic interface. See [IMPLEMENTATION.md](IMPLEMENTATION.md) for the rendering pipeline, event loop internals, and module dependency graph.

Key modules in `kettle-lib/kettle/private/`:
- `protocol.rkt` - Core Kettle generic interface, message types, commands
- `terminal.rkt` - Raw terminal control via `#%terminal` API (ioctl-based raw mode and size detection)
- `input.rkt` - Keyboard/mouse input parsing (CSI, SGR mouse, bracketed paste)
- `style.rkt` - ANSI styling, colors (16/256/truecolor), text measurement
- `image.rkt` - Algebraic image tree (text, hcat, vcat, zcat, crop, pad, styled, flex)
- `layout.rkt` - String-based horizontal/vertical joining and placement (legacy)
- `borders.rkt` - Box border rendering
- `renderer.rkt` - Cell-buffer renderer (paint image tree -> diff -> minimal ANSI output)
- `program.rkt` - Kettle event loop (`program-run`, `define-kettle-program`)
- `run.rkt` - Big-bang-style entry point (wraps plain values in `gen:kettle-model`)
- `errors.rkt` - Exception types (`exn:fail:kettle`)
- `subscriptions.rkt` - Elm-style subscriptions (timers, resize, port watchers)
- `layout-constraints.rkt` - Constraint-based layout splitting

Components in `kettle-lib/kettle/components/`:
- `textinput.rkt`, `textarea.rkt` - Text entry
- `list-view.rkt`, `table.rkt` - Data display
- `spinner.rkt`, `progress.rkt` - Status indicators
- `paginator.rkt`, `viewport.rkt` - Navigation

## Development

```bash
# Install packages for development
raco pkg install --auto ./kettle-lib ./kettle-test-lib ./kettle-doc ./kettle

# Run with recompilation
racket -y <filename>

# Build docs
raco setup --pkgs kettle-doc
```

## Testing

All tests live in `kettle-test/kettle/tests/`:
- **Unit tests** (`test-input.rkt`, `test-renderer.rkt`) -- test individual modules directly
- **Integration tests** (`test-integration.rkt`) -- headless Kettle loop via `kettle/test`
- **E2E tests** (`test-e2e.rkt`) -- full programs in tmux via `kettle/test-tmux`
- **Benchmarks** (`bench-render.rkt`, `bench-log-viewer.rkt`) -- rendering performance

```bash
# Run all tests
raco test -y kettle-test/kettle/tests/

# Run e2e only (requires tmux)
raco test -y kettle-test/kettle/tests/test-e2e.rkt
```

## Manual Testing with tmux

Use `tmux-run.sh` to run a Kettle program in a tmux session, optionally send keys, and capture output:

```bash
# Run a program, wait for startup, capture output
kettle-test/kettle/tests/tmux-run.sh <racket-file> [wait-secs] [keys-to-send] [post-key-wait]

# Examples:
kettle-test/kettle/tests/tmux-run.sh test-kitty-manual.rkt 10
kettle-test/kettle/tests/tmux-run.sh test-kitty-manual.rkt 10 "+" 2
```

This is useful for testing TUI programs that need a real terminal (e.g. Kitty keyboard protocol, mouse input, alternate screen).

**Important:** Always use `tmux-run.sh` for manual tmux testing. Do not construct raw tmux commands directly.

## Documentation Files

These documents should be kept in sync with code changes:

- **[IMPLEMENTATION.md](IMPLEMENTATION.md)** - Rendering pipeline, event loop internals, module dependency graph, terminal control, input parsing, subscription system. Update when changing the architecture of any core module.
- **[DESIGN.md](DESIGN.md)** - Design rationale, sources of inspiration, what was borrowed from which library. Update when adding new architectural patterns or significantly changing existing ones.
- **[UPSTREAM_IMPROVEMENTS.md](UPSTREAM_IMPROVEMENTS.md)** - Issues and workarounds in `ansi` and `tui-ubuf` dependencies. Update when adding new workarounds or when upstream fixes land.

## License

MIT
