# Kettle

A Racket library for building Terminal User Interfaces (TUIs) using The Elm Architecture (TEA) pattern. Ported from the Common Lisp `cl-tuition` library.

## Package Structure

Three-package layout:
- `kettle/` - Meta package (depends on kettle-lib and kettle-doc)
- `kettle-lib/` - Implementation: `kettle/main.rkt` re-exports the full API
- `kettle-doc/` - Scribble documentation in `kettle/scribblings/`

## Architecture

TEA pattern: model, update, view via `gen:tea-model` generic interface. See [IMPLEMENTATION.md](IMPLEMENTATION.md) for the rendering pipeline, event loop internals, and module dependency graph.

Key modules in `kettle-lib/kettle/private/`:
- `protocol.rkt` - Core TEA generic interface, message types, commands
- `terminal.rkt` - Raw terminal control via `#%terminal` API (ioctl-based raw mode and size detection)
- `input.rkt` - Keyboard/mouse input parsing (CSI, SGR mouse, bracketed paste)
- `style.rkt` - ANSI styling, colors (16/256/truecolor), text measurement
- `image.rkt` - Algebraic image tree (text, hcat, vcat, zcat, crop, pad, styled, flex)
- `layout.rkt` - String-based horizontal/vertical joining and placement (legacy)
- `borders.rkt` - Box border rendering
- `renderer.rkt` - Cell-buffer renderer (paint image tree -> diff -> minimal ANSI output)
- `program.rkt` - TEA event loop (`program-run`, `define-tea-program`)
- `run.rkt` - Big-bang-style entry point (wraps plain values in `gen:tea-model`)
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
raco pkg install --auto ./kettle-lib ./kettle-doc ./kettle

# Run with recompilation
racket -y <filename>

# Build docs
raco setup --pkgs kettle-doc
```

## Testing

Three tiers of tests in `kettle-lib/kettle/tests/`:
- **Unit tests** -- test individual modules directly
- **Integration tests** (`test-integration.rkt`) -- headless TEA loop via `kettle/test`
- **E2E tests** (`test-e2e.rkt`) -- full programs in tmux via `kettle/test-tmux`

```bash
# Run all tests
raco test -y kettle-lib/kettle/tests/

# Run only unit + integration (no tmux needed)
raco test -y kettle-lib/kettle/tests/ --exclude test-e2e.rkt

# Run e2e only (requires tmux)
raco test -y kettle-lib/kettle/tests/test-e2e.rkt
```

## License

MIT
