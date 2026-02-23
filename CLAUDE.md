# Kettle

A Racket library for building Terminal User Interfaces (TUIs) using The Elm Architecture (TEA) pattern. Ported from the Common Lisp `cl-tuition` library.

## Package Structure

Three-package layout:
- `kettle/` - Meta package (depends on kettle-lib and kettle-doc)
- `kettle-lib/` - Implementation: `kettle/main.rkt` re-exports the full API
- `kettle-doc/` - Scribble documentation in `kettle/scribblings/`

## Architecture

TEA pattern: model, update, view via `gen:tea-model` generic interface.

Key modules in `kettle-lib/kettle/private/`:
- `protocol.rkt` - Core TEA generic interface, message types, commands
- `terminal.rkt` - Raw terminal control (alt screen, cursor, mouse)
- `input.rkt` - Keyboard/mouse input parsing
- `style.rkt` - ANSI styling, colors, text measurement
- `layout.rkt` - Horizontal/vertical joining and placement
- `borders.rkt` - Box border rendering
- `renderer.rkt` - Diffing renderer
- `program.rkt` - TEA program runner (`program-run`, `defprogram`)
- `errors.rkt` - Exception types (`exn:fail:kettle`)

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

TUI programs require a terminal. Use Xvfb for headless testing if needed.

## License

MIT
