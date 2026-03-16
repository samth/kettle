# Online Notes: Source-Level Review of Racket terminal/TUI libraries vs Kettle

This revision is based on actually cloning and reading source code where possible.

## Retrieval status

All packages successfully cloned/downloaded and inspected from source:
- `tui-term` (`https://gitlab.com/racketeer/tui-term.git`)
- `tui-ubuf` (`https://gitlab.com/racketeer/tui-ubuf.git`)
- `termios` (`https://github.com/BartAdv/racket-termios.git`)
- `termconfig` (the likely package behind “term config”; `https://github.com/dodgez/termconfig.git`)
- `rktermios` (`https://gitlab.com/racketeer/rktermios.git`)
- `ansi` (`https://git.leastfixedpoint.com/tonyg/racket-ansi.git`)
- `charterm` (`https://www.neilvandyke.org/racket/charterm.zip`)

---

## Detailed package notes

## `tui-term`

What it is (from source):
- A low-level terminal abstraction around input/output ports and message queues.
- `tui/term` re-exports key pieces: terminal constructors, VT output helpers, decoded message structs, and utility macros.

Design details observed in code:
- Central `tterm` struct stores `in`, `out`, `size`, terminal type, and optional platform hook (`term/term-struct.rkt`).
- `make-tty-term` wires `/dev/tty` ports into a queue-backed VT input port and emits `tsizemsg` on resize (`term/tty.rkt`).
- VT output is explicit escape-sequence helpers (`term-gotoxy`, `term-clrscr`, alt-screen, cursor visibility, mouse enable/disable) in `term/vt-output-port.rkt`.
- Input decoding is serious and stateful: ESC-sequence parsing, modifier decoding, mouse/resize events in `term/vt-input-port.rkt`.
- Message model (`tmsg`, `tkeymsg`, `tmousemsg`, `tsizemsg`, `tcmdmsg`) is timestamped and queue-friendly (`term/messages.rkt`).

Important comparison to Kettle:
- This is terminal substrate + message decoding, not an app architecture.
- No TEA loop, no declarative image tree, no component model.

Potential Kettle use:
- Kettle could delegate more key/mouse decode logic to `tui-term` instead of maintaining its own parser.
- Kettle could also consider adopting a queue/event abstraction similar to `tqueue`.

## `tui-ubuf`

What it is (from source):
- A Unicode-aware terminal cell buffer library with clipping, overlays, drawing primitives, and VT emission.

Design details observed in code:
- `ubuf` struct tracks stride/rows, flat cell vector, output buffer, origin, clip rectangle (`ubuf/ubuf-struct.rkt`).
- Cell packing uses 4 slots per cell: char, fg, bg, flags; flags include dirty/bold/underline/italic/blink/wide bits (`ubuf/cell.rkt`, `ubuf/attributes.rkt`).
- Dirty-tracking and `#:only-dirty` / `#:clear-dirty` output modes are first-class (`ubuf/output.rkt`).
- Handles truecolor and indexed color (`ubuf/color.rkt`).
- Handles East Asian width / wide-char behavior in cell operations (`ubuf/cell.rkt` + eaw categories data).
- VT output path is optimized around cursor motion minimization and attribute transition emission (`ubuf/vt-output.rkt`).

Important comparison to Kettle:
- Overlaps strongly with Kettle renderer internals (cell buffer, style transitions, dirty/diff ideas).
- Does not provide Kettle’s high-level TEA protocol or component framework.

Potential Kettle use:
- Kettle could keep its `image` algebra + program runtime, and replace/augment low-level renderer backend with `tui-ubuf`.
- Candidate simplifications: bespoke cell structs, parts of wide-char handling, dirty-state bookkeeping, and some VT emission logic.

## `termios`

What it is (from source):
- Direct FFI surface for POSIX termios APIs.

Design details observed in code:
- Exposes `tcgetattr`, `tcsetattr`, `cfmakeraw`, speed functions, flush/flow/sendbreak (`termios/termios.rkt`).
- Provides broad constants from termios headers (`termios/defines.rkt`), with generated helper defines in private install step.
- Uses FFI error handling and errno mapping for failure paths.
- Offers struct-level access (`TERMIOS`) and low-level flag manipulation compatibility.

Important comparison to Kettle:
- Much lower-level than Kettle’s terminal wrapper.
- Kettle currently relies on `#%terminal` plus manual escape sequences, not explicit termios flag surfaces.

Potential Kettle use:
- If Kettle needs finer raw-mode control, flow-control toggles, or platform-specific termios knobs, this package is a stronger foundation than custom ad-hoc FFI.

## `termconfig` ("term config")

What it is (from source):
- Cross-platform terminal helper layer with separate Unix/Windows implementations and common cursor/color helpers.

Design details observed in code:
- Runtime OS split in `main.rkt` chooses `private/windows.rkt` vs `private/unix.rkt`.
- Unix path uses FFI (`tcgetattr`/`cfmakeraw`/`tcsetattr`) and stores/restores previous terminal state.
- Windows path uses console APIs (`GetStdHandle`, `GetConsoleMode`, `SetConsoleMode`) and virtual terminal input flags.
- Common module provides cursor movement, show/hide cursor, screen clear, and basic ANSI color constants.

Important comparison to Kettle:
- Kettle has stronger app/runtime abstractions, but this package has clearer cross-platform raw-mode split and explicit Windows console support strategy.

Potential Kettle use:
- Kettle could adopt similar OS-separated backend organization for terminal init/resume paths.
- Useful if Kettle wants to strengthen Windows support portability.

## `rktermios`

What it is (from source):
- Minimal pure-FFI raw-mode helper focused on BC/CS compatibility.

Design details observed in code:
- Wraps termios bytes in an opaque `rktermios` struct.
- Supports get/copy/load/make-raw operations.
- Notable implementation choice: detects effective `struct termios` size dynamically by calling FFI procedures over scratch memory and scanning modified bytes.

Important comparison to Kettle:
- Tiny and focused. It does one thing Kettle does not expose publicly: explicit termios snapshot/copy/load primitives.

Potential Kettle use:
- Could be a replacement for bespoke termios assumptions if Kettle wants explicit raw-mode state objects independent of `#%terminal` internals.

## `ansi`

What it is (from source):
- Three-module library: `ansi/ansi.rkt` (escape sequence constructors), `ansi/lcd-terminal.rkt` (input lexer/decoder), and `ansi/private/tty-raw-extension.rkt` (native raw-mode toggle).
- No published docs; the API is entirely defined by `(provide (all-defined-out))` in `ansi.rkt` and explicit provides in the other modules.

Design details observed in code:

**`ansi/ansi.rkt`** — Comprehensive ANSI/VT10x escape sequence library (~460 lines):
- Two macros `define-escape-sequence` and `define-variable-arity-escape-sequence` generate functions that build escape strings by concatenating CSI prefix + formatted parameters + terminator.
- Covers: cursor movement (up/down/left/right/goto/absolute), screen/line clearing, insert/delete lines/characters, scrolling, pagination.
- Full SGR (Select Graphic Rendition) constants: `style-bold`, `style-faint`, `style-italic/inverse`, `style-underline`, `style-blink-slow`, `style-blink-fast`, `style-inverse`, `style-conceal`, `style-crossed-out`, plus their reset counterparts. Color constants (`color-black` through `color-white`) for 8-color and xterm 256-color (`select-xterm-256-text-color`, `select-xterm-256-background-color`).
- DEC VT100 private sequences: double-width/height lines, save/restore cursor, margin setting, soft reset, rectangular area operations (VT400).
- xterm extensions: full-reset, window/icon title setting via OSC.
- Extensive terminal mode constants for `set-mode`/`reset-mode`: standard ECMA modes (guarded area, keyboard action, insertion/replacement, etc.), DEC private modes (autowrap, origin, auto-repeat, scrolling, etc.), and xterm-specific modes including all mouse tracking modes (`x10-mouse-reporting-mode`, `x11-normal-mouse-tracking-mode`, `x11-button-event-mouse-tracking-mode`, `x11-any-event-mouse-tracking-mode`, `x11-extended-mouse-tracking-mode`, `x11-focus-event-mode`), alternate screen buffer modes, and rxvt extensions.

**`ansi/lcd-terminal.rkt`** — Input lexer/decoder (~285 lines):
- `lex-lcd-input` function: a regexp-driven input parser that reads from a port and returns structured event values.
- Event structs: `key` (value + modifier set), `mouse-event` (type/button/row/column/modifiers), `mouse-focus-event`, `position-report`, `screen-size-report`, `unknown-escape-sequence`.
- Modifier handling via sets: `’shift`, `’meta`, `’control`, with helper constructors `S-`, `C-`, `M-`, `C-S-`, `C-M-`.
- Parses: SGR extended mouse events (`\e[<type;x;y[mM]`), basic X11 mouse events (`\e[M...`), CSI bracket sequences with parameter lists, ESC-O sequences (application keypad), and plain ASCII/UTF-8 bytes.
- `analyze-vt-bracket-key`: Handles arrows, home/end, insert/delete, page up/down, F1-F20 (via tilde-terminated sequences), position reports, screen size reports, and numerous `st` (suckless terminal) quirks.
- `analyze-vt-O-mainchar`: rxvt keypad keys, VT100 application mode arrow keys, function keys F1-F4.
- `interpret-ascii-code`: Maps control codes 0x00-0x1F to `(C- char)`, printable range to `(simple-key char)`, 0x7F to `’backspace`.
- Configurable UTF-8 mode (`lcd-terminal-utf-8?` parameter): when disabled, high-bit bytes are treated as meta-modified ASCII rather than multibyte characters.
- Configurable basic X11 mouse support (`lcd-terminal-basic-x11-mouse-support?`): auto-disabled for `st` terminals where basic mouse events overlap with control-delete.

**`ansi/private/tty-raw-extension.rkt` + `tty_raw.c`** — Native raw-mode toggle:
- C extension (`tty_raw.c`, ~49 lines): saves/restores termios state on stdin, disables ECHO/ICANON/IEXTEN/ISIG, input processing (BRKINT/ICRNL/INPCK/ISTRIP/IXON), sets CS8, disables OPOST. Registers `atexit` handler for automatic restore.
- Racket wrapper exposes `tty-raw!` and `tty-restore!` via FFI with `#:in-original-place? #t` for thread safety.
- Uses `register-process-global` to ensure the atexit handler is registered exactly once across places.

**Test programs:**
- `test-raw.rkt`: Interactive key display with mouse tracking (SGR extended + any-event) and focus events. Demonstrates full input pipeline.
- `test-screen-size.rkt`: Queries terminal dimensions via `device-request-screen-size` CSI sequence and parses the `screen-size-report` response.
- `test-modes.rkt`: Tests send/receive mode toggle.
- `test-ansi.rkt`: Visual demo of SGR styling, cursor movement, insert/delete lines, double-width text, screen clearing.

Comparison to Kettle:
- **Escape sequence generation** (`ansi.rkt`): Strong overlap with Kettle’s `style.rkt` (SGR output) and `renderer.rkt` (cursor control, screen clearing). The `ansi` package is more comprehensive — it covers sequences Kettle doesn’t use (double-width lines, rectangular area operations, pagination, tabulation) and has a cleaner generated-function approach vs Kettle’s hand-written format strings.
- **Input parsing** (`lcd-terminal.rkt`): Substantial overlap with Kettle’s `input.rkt`. Both parse CSI sequences, mouse events (basic and SGR extended), and special keys. The `ansi` package uses a regexp-first approach (try regexp matches on the port, fall through to byte-level parsing), while Kettle uses a byte-at-a-time state machine. The `ansi` package handles focus events and `st` terminal quirks; Kettle handles Kitty keyboard protocol (CSI u) which `ansi` does not.
- **Raw-mode** (`tty-raw-extension.rkt`): Direct C extension vs Kettle’s `#%terminal` API. Both disable the same terminal flags. The `ansi` package adds an atexit handler for automatic cleanup, which Kettle handles via its own shutdown logic.
- Neither package provides any application architecture, rendering pipeline, or component model.

Potential Kettle use:
- Kettle could use `ansi/ansi.rkt` as a canonical escape-sequence library instead of inline format strings in `renderer.rkt` and `style.rkt`. The macro-generated functions are less error-prone and cover more sequences.
- Kettle could use `ansi/lcd-terminal.rkt` as a base input parser and layer Kitty protocol support on top, rather than maintaining a parallel CSI parser in `input.rkt`.
- The mouse-event struct model (using sets for modifiers) is arguably cleaner than Kettle’s separate boolean fields, though it’s a different design trade-off (pattern matching vs set membership tests).

## `charterm`

What it is (from source):
- A self-contained, single-file (~2900 lines) character-cell terminal interface with inline Scribble documentation (via the `mcfly` literate-programming package). Supports multiple terminal protocols and keyboard decoder profiles.
- Pure Racket — no native code or FFI. Uses the `stty` external program for raw-mode and terminal state management.

Design details observed in code:

**Core struct** (`charterm`, line 1587):
- Fields: `tty` (path string), `in`/`out` (ports), `evt` (wrapped event for `sync`), `buf`/`buf-size`/`buf-start`/`buf-end` (byte buffer with manual cursor management), `termvar` (from `$TERM`), `protocol` (symbol), `keydec*` (key decoder), `screensize` (adaptive strategy state).
- Implements `prop:evt` so a `charterm` object can be used directly with `sync` — it wraps the input port and yields the charterm itself when input is ready.

**Terminal lifecycle** (`open-charterm`, `close-charterm`, `with-charterm`):
- `open-charterm`: Opens `/dev/tty` (or provided path), calls `stty raw -echo` via `system*`, opens bidirectional file ports with buffering disabled. Detects `$TERM` and selects protocol + keydec from a table of ~30 exact matches and regex fallbacks.
- `close-charterm`: Closes ports, calls `stty cooked echo` to restore.
- `with-charterm`: `dynamic-wind` form ensuring cleanup on escape.
- OS detection for `stty` flag differences: uses `-f` on macOS/BSD, `-F` on Linux.
- `current-charterm` parameter provides implicit threading of the terminal object.

**Protocol system** (`%charterm:protocol-case` macro):
- Compile-time exhaustiveness-checked `case` dispatch over protocols: `ansi`, `wyse-wy50`, `televideo-925`. The macro raises a syntax error if any protocol is missing from a case expression, preventing incomplete implementations.
- Each display operation (cursor positioning, clearing, styling) has protocol-specific implementations. For example, cursor positioning uses CSI sequences for ANSI, `\ea{row}R{col}C` for Wyse, and `\e=` with byte-encoded positions for TeleVideo.

**Keyboard decoding** — three-layer architecture:
1. **Keysets** (`charterm-keyset` struct): Define byte-sequence-to-keycode mappings for specific terminal families. Defined keysets: `ascii` (all control codes + printable), `dec-vt100` (PF1-PF4, arrows), `dec-vt220` (F1-F20, insert/delete/home/end/pgup/pgdn), `screen` (GNU Screen-specific F-keys), `linux` (Linux console quirks), `xterm-x11r6`, `xterm-xfree86`, `xterm-new` (three generations of xterm key encodings with incompatible F-key sequences), `rxvt`, `wyse-wy50` (function/editing keys via Wyse escape sequences), `televideo-925`. Each keyset uses a "bytelang" DSL for specifying byte sequences (e.g., `"esc O P"` for VT100 F1, `"esc [ 1 1 ~"` for VT220 F1).
2. **Keydecs** (`charterm-keydec` struct): Compose multiple keysets into a lookup tree. Each keydec has a primary keytree (hash-of-hashes for multi-byte sequences) and a secondary keytree (single-byte fallback). Predefined keydecs combine appropriate keysets — e.g., `charterm-xterm-new-keydec` layers `xterm-new` + `dec-vt220` + `dec-vt100` + `ascii`.
3. **Reader** (`charterm-read-key` / `charterm-read-keyinfo`): Walks the keytree one byte at a time with 0.5-second timeout for disambiguation (e.g., bare ESC vs ESC-prefixed sequence). Returns either a keycode (char/symbol) or a `keyinfo` struct bundling keyset ID, byte sequence, key label, and all alternate keycodes.

**Display operations:**
- `charterm-display`: Multi-argument display with `#:width`, `#:pad`, and `#:truncate` options. Converts args to Latin-1 bytes (non-Latin-1 chars become `?`). Handles padding/truncation at the byte level.
- `charterm-cursor`: Protocol-dispatched cursor positioning (1-indexed).
- `charterm-clear-screen`, `charterm-clear-line`, `charterm-clear-line-left`, `charterm-clear-line-right`: Protocol-aware clearing.
- Video attributes: `charterm-bold`, `charterm-underline`, `charterm-blink`, `charterm-inverse`, `charterm-normal` — mutually exclusive (setting one clears others). Only supported on ANSI protocol.
- `charterm-insert-line`, `charterm-delete-line`: Line manipulation.
- No color support beyond what the video attribute modes provide.

**Screen size detection** (adaptive strategy):
- Tries CSI `\e[18t` control sequence first (ANSI protocol only, excluding `screen`).
- Falls back to parsing `stty -a` output for `rows`/`columns`.
- Remembers what worked and skips failed methods on subsequent calls.
- Defaults to 80×24 if both methods fail.

**Demo** (`demo.rkt`):
- Interactive program showing key decoding, editable text field (manual byte-buffer manipulation), clock display with 1-second timeout polling, screen resize handling, and terminal metadata display. Demonstrates the full charterm API surface.

Comparison to Kettle:
- **Terminal management**: `charterm` uses `stty` (external process) for raw mode; Kettle uses `#%terminal` (FFI/ioctl). The `stty` approach is more portable (no native code) but slower and less precise. `charterm` also handles Wyse and TeleVideo protocols that Kettle does not target at all.
- **Keyboard input**: `charterm`'s keyset/keydec architecture is more systematic than Kettle's input parser — it supports compile-time exhaustiveness checking of protocols and can handle diverse terminal families. However, it lacks modern protocol support: no Kitty keyboard protocol (CSI u), no SGR mouse events, no bracketed paste. The `ansi` package's `lcd-terminal.rkt` is more modern in this regard.
- **Display**: `charterm` is strictly character-at-a-time with imperative cursor movement. No cell buffer, no image algebra, no diffing. Each display call writes directly to the terminal. This is a fundamentally different model from Kettle's declarative image tree → cell buffer → diff → minimal ANSI output pipeline.
- **Styling**: Very limited — five mutually-exclusive attributes, no color API beyond the basic modes. Kettle's style system (16/256/truecolor, composable bold+italic+underline+etc.) is far more capable.
- **Architecture**: No event loop, no model-update-view pattern, no subscriptions, no components. `charterm` is a low-level imperative terminal toolkit.

Potential Kettle use:
- The keyset/keydec architecture is an interesting model for making Kettle's input parser more extensible and testable — defining key mappings as data rather than code. Kettle could adopt a similar approach for supporting multiple terminal families without hardcoding everything in the parser.
- The adaptive screen-size detection strategy (try control sequence, fall back to stty, remember what works) is a useful pattern Kettle could adopt for robustness across diverse terminal environments.
- The protocol-case exhaustiveness macro is a good defensive-programming pattern that Kettle could use if it ever needs to support non-ANSI terminals.
---

## What this implies for Kettle specifically

Kettle’s unique value remains:
- TEA-like `gen:kettle-model` + commands + subscriptions runtime.
- Declarative `image` composition model integrated with app loop.
- Reusable TUI components and dedicated headless/tmux test harnesses.

Most reusable external value is at the lower layers:
1. `ansi` for canonical escape-sequence generation (`ansi/ansi.rkt`) and input decoding (`ansi/lcd-terminal.rkt`). The escape-sequence library is the most directly useful — cleaner than inline format strings, covers more sequences, and is well-tested. The input lexer handles SGR mouse events and focus tracking that Kettle also needs.
2. `tui-ubuf` for rendering buffer internals and output emission mechanics.
3. `tui-term` for input/message decoding and terminal queue plumbing.
4. `termios`/`rktermios`/`termconfig` for more explicit low-level terminal state control (especially portability/Windows concerns).
5. `charterm` for its keyset/keydec data-driven key mapping architecture and adaptive screen-size detection strategy, though the rest of the package is too low-level and limited for Kettle's needs.

If Kettle wants to reduce custom code while preserving architecture, the safest direction is:
- Keep Kettle protocol + program + components.
- Replace isolated renderer/input/terminal internals behind adapters.
