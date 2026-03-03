# Upstream Library Improvement Notes

Issues and workarounds discovered while integrating `ansi` and `tui-ubuf`
into Kettle. Each item describes a limitation, the current workaround in
Kettle, and a suggested upstream fix.

---

## tui-ubuf

### 1. No `#:reverse` attribute support in `ubuf-putchar!`

**Problem:** `ubuf-putchar!` supports `#:bold`, `#:italic`, `#:underline`, and
`#:blink`, but not `#:reverse` (SGR 7). Reverse video is a common text
attribute used in TUI headers, status bars, and selection highlighting.

**Workaround:** Kettle manually swaps fg and bg colors when `reverse?` is true
before calling `ubuf-putchar!`. This doesn't handle the case where the default
terminal fg/bg should be reversed (Kettle uses 7/0 as defaults).

**Suggested fix:** Add `#:reverse` keyword to `ubuf-putchar!` and corresponding
`tcf-reverse-*` flags in `attributes.rkt`, with SGR 7/27 output in
`vt-output.rkt`.

### 2. No `#:strikethrough` or `#:faint` attribute support

**Problem:** `ubuf-putchar!` doesn't support strikethrough (SGR 9) or faint/dim
(SGR 2) text attributes.

**Workaround:** Kettle stores these in its style struct but silently drops them
when writing to the ubuf.

**Suggested fix:** Add `#:strikethrough` and `#:faint` keywords to
`ubuf-putchar!`.

### 3. Attribute tracking not reset at row boundaries

**Problem:** In `vt-output.rkt`, the linear-mode row separator emits `\x1b[0m\r\n`
(full SGR reset) between rows. After this, `last-fg` and `last-bg` are reset to
`tcf-mask` (forcing re-emission), but `last-bold`, `last-italic`,
`last-underline`, and `last-blink` are not. Since `\x1b[0m` turns off all
attributes in the terminal, the tracking variables become out of sync with the
actual terminal state. If row N has italic and row N+1 also has italic, the code
thinks italic is already on and skips `\x1b[3m`, but the terminal already turned
it off via the reset. Row N+1 renders without italic.

**Fix applied:** Added `(set! last-bold tcf-mask)`, `(set! last-italic tcf-mask)`,
`(set! last-underline tcf-mask)`, and `(set! last-blink tcf-mask)` after the
existing `last-fg`/`last-bg` resets. This is in the local clone at `tui-ubuf/`.
This should be upstreamed.

---

## ansi

### 1. `lex-lcd-input` didn't handle Kitty keyboard protocol (CSI u)

**Problem:** The Kitty keyboard protocol uses `ESC[keycode;modifiers:event_type u`
sequences. The colon sub-parameter separator and the `u` terminator were not
recognized by `lex-lcd-input`. These sequences were partially consumed and
returned as `unknown-escape-sequence` values, with some bytes left in the port.

**Fix applied:** Added regex patterns for CSI u sequences, a `kitty-key-event`
struct, `kitty-keycode->value` mapping, and `decode-kitty-key` to
`ansi/lcd-terminal.rkt`. This should be upstreamed.

### 2. `lex-lcd-input` didn't handle bracketed paste

**Problem:** Bracketed paste uses `ESC[200~` to start and `ESC[201~` to end.
`lex-lcd-input` parsed `ESC[200~` as a regular CSI tilde sequence, returning it
as a Delete/Insert/etc. key, and left the paste content and end marker in the
port buffer.

**Fix applied:** Added a `bracketed-paste-event` struct and a regex match for
`ESC[200~` that reads content until `ESC[201~`. This should be upstreamed.

### 3. `lex-lcd-input` didn't handle Kitty query response (CSI ? N u)

**Problem:** `ESC[?Nu` is the Kitty keyboard protocol query response, where N is
a flags integer. The `?` prefix wasn't matched by any existing pattern.

**Fix applied:** Added a regex for `ESC[?Nu` that returns a `kitty-key-event`
with value `'query-response` and the flags in the event-type field. This should
be upstreamed.

### 4. Backtab returned as Ctrl+Shift+I instead of `'backtab`

**Problem:** `ESC[Z` (backtab/shift-tab) was mapped to `(key #\I (set 'control
'shift))` which is technically correct but inconvenient. Consumers must check for
the Ctrl+Shift+I combination instead of a simple `'backtab` symbol.

**Fix applied:** Changed `["Z" (C-S- #\I)]` to `["Z" (simple-key 'backtab)]`.
This should be upstreamed.

### 5. Tab, Enter, and Escape not returned as named keys

**Problem:** `interpret-ascii-code` mapped all control characters uniformly to
`(key <char> (set 'control))`. Tab (0x09) was returned as `(key #\I (set
'control))`, Enter (0x0d) as `(key #\M (set 'control))`, and Escape (0x1b) as
`(key #\[ (set 'control))`. These are common keys that should have symbolic
names.

**Fix applied:** Added special cases in `interpret-ascii-code` for 0x09
(`'tab`), 0x0a/0x0d (`'enter`), and 0x1b (`'escape`). This should be
upstreamed.

### 6. Mouse coordinates swapped (row/column)

**Problem:** `decode-basic-mouse-event` and `decode-extended-mouse-event` passed
local variables `x` and `y` to the `mouse-event` constructor in the wrong order.
The struct fields are `(type button row column modifiers)`, but the constructors
were called with `(mouse-event type button x y modifiers)` where `x` is the
column and `y` is the row. This caused `mouse-event-row` to return the column
and `mouse-event-column` to return the row.

**Fix applied:** Changed all `mouse-event` constructor calls to pass `y x`
(i.e., row then column) matching the struct field order. This should be
upstreamed.

### 7. Standalone Escape key not detected

**Problem:** A lone `0x1b` byte (Escape key press) was not distinguished from
the start of an escape sequence. When the user presses Escape without any
following bytes, `lex-lcd-input` would block waiting for more input, or
(depending on timing) would consume the next keypress as part of a malformed
escape sequence.

**Fix applied:** Added a check at the start of `lex-lcd-input`: if `0x1b` is the
first byte and no more bytes are immediately available in the port buffer (via
`peek-bytes-avail!*`), return `(simple-key 'escape)`. Terminal escape sequences
arrive atomically (all bytes in one write), so a lone `0x1b` is unambiguous.
This should be upstreamed.

### 8. Alt+key not recognized

**Problem:** `ESC` followed by a printable character (Alt+key in most terminals)
was not handled. The ESC was consumed, and the following character fell through
to the regular character handler, losing the Alt modifier.

**Fix applied:** Added a regex pattern `#px#"^\e([\x20-\x7f])"` after
CSI/SS3/mouse/kitty patterns that returns the key with a `'meta` modifier. This
should be upstreamed.

---

## Summary of changes to ansi/lcd-terminal.rkt

All changes are on the `kettle-extensions` branch of
<https://github.com/samth/racket-ansi> (also in the local clone at
`/home/samth/sw/racket-scratch/extra-pkgs/ansi/`):

- New structs: `kitty-key-event`, `bracketed-paste-event`
- New functions: `kitty-keycode->value`, `decode-kitty-key`
- New regex patterns in `lex-lcd-input`: bracketed paste, Kitty query, CSI u,
  Alt+key, standalone escape
- Modified: `interpret-ascii-code` (tab/enter/escape), backtab mapping,
  mouse-event constructor argument order (row/column swap fix)
