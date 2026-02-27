# Upstream Library Improvement Notes

Issues and workarounds discovered while integrating `ansi` and `tui-ubuf`
into Kettle. Each item describes a limitation, the current workaround in
Kettle, and a suggested upstream fix.

---

## tui-ubuf

### 1. `display-ubuf!` always emits trailing `\r\n` after last row

**Problem:** `display-ubuf!` hardcodes `last-newline?` as `#t` when calling
`display-ubuf-cells`. In linear mode, this appends `\x1b[0m\r\n` after every
row including the last. For a full-screen image (e.g., 24 rows in a 24-row
terminal), the trailing `\r\n` scrolls the first row off the top of the screen.

**Workaround:** Kettle imports `display-ubuf-cells` from `tui/ubuf/vt-output`
and `ubuf-stride`/`ubuf-cells`/`ubuf-outbuf`/`ubuf-clip-*` from
`tui/ubuf/ubuf-struct`, then calls `display-ubuf-cells` directly with
`last-newline?` set to `#f`. This requires knowledge of ubuf struct internals.

**Suggested fix:** Add a `#:last-newline?` keyword argument to `display-ubuf!`
(defaulting to `#t` for backward compatibility).

### 2. `display-ubuf-cells` doesn't erase to end of line between rows

**Problem:** In linear mode, `display-ubuf-cells` emits `\x1b[0m\r\n` between
rows to reset attributes. However, it doesn't emit `\x1b[K` (erase to end of
line), so the area to the right of each row retains whatever SGR attributes were
active from the last cell. When the rendered image is narrower than the terminal,
this causes visible differences in background appearance between rows with styled
content and rows without. Additionally, the between-row reset only cleared `fg`
and `bg` tracking state, not `bold`, `underline`, `italic`, or `blink`, so those
attributes could carry over incorrectly.

**Fix applied:** Changed the between-row sequence to `\x1b[0m\x1b[K\r\n` and
added resets for all tracked attribute state. Kettle's renderer also now emits
`\x1b[0m` before `clear-to-end-of-screen` to ensure the final clear uses default
attributes. The fix is in the local clone at `tui-ubuf/`. This should be
upstreamed.

### 3. No `#:reverse` attribute support in `ubuf-putchar!`

**Problem:** `ubuf-putchar!` supports `#:bold`, `#:italic`, `#:underline`, and
`#:blink`, but not `#:reverse` (SGR 7). Reverse video is a common text
attribute used in TUI headers, status bars, and selection highlighting.

**Workaround:** Kettle manually swaps fg and bg colors when `reverse?` is true
before calling `ubuf-putchar!`. This doesn't handle the case where the default
terminal fg/bg should be reversed (Kettle uses 7/0 as defaults).

**Suggested fix:** Add `#:reverse` keyword to `ubuf-putchar!` and corresponding
`tcf-reverse-*` flags in `attributes.rkt`, with SGR 7/27 output in
`vt-output.rkt`.

### 4. No `#:strikethrough` or `#:faint` attribute support

**Problem:** `ubuf-putchar!` doesn't support strikethrough (SGR 9) or faint/dim
(SGR 2) text attributes.

**Workaround:** Kettle stores these in its style struct but silently drops them
when writing to the ubuf.

**Suggested fix:** Add `#:strikethrough` and `#:faint` keywords to
`ubuf-putchar!`.

### 5. `display-ubuf-cells` not exported from main `tui/ubuf` module

**Problem:** `display-ubuf-cells` is provided from `tui/ubuf/vt-output` but not
re-exported from the main `tui/ubuf` module. Similarly, the ubuf struct
accessors (`ubuf-stride`, `ubuf-cells`, etc.) are only available from
`tui/ubuf/ubuf-struct`. Users who need `display-ubuf-cells` (e.g., to control
`last-newline?`) must import from internal modules.

**Suggested fix:** If `display-ubuf!` gains a `#:last-newline?` parameter, this
becomes moot. Otherwise, consider re-exporting `display-ubuf-cells` from
`tui/ubuf`.

---

## ansi

### 6. `lex-lcd-input` didn't handle Kitty keyboard protocol (CSI u)

**Problem:** The Kitty keyboard protocol uses `ESC[keycode;modifiers:event_type u`
sequences. The colon sub-parameter separator and the `u` terminator were not
recognized by `lex-lcd-input`. These sequences were partially consumed and
returned as `unknown-escape-sequence` values, with some bytes left in the port.

**Fix applied:** Added regex patterns for CSI u sequences, a `kitty-key-event`
struct, `kitty-keycode->value` mapping, and `decode-kitty-key` to
`ansi/lcd-terminal.rkt`. This should be upstreamed.

### 7. `lex-lcd-input` didn't handle bracketed paste

**Problem:** Bracketed paste uses `ESC[200~` to start and `ESC[201~` to end.
`lex-lcd-input` parsed `ESC[200~` as a regular CSI tilde sequence, returning it
as a Delete/Insert/etc. key, and left the paste content and end marker in the
port buffer.

**Fix applied:** Added a `bracketed-paste-event` struct and a regex match for
`ESC[200~` that reads content until `ESC[201~`. This should be upstreamed.

### 8. `lex-lcd-input` didn't handle Kitty query response (CSI ? N u)

**Problem:** `ESC[?Nu` is the Kitty keyboard protocol query response, where N is
a flags integer. The `?` prefix wasn't matched by any existing pattern.

**Fix applied:** Added a regex for `ESC[?Nu` that returns a `kitty-key-event`
with value `'query-response` and the flags in the event-type field. This should
be upstreamed.

### 9. Backtab returned as Ctrl+Shift+I instead of `'backtab`

**Problem:** `ESC[Z` (backtab/shift-tab) was mapped to `(key #\I (set 'control
'shift))` which is technically correct but inconvenient. Consumers must check for
the Ctrl+Shift+I combination instead of a simple `'backtab` symbol.

**Fix applied:** Changed `["Z" (C-S- #\I)]` to `["Z" (simple-key 'backtab)]`.
This should be upstreamed.

### 10. Tab, Enter, and Escape not returned as named keys

**Problem:** `interpret-ascii-code` mapped all control characters uniformly to
`(key <char> (set 'control))`. Tab (0x09) was returned as `(key #\I (set
'control))`, Enter (0x0d) as `(key #\M (set 'control))`, and Escape (0x1b) as
`(key #\[ (set 'control))`. These are common keys that should have symbolic
names.

**Fix applied:** Added special cases in `interpret-ascii-code` for 0x09
(`'tab`), 0x0a/0x0d (`'enter`), and 0x1b (`'escape`). This should be
upstreamed.

### 11. Alt+key not recognized

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
`/home/samth/sw/racket-scratch/extra-pkgs/ansi/`). The diff adds ~89 lines to
`ansi/lcd-terminal.rkt`:

- New structs: `kitty-key-event`, `bracketed-paste-event`
- New functions: `kitty-keycode->value`, `decode-kitty-key`
- New regex patterns in `lex-lcd-input`: bracketed paste, Kitty query, CSI u,
  Alt+key
- Modified: `interpret-ascii-code` (tab/enter/escape), backtab mapping
