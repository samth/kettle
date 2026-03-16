# AI Code Review: Kettle

## Executive Summary
Kettle has a solid overall shape: the core model/update/view abstraction is consistent, the internal architecture is documented unusually well, and the input/parser layer has meaningful automated coverage. The codebase does not look like throwaway scaffolding. The main problems are concentrated in the edges where "finished-looking" APIs are not actually reliable yet: command execution, subscriptions, Unicode rendering, viewport clipping, and several documented component options.

The highest-risk issues are in production-facing paths. `exec-cmd` shells unescaped user-controlled arguments through `system`, which is both a command-injection bug and an argument-quoting bug. The renderer claims grapheme-cluster support, but actual painting drops combining marks and ZWJ continuations, so visible output loses data. The subscription layer has two broken public features: `watch-port` delivers byte counts instead of bytes, and `on-resize` is documented as supported but never actually receives resize events after startup.

Remediation is straightforward but not trivial. The core runtime and renderer need first attention, followed by component correctness and then docs/tests. Estimated effort: roughly 2-4 focused engineering days to fix the critical/runtime issues and add regression coverage, plus another 1-2 days to clean up the stale docs and example drift.

## Critical Issues (Must Fix)

### `exec-cmd` Is Vulnerable to Shell Injection and Breaks Quoted Arguments
- **Category**: Security Issues
- **Location**: `kettle-lib/kettle/private/program.rkt:389`
- **Problem**: `run-exec-command` converts `(exec-cmd program args callback)` into a single shell string with `(string-append exec-prog " " (string-join exec-args " "))` and passes it to `system`. Any argument containing spaces, quotes, glob characters, `;`, `&&`, `$()`, or backticks will be reinterpreted by the shell. This is both a security bug and a correctness bug for perfectly normal filenames like `"My File.txt"`.
- **Fix**: Replace the shell-string path with `subprocess`, `system*`, or equivalent argv-based execution. Keep `exec-cmd-program` and `exec-cmd-args` separate all the way to process launch, and return/handle the subprocess exit status explicitly.
- **Inline regression test**:

```racket
(test-case "exec-cmd preserves argv boundaries"
  (when (tmux-available?)
    (define out (make-temporary-file "kt-exec-out~a"))
    (define script (make-temporary-file "kt-exec-argv~a.sh"))
    (call-with-output-file script
      (lambda (o)
        (fprintf o "#!/usr/bin/env bash\nprintf '%%s\\n%%s\\n' \"$#\" \"$1\" > '~a'\n"
                 (path->string out)))
      #:exists 'replace)
    (file-or-directory-permissions script #o755)

    (define app (make-temporary-file "kt-exec-app~a.rkt"))
    (call-with-output-file app
      (lambda (o)
        (fprintf o
                 "#lang racket/base\n(require kettle)\n\
                  (struct m () #:transparent #:methods gen:kettle-model\n\
                    [(define (init self)\n\
                       (cmd self (exec-cmd \"~a\" (list \"a b\") (lambda () (quit-msg)))))\n\
                     (define (update self msg) self)\n\
                     (define (view self) (text \"exec\"))])\n\
                  (module+ main (program-run (make-program (m) #:alt-screen #t)))\n"
                 (path->string script)))
      #:exists 'replace)

    (with-tmux-session ([s (path->string app) #:startup-delay 1.0])
      (sleep 1.0))

    (check-equal? (file->string out) "1\na b\n")))
```

### Grapheme Clusters Measure Correctly but Render Incorrectly
- **Category**: Logic Errors and Incorrect Implementations
- **Location**: `kettle-lib/kettle/private/renderer.rkt:106`
- **Problem**: `paint-text!` tracks grapheme boundaries, but it only writes the first codepoint of each cluster into the cell buffer and drops combining marks, ZWJ joins, and skin-tone modifiers entirely. Verified behavior: `image->string (text "e\u0301")` renders `e`, not `é`. The code therefore measures clusters as single glyphs but paints them as truncated glyphs, which is silent data loss.
- **Fix**: Change the renderer to operate on grapheme clusters rather than individual codepoints. Either store/render cluster strings in the buffer or extend the ubuf layer so a cell can represent a multi-codepoint grapheme. Add regression tests that assert rendered output, not just `visible-length`/`truncate-text`.
- **Inline regression test**:

```racket
(test-case "renderer keeps combining marks in output"
  (define rendered (image->string (text "e\u0301")))
  (check-true (string-contains? rendered "e\u0301")))
```

### Subscription Primitives Are Broken: `watch-port` Sends Counts, and `on-resize` Never Fires
- **Category**: Logic Errors and Incorrect Implementations
- **Location**: `kettle-lib/kettle/private/subscriptions.rkt:51`
- **Problem**: `watch-port` uses `read-bytes-avail!*` with a preallocated buffer and forwards the return value from that call. In Racket, that return value is the number of bytes read, not the bytes themselves, so subscribers receive an integer instead of the port payload. Separately, `sub:resize` is documented as being driven by the runtime, but `program-run` only sends `window-size-msg` at startup and after `exec-cmd` returns; there is no SIGWINCH/polling/input-path that delivers actual live resize events.
- **Fix**: For `sub:port`, allocate a buffer, capture the byte count, and pass `(subbytes buf 0 count)` to `make-msg`. For `sub:resize`, add real resize detection in the runtime and feed `window-size-msg` into the message channel whenever the terminal size changes. Add integration tests that exercise both features through `program-run`, not just the headless harness.
- **Inline regression test**:

```racket
(test-case "watch-port passes byte payloads, not byte counts"
  (define seen '())
  (define stop
    (start-subscription
     (watch-port (open-input-bytes #"abc") (lambda (bs) bs))
     (lambda (m) (set! seen (cons m seen)))))
  (sleep 0.05)
  (stop-subscription stop)
  (check-equal? (reverse seen) (list #"abc")))

;; Companion e2e test: run a tiny program in tmux, resize the tmux window with
;; `tmux -L kettle-test resize-window ...`, and assert the rendered resize count
;; increments from the startup value.
```

### Fresh Installs Do Not Encode the Required `tui-ubuf` Behavior
- **Category**: Dependency and Version Issues
- **Location**: `kettle-lib/info.rkt:5`
- **Problem**: The package depends on generic `"tui-ubuf"`, but `UPSTREAM_IMPROVEMENTS.md` documents correctness fixes that exist only in the local `tui-ubuf/` clone, including row-end erase behavior and attribute reset fixes. That means a fresh install can pass dependency resolution while still missing rendering behavior this repository expects.
- **Fix**: Publish and pin a patched `tui-ubuf` source, vendor it as an explicit package dependency, or remove reliance on the patched behavior. Do not leave correctness-critical fixes only in an untracked local clone plus prose notes.

## Serious Issues (Should Fix)

### `wrap-text` Ignores `#:normalize-spaces`
- **Category**: Logic Errors and Incorrect Implementations
- **Location**: `kettle-lib/kettle/private/style.rkt:610`
- **Problem**: `wrap-text` accepts `#:normalize-spaces`, and the docs say it controls spacing behavior, but the implementation always collapses whitespace tokens into a single pending space. Verified behavior: `wrap-text "a   b" 10 #:normalize-spaces #f` returns `"a b"`. This also means internal callers such as `render-styled` cannot preserve intentional spacing when wrapping.
- **Fix**: Honor `normalize-spaces` in the token loop. When it is `#f`, preserve the original whitespace tokens instead of collapsing them to a single separator. Add regression tests for both modes.
- **Inline regression test**:

```racket
(test-case "wrap-text honors #:normalize-spaces"
  (check-equal? (wrap-text "a   b" 10 #:normalize-spaces #f) "a   b")
  (check-equal? (wrap-text "a   b" 10 #:normalize-spaces #t) "a b"))
```

### Viewport Width Is Not Enforced and Horizontal Slicing Uses the Wrong Units
- **Category**: Type Safety and Data Integrity
- **Location**: `kettle-lib/kettle/components/viewport.rkt:92`
- **Problem**: `viewport-visible-lines-list` only crops lines when `x-offset > 0`, so a viewport of width 3 can render `"abcdef"` unchanged until the user scrolls horizontally. When it does crop, it computes `start`/`end` in display columns and then passes them directly to `substring`, which uses codepoint indices, so ANSI sequences and wide characters are sliced incorrectly.
- **Fix**: Always clip visible lines to the viewport width, even at `x-offset = 0`. Replace `substring` with ANSI-aware, display-column-aware clipping logic shared with other text utilities. Add tests for narrow viewports, ANSI-colored content, and wide-character content.
- **Inline regression test**:

```racket
(test-case "viewport enforces width even at x-offset zero"
  (define rendered
    (image->string
     (view (make-viewport #:width 3 #:height 1 #:content "abcdef"))))
  (check-true (string-contains? rendered "abc"))
  (check-false (string-contains? rendered "abcdef")))
```

### `textarea-blur` Does Not Work, and `#:char-limit` / `#:max-lines` Are Dead Options
- **Category**: Dead Code and Phantom Features
- **Location**: `kettle-lib/kettle/components/textarea.rkt:63`
- **Problem**: `ta-copy` uses `#f` as both the "argument not supplied" sentinel and the actual value for `focused?`, so `textarea-blur` cannot set focus to false. Verified behavior: blurring a focused textarea leaves `focused?` as `#t`. Separately, `#:char-limit` and `#:max-lines` are stored on the struct but are never enforced in insert/newline paths; verified behavior: a textarea with `#:char-limit 1` accepts `"abc"`, and one with `#:max-lines 1` accepts multiple lines.
- **Fix**: Use explicit sentinel values in `ta-copy` so `#f` can be assigned intentionally. Enforce `char-limit` in both single-line and multi-line insert paths, and block/trim newline insertion when `max-lines` is reached. Add focused/unfocused and limit regression tests.
- **Inline regression test**:

```racket
(test-case "textarea blur clears focus"
  (check-false
   (textarea-focused? (textarea-blur (textarea-focus (make-textarea))))))

(test-case "textarea enforces char-limit and max-lines"
  (define ta (make-textarea #:char-limit 1 #:max-lines 1))
  (define ta2 (textarea-insert-string ta "abc\ndef"))
  (check-equal? (textarea-value ta2) "a")
  (check-equal? (textarea-line-count ta2) 1))
```

### `kettle` Omits the Kitty Query Response Message from Its Public API
- **Category**: Hallucinated or Non-Existent APIs
- **Location**: `kettle-lib/kettle/main.rkt:45`
- **Problem**: `private/protocol.rkt` defines and exports `kitty-query-response-msg`, but `kettle/main.rkt` claims to re-export the full public API while omitting that struct from its `provide` list. Current tests do not catch this because `test-input.rkt` gets the type from `kettle/test`, which re-exports the private protocol directly.
- **Fix**: Add `(struct-out kitty-query-response-msg)` to `kettle-lib/kettle/main.rkt`. Add a package-level smoke test that requires `kettle` itself and checks the exported Kitty-related message API.
- **Inline regression test**:

```racket
(test-case "kettle exports kitty query response message helpers"
  (check-pred procedure? (dynamic-require 'kettle 'kitty-query-response-msg))
  (check-pred procedure? (dynamic-require 'kettle 'kitty-query-response-msg?)))
```

### Table Styling Options Are Accepted but Never Applied
- **Category**: Dead Code and Phantom Features
- **Location**: `kettle-lib/kettle/components/table.rkt:25`
- **Problem**: `table` stores `header-style`, `row-style`, and `border-color`, and `make-table` accepts those keyword arguments, but `table-render` never reads any of them. Verified behavior: rendering a styled table produces the same output as rendering an unstyled one.
- **Fix**: Thread the stored styles through header rendering, row rendering, and border construction. If styling is intentionally out of scope, remove the dead fields and keyword arguments from the public API.
- **Inline regression test**:

```racket
(test-case "table styling keywords affect rendered output"
  (define plain
    (image->string
     (table-render (make-table #:headers '("H") #:rows '(("x"))))))
  (define styled-out
    (image->string
     (table-render
      (make-table #:headers '("H")
                  #:rows '(("x"))
                  #:header-style (make-style #:bold #t)
                  #:row-style (make-style #:foreground fg-red)
                  #:border-color fg-blue))))
  (check-not-equal? plain styled-out))
```

### Markdown Rendering Accepts a Width but Does Not Actually Wrap Paragraphs or Lists
- **Category**: Logic Errors and Incorrect Implementations
- **Location**: `kettle-components/kettle/components/markdown.rkt:186`
- **Problem**: `render-markdown` computes `content-width` from `#:width`, but paragraph/list/heading rendering concatenates strings directly and never wraps them to that width. Verified behavior: `render-markdown "This is a very long line..." #:width 10` returns a single long line. The width argument currently only affects horizontal rules and indentation math.
- **Fix**: Run block text through `wrap-text` using `content-width` before adding margins/prefixes. Apply the same treatment to list items and blockquotes, and add width-specific tests.
- **Inline regression test**:

```racket
(test-case "render-markdown wraps paragraphs to requested width"
  (define result
    (render-markdown "This is a very long line of text that should wrap."
                     #:style 'ascii
                     #:width 10))
  (check-true (string-contains? result "\n")))
```

### Runtime Error Handling Silently Swallows Failures
- **Category**: Fake or Shallow Error Handling
- **Location**: `kettle-lib/kettle/private/program.rkt:209`
- **Problem**: The event loop, command execution, input loop, and subscription evaluation catch broad `exn:fail?` and forward them to `handle-kettle-error`, whose default implementation only logs a warning. That means a bad user update/view function or a broken subscription can fail invisibly while the program keeps running with partially updated state or with subscriptions silently replaced by `'()`.
- **Fix**: Narrow the catches to genuinely recoverable operations, and make the default behavior fail fast in development. At minimum, surface exceptions back to the main loop with enough context to stop or mark the program failed instead of silently degrading behavior.

### Example HTTP Demo Parses `HTTP/1.1` as Status Code `1`
- **Category**: Logic Errors and Incorrect Implementations
- **Location**: `kettle-examples/kettle/examples/http.rkt:41`
- **Problem**: The code extracts the first numeric regex match from the status line, so `"HTTP/1.1 200 OK"` yields `"1"` instead of `"200"`. The example therefore demonstrates incorrect logic in the happy path.
- **Fix**: Split the status line on spaces and parse the second token, or switch to a higher-level HTTP client API that exposes the response code directly.
- **Inline regression test**:

```racket
;; After extracting the parser into a helper:
(test-case "status-line parser returns the HTTP status, not the protocol version"
  (check-equal? (status-line->code "HTTP/1.1 200 OK") 200)
  (check-equal? (status-line->code "HTTP/2 404 Not Found") 404))
```

## Minor Issues (Nice to Fix)

### `join-horizontal` and `join-vertical` Crash on Empty Input
- **Category**: Logic Errors and Incorrect Implementations
- **Location**: `kettle-lib/kettle/private/layout.rkt:52`
- **Problem**: Both functions try to "return" `""` when no blocks are supplied, but `return` is just an identity helper, not a non-local exit. Execution continues into `(apply max ...)`, which raises an arity error. Verified behavior: `(join-horizontal 'top)` crashes.
- **Fix**: Replace the `when` form with an outer `if`/`cond` that truly returns `""` before any width/height computation.
- **Inline regression test**:

```racket
(test-case "layout joins accept empty input"
  (check-equal? (join-horizontal 'top) "")
  (check-equal? (join-vertical 'left) ""))
```

### Component Documentation Describes Nonexistent or Wrong APIs
- **Category**: Documentation Lies
- **Location**: `kettle-doc/kettle/scribblings/components.scrbl:26`
- **Problem**: The component docs list APIs that do not exist or have the wrong semantics, including `list-view-update`, `list-view-render`, mutating `progress-set-percent!`, mutating `progress-increment!`, and `table-render` returning `string?` even though the implementation returns an image. This is exactly the kind of hallucinated API surface that causes downstream code to fail at first use.
- **Fix**: Regenerate the component docs from the actual exports in the source modules. Correct both names and return contracts, and add a doc-build smoke test that requires every documented identifier.

### README Install Instructions Do Not Cover All Shipped Examples
- **Category**: Documentation Lies
- **Location**: `README.md:89`
- **Problem**: The documented install command only installs `kettle-lib`, `kettle-doc`, and `kettle`, but several shipped examples require `kettle-components` (`markdown`, `datepicker`, `overlay`, `zones`). Following the README exactly leaves those examples unavailable.
- **Fix**: Either include `kettle-components` in the documented install flow or explicitly separate "core library install" from "install everything needed for all examples."

### `top.rkt` Is Linux-Specific but Documented as a Generic Example
- **Category**: Configuration and Environment
- **Location**: `kettle-examples/kettle/examples/top.rkt:69`
- **Problem**: The process viewer reads `/proc` directly and shells out to `who`, which makes it Linux-specific. The README advertises it as a normal example without a platform restriction, so users on macOS/BSD/Windows will hit a broken demo without warning.
- **Fix**: Add a platform guard in the example and clearly document "Linux only" in the README and example header.

## Positive Observations
- The core TEA-style protocol is coherent and consistent across the runtime and components; the project is not suffering from random architectural drift.
- `DESIGN.md`, `IMPLEMENTATION.md`, and `UPSTREAM_IMPROVEMENTS.md` are unusually detailed for an AI-generated codebase and make it possible to review intent against implementation.
- Input parsing coverage is good: the test suite exercises Kitty keyboard protocol parsing, query responses, mouse parsing, and multiple text-width edge cases.
- The code is generally written in an immutable-update style for models, which makes many fixes local and mechanically safe.

## Recommended Next Steps
1. Fix the runtime-critical defects first: `exec-cmd`, grapheme rendering, `watch-port`, and real resize delivery.
2. Add regression tests for each confirmed bug before changing behavior, especially for renderer output, viewport clipping, textarea limits/focus, and package-level API exports from `require kettle`.
3. Make dependency intent explicit by pinning or vendoring the required `tui-ubuf` behavior instead of relying on a local clone plus prose notes.
4. Clean up the public surface next: remove or implement dead keyword arguments, then bring the Scribble docs and README back in sync with the real exports and installation story.
