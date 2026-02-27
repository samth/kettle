#!/usr/bin/env bash
# tmux-run.sh -- Run a Racket program in tmux, optionally send keys, capture output.
#
# Usage:
#   ./tmux-run.sh <racket-file> [wait-secs] [keys-to-send] [post-key-wait] [width] [height]
#
# Keys syntax:
#   Space-separated tokens. Most are passed directly to tmux send-keys.
#   Special tokens:
#     @mouse:col,row      -- send SGR mouse press+release at (col,row) (1-based)
#     @esc                 -- send a raw standalone Escape byte (0x1b)
#
# Examples:
#   ./tmux-run.sh /tmp/test-counter-noalt.rkt 20
#   ./tmux-run.sh /tmp/test-counter-noalt.rkt 20 "+" 2
#   ./tmux-run.sh /tmp/test-byte-ready.rkt 8 "x" 2
#   ./tmux-run.sh my-program.rkt 3 "q" 1 120 40
#   ./tmux-run.sh zones.rkt 5 "@mouse:5,5" 1
#   ./tmux-run.sh datepicker.rkt 5 "Enter @esc" 1
#
# Output:
#   Prints captured pane contents to stdout.
#   Stderr from the Racket program is saved to /tmp/kt-tmux-run.log
#
# Notes:
#   Automatically detects alternate screen mode and captures accordingly.

set -euo pipefail

RACKET_FILE="${1:?Usage: $0 <racket-file> [wait-secs] [keys] [post-key-wait] [width] [height]}"
WAIT="${2:-20}"
KEYS="${3:-}"
POST_KEY_WAIT="${4:-2}"
WIDTH="${5:-80}"
HEIGHT="${6:-24}"

SESSION="kt-run-$$"
ERRLOG="/tmp/kt-tmux-run.log"

cleanup() {
    tmux kill-session -t "$SESSION" 2>/dev/null || true
}
trap cleanup EXIT

# Capture the pane, preferring alternate screen if it has content
capture_pane() {
    # Try alternate screen first
    local alt_content
    alt_content=$(tmux capture-pane -t "$SESSION" -p -a 2>/dev/null || true)
    # Strip whitespace to check if there's real content
    local trimmed="${alt_content//[$'\n\r\t ']/}"
    if [[ -n "$trimmed" ]]; then
        echo "$alt_content"
    else
        tmux capture-pane -t "$SESSION" -p
    fi
}

# Create session with bash (so it persists even if the program exits)
tmux new-session -d -s "$SESSION" -x "$WIDTH" -y "$HEIGHT" bash

# Launch the Racket program
tmux send-keys -t "$SESSION" "racket -y $RACKET_FILE 2>$ERRLOG" Enter

# Wait for startup
echo "Waiting ${WAIT}s for program to start..." >&2
sleep "$WAIT"

# Capture initial state
echo "=== INITIAL CAPTURE ===" >&2
capture_pane

# Send a single key token to the tmux session
send_token() {
    local tok="$1"
    case "$tok" in
        @mouse:*)
            # @mouse:col,row -- SGR mouse press+release (1-based screen coords)
            local coords="${tok#@mouse:}"
            local col="${coords%,*}"
            local row="${coords#*,}"
            printf -v press '\e[<0;%s;%sM' "$col" "$row"
            printf -v release '\e[<0;%s;%sm' "$col" "$row"
            tmux send-keys -t "$SESSION" -l "$press"
            sleep 0.2
            tmux send-keys -t "$SESSION" -l "$release"
            ;;
        @esc)
            # Raw standalone Escape byte
            printf -v esc '\x1b'
            tmux send-keys -t "$SESSION" -l "$esc"
            ;;
        *)
            tmux send-keys -t "$SESSION" "$tok"
            ;;
    esac
}

# Send keys if provided
if [[ -n "$KEYS" ]]; then
    echo "Sending keys: $KEYS" >&2
    for tok in $KEYS; do
        send_token "$tok"
        sleep 0.3
    done
    sleep "$POST_KEY_WAIT"
    echo "=== AFTER KEYS ===" >&2
    capture_pane
fi

# Show errors if any
if [[ -s "$ERRLOG" ]]; then
    echo "=== STDERR ===" >&2
    cat "$ERRLOG" >&2
fi
