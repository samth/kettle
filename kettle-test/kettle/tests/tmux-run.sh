#!/usr/bin/env bash
# tmux-run.sh -- Run a Racket program in tmux, optionally send keys, capture output.
#
# Usage:
#   ./tmux-run.sh <racket-file> [wait-secs] [keys-to-send] [post-key-wait] [width] [height]
#
# Examples:
#   ./tmux-run.sh /tmp/test-counter-noalt.rkt 20
#   ./tmux-run.sh /tmp/test-counter-noalt.rkt 20 "+" 2
#   ./tmux-run.sh /tmp/test-byte-ready.rkt 8 "x" 2
#   ./tmux-run.sh my-program.rkt 3 "q" 1 120 40
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

# Send keys if provided
if [[ -n "$KEYS" ]]; then
    echo "Sending keys: $KEYS" >&2
    tmux send-keys -t "$SESSION" "$KEYS"
    sleep "$POST_KEY_WAIT"
    echo "=== AFTER KEYS ===" >&2
    capture_pane
fi

# Show errors if any
if [[ -s "$ERRLOG" ]]; then
    echo "=== STDERR ===" >&2
    cat "$ERRLOG" >&2
fi
