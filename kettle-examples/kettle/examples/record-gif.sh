#!/usr/bin/env bash
# record-gif.sh -- Record a Kettle program with scripted interactions as a GIF.
#
# Runs a Kettle program in tmux, sends scripted keystrokes on a schedule,
# records the session with asciinema, and converts to GIF with agg.
#
# Usage:
#   ./record-gif.sh <script-file> <output.gif> [options]
#
# Options:
#   --cols N         Terminal width  (default: 80)
#   --rows N         Terminal height (default: 24)
#   --font-size N    GIF font size   (default: 16)
#   --speed N        Playback speed multiplier (default: 1)
#   --theme NAME     agg theme: asciinema/monokai/solarized-dark/solarized-light
#                    (default: monokai)
#
# Script file format (one command per line, # comments allowed):
#   PROGRAM <racket-file>        Path to the Kettle program to run
#   ARGS <arguments>             Extra command-line arguments for the program
#   STARTUP <seconds>            Wait for program to start (default: 5)
#   WAIT <seconds>               Pause before next action
#   KEY <key>                    Send a single tmux key (e.g. +, q, Enter, Up, C-q)
#   TYPE <text>                  Type text character-by-character (0.08s per char)
#   KEYS <literal>               Send a literal string via tmux send-keys
#
# Examples:
#   # counter.script
#   PROGRAM counter.rkt
#   STARTUP 5
#   WAIT 1
#   KEY +
#   WAIT 0.5
#   KEY +
#   WAIT 0.5
#   KEY +
#   WAIT 1
#   KEY -
#   WAIT 1
#   KEY q
#
#   ./record-gif.sh counter.script counter.gif --cols 60 --rows 16

set -euo pipefail

# --- Parse arguments ---

SCRIPT_FILE=""
OUTPUT_GIF=""
COLS=80
ROWS=24
FONT_SIZE=16
SPEED=1
THEME="monokai"

while [[ $# -gt 0 ]]; do
    case "$1" in
        --cols)    COLS="$2";      shift 2 ;;
        --rows)    ROWS="$2";      shift 2 ;;
        --font-size) FONT_SIZE="$2"; shift 2 ;;
        --speed)   SPEED="$2";     shift 2 ;;
        --theme)   THEME="$2";     shift 2 ;;
        -*)        echo "Unknown option: $1" >&2; exit 1 ;;
        *)
            if [[ -z "$SCRIPT_FILE" ]]; then
                SCRIPT_FILE="$1"
            elif [[ -z "$OUTPUT_GIF" ]]; then
                OUTPUT_GIF="$1"
            else
                echo "Unexpected argument: $1" >&2; exit 1
            fi
            shift ;;
    esac
done

if [[ -z "$SCRIPT_FILE" || -z "$OUTPUT_GIF" ]]; then
    echo "Usage: $0 <script-file> <output.gif> [--cols N] [--rows N] [--font-size N] [--speed N] [--theme NAME]" >&2
    exit 1
fi

# Resolve paths
SCRIPT_DIR="$(cd "$(dirname "$SCRIPT_FILE")" && pwd)"
SCRIPT_FILE="$(cd "$(dirname "$SCRIPT_FILE")" && pwd)/$(basename "$SCRIPT_FILE")"
OUTPUT_GIF="$(cd "$(dirname "$OUTPUT_GIF")" && pwd)/$(basename "$OUTPUT_GIF")"

# --- Check dependencies ---

for cmd in tmux asciinema agg racket; do
    if ! command -v "$cmd" &>/dev/null; then
        echo "Error: $cmd is required but not found in PATH." >&2
        exit 1
    fi
done

# --- Parse script file ---

PROGRAM=""
PROGRAM_ARGS=""
STARTUP_WAIT=5

# Read script commands into arrays for later execution
declare -a CMD_TYPES=()
declare -a CMD_ARGS=()

while IFS= read -r line || [[ -n "$line" ]]; do
    # Strip comments and whitespace
    line="${line%%#*}"
    line="$(echo "$line" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')"
    [[ -z "$line" ]] && continue

    cmd="${line%% *}"
    arg="${line#* }"
    # Handle commands with no argument
    [[ "$cmd" == "$arg" ]] && arg=""

    case "$cmd" in
        PROGRAM)
            # Resolve program path relative to script directory
            if [[ "$arg" != /* ]]; then
                PROGRAM="$SCRIPT_DIR/$arg"
            else
                PROGRAM="$arg"
            fi
            ;;
        ARGS)
            # Resolve any relative paths in args relative to script directory
            PROGRAM_ARGS="$arg"
            ;;
        STARTUP)
            STARTUP_WAIT="$arg"
            ;;
        WAIT|KEY|KEYS|TYPE)
            CMD_TYPES+=("$cmd")
            CMD_ARGS+=("$arg")
            ;;
        *)
            echo "Warning: unknown script command '$cmd', skipping" >&2
            ;;
    esac
done < "$SCRIPT_FILE"

if [[ -z "$PROGRAM" ]]; then
    echo "Error: script file must contain a PROGRAM line" >&2
    exit 1
fi

if [[ ! -f "$PROGRAM" ]]; then
    echo "Error: program file not found: $PROGRAM" >&2
    exit 1
fi

# --- Setup ---

SESSION="kt-rec-$$"
SOCKET="kt-rec-$$"
CAST_FILE="/tmp/kt-rec-$$.cast"
ERRLOG="/tmp/kt-rec-$$.log"

cleanup() {
    tmux -L "$SOCKET" kill-server 2>/dev/null || true
    rm -f "$CAST_FILE" "$ERRLOG"
}
trap cleanup EXIT

echo "Recording: $PROGRAM -> $OUTPUT_GIF (${COLS}x${ROWS})" >&2

# --- Create isolated tmux server and session ---

# Use a dedicated tmux socket so we don't interfere with user's sessions
tmux -L "$SOCKET" new-session -d -s "$SESSION" -x "$COLS" -y "$ROWS" bash

# Disable status bar for a clean recording
tmux -L "$SOCKET" set-option -t "$SESSION" status off

# cd to the script directory so relative ARGS paths resolve correctly
tmux -L "$SOCKET" send-keys -t "$SESSION" "cd '$SCRIPT_DIR'" Enter
sleep 0.2

# Start the Kettle program
tmux -L "$SOCKET" send-keys -t "$SESSION" \
    "racket -y '$PROGRAM' $PROGRAM_ARGS 2>'$ERRLOG'" Enter

# Wait for startup
echo "Waiting ${STARTUP_WAIT}s for program to start..." >&2
sleep "$STARTUP_WAIT"

# --- Record with asciinema ---

# Background: send scripted keystrokes while recording
(
    for i in "${!CMD_TYPES[@]}"; do
        cmd="${CMD_TYPES[$i]}"
        arg="${CMD_ARGS[$i]}"
        case "$cmd" in
            WAIT)
                sleep "$arg"
                ;;
            KEY)
                tmux -L "$SOCKET" send-keys -t "$SESSION" "$arg"
                ;;
            KEYS)
                tmux -L "$SOCKET" send-keys -t "$SESSION" -l "$arg"
                ;;
            TYPE)
                for (( j=0; j<${#arg}; j++ )); do
                    char="${arg:$j:1}"
                    tmux -L "$SOCKET" send-keys -t "$SESSION" -l "$char"
                    sleep 0.08
                done
                ;;
        esac
    done

    # Give a moment for the final state to render, then kill the session
    sleep 0.5
    tmux -L "$SOCKET" kill-session -t "$SESSION" 2>/dev/null || true
) &
KEY_PID=$!

# Record the tmux session
# TERM must be set for tmux attach to work properly with asciinema
TERM=xterm-256color asciinema rec \
    --cols "$COLS" \
    --rows "$ROWS" \
    --command "tmux -L '$SOCKET' attach -t '$SESSION'" \
    --overwrite \
    "$CAST_FILE" 2>/dev/null || true

# Wait for the key-sender to finish
wait "$KEY_PID" 2>/dev/null || true

# --- Show errors if any ---

if [[ -s "$ERRLOG" ]]; then
    echo "Program stderr:" >&2
    head -20 "$ERRLOG" >&2
fi

# --- Verify recording ---

if [[ ! -s "$CAST_FILE" ]]; then
    echo "Error: asciinema recording is empty" >&2
    exit 1
fi

echo "Recorded $(wc -l < "$CAST_FILE") frames to $CAST_FILE" >&2

# --- Convert to GIF ---

echo "Converting to GIF..." >&2
agg \
    --font-size "$FONT_SIZE" \
    --speed "$SPEED" \
    --theme "$THEME" \
    "$CAST_FILE" "$OUTPUT_GIF"

echo "Done: $OUTPUT_GIF ($(du -h "$OUTPUT_GIF" | cut -f1))" >&2
