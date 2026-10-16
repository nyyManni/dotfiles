#!/bin/bash
# Push-to-talk helper script.
# Starts on keydown, sleeps until signalled on keyup.
# On SIGUSR1 (key release): types text and exits cleanly.

LOCK_FILE="/tmp/push-to-talk.lock"
PID_FILE="/tmp/push-to-talk.pid"

# Acquire an exclusive lock (non-blocking). If another instance is already
# running, kill it first, then take the lock.
exec 9>"$LOCK_FILE"
if ! flock -n 9; then
    # Another instance holds the lock — kill it and wait for the lock.
    if [[ -f "$PID_FILE" ]]; then
        kill "$(cat "$PID_FILE")" 2>/dev/null
    fi
    flock 9
fi

cleanup() {
    rm -f "$PID_FILE"
    flock -u 9
}

on_release() {
    wtype "PTT_RELEASED"
    cleanup
    exit 0
}

trap on_release SIGUSR1
trap 'cleanup; exit 1' SIGTERM SIGINT

echo $$ > "$PID_FILE"

# Sleep indefinitely. `wait` on a backgrounded sleep is interruptible by
# signals, unlike a foreground `sleep infinity`.
while true; do
    sleep infinity &
    wait $!
done
