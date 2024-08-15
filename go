#!/usr/bin/env -S bash -euE

figlet -f univers "$(printf '%s' "$1" | tr '[:lower:]' '[:upper:]')" -w 1000

pane="$(tmux split-window -v -c  "#{pane_current_path}" -l 4 -P -F "#{pane_id}")"
tmux select-pane -U

./new "$1" > /dev/null 2>&1 &
sleep 3

cd "$1"
tmux send-keys -t "$pane" "cd ${1}" Enter
tmux send-keys -t "$pane" "time cabal build" Enter
tmux send-keys -t "$pane" "../t a"
em a/Main.hs

