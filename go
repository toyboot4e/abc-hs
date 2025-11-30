#!/usr/bin/env -S bash -euE

figlet -f univers -w 1000 "$(printf '%s' "$1" | tr '[:lower:]' '[:upper:]')"

pane="$(tmux split-window -v -c  "#{pane_current_path}" -l 4 -P -F "#{pane_id}")"
tmux select-pane -U

./new "$1" haskell > /dev/null 2>&1 &
sleep 8

cd "$1"
tmux send-keys -t "$pane" "cd ${1}" Enter
tmux send-keys -t "$pane" "time cabal build" Enter
tmux send-keys -t "$pane" "j tc a"
em a/Main.hs

