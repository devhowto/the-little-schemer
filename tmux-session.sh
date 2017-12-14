#!/usr/bin/env bash

# Place where I manipulate files when I an studying my Anki cards
# and need to run quick snippets of code to assert my knowledge of
# the things I am committing to the long-term memory.
dir="${HOME}/develop/the-little-schemer"
ses='the-little-schemer'

tmux new-session -d -s "$ses" -c "$dir"
tmux rename-window 'emacs' \; send-keys $'emacs -nw definitions.scm' C-m
tmux new-window -t "${ses}:2" -n 'vim' -c "$dir" \; send-keys $'vim the-little-schemer.adoc' C-m
tmux new-window -t "${ses}:3" -n 'shell' -c "$dir"

tmux select-window -t "${ses}:1"
tmux -2 attach-session -t "${ses}"

