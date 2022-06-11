#!/usr/bin/env bash -euE

run() {
    stack build && stack exec "$1-exe"
}

f="app/${1}-input"

if [[ -f "$f" ]] ; then
    cat "$f" | run "$@"
else
    run "$@"
fi

