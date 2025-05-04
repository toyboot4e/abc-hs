# Just a task runner
# <https://github.com/casey/just>

# shows this help message
help:
    @just -l

# runs oj test
[no-cd]
test problem:
    @cabal build "{{problem}}-exe" # --ghc-options -DDEBUG
    @oj t -N --directory "{{problem}}/test-cases" -c "cabal run {{problem}}-exe"

[private]
alias t := test

# runs the executable
[no-cd]
run problem:
    @cabal run "{{problem}}-exe" # --ghc-options -DDEBUG

[private]
alias r := run

# builds the executable
[no-cd]
build problem:
    @cabal build "{{problem}}-exe" # --ghc-options -DDEBUG

[private]
alias b := build

# submits the code
[no-cd]
submit problem:
    @toy-lib -e "{{problem}}/Main.hs" > "{{problem}}/.submit.hs"
    @cd "{{problem}}" && acc submit -s -- -y

[private]
alias s := submit

# submits and tests the code
[no-cd]
test-and-submit problem: && (test problem) (submit problem)

[private]
alias ts := test-and-submit

# test and copy the bundled file
[no-cd]
copy problem:
    #!/usr/bin/env bash
    if [ {{os()}} == "macos" ] ; then
        toy-lib -e "{{problem}}/Main.hs" | pbcopy
    else
        # X Window System
        toy-lib -e "{{problem}}/Main.hs" | xclip -sel clip
    fi

[private]
alias c := copy

# copy the bundled file
[no-cd]
test-and-copy problem: && (test problem) (copy problem)

[private]
alias tc := test-and-copy
