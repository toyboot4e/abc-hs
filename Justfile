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

# submits the code
[no-cd]
submit problem:
    @toy-lib -e "{{problem}}/Main.hs" > "{{problem}}/.submit.hs"
    @cd "{{problem}}" && npx acc submit -s -- -y

[private]
alias s := submit

# submits and tests the code
[no-cd]
test-and-submit problem: && (test problem) (submit problem)

[private]
alias ts := test-and-submit

