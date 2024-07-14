#!/usr/bin/env -S bash -euE
# submit

# npm install atcoder-cli
acc='npx acc'

toy-lib -e "$1/Main.hs" > "$1/.submit.hs"
cd "$1"
$acc submit -s -- -y

