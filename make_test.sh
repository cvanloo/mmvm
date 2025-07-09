#!/usr/bin/env bash

MODE="${MODE:--d}"
LIMIT=$((${LIMIT:-0}))

go build mmvm.go || exit 1
if ! command -v mmvm 2>&1 >/dev/null
then
    echo "mmvm installation not found"
    exit 1
fi

function run_diff {
    if [[ $LIMIT > 0 ]]; then
        ./mmvm "$MODE" -n $((LIMIT-1)) "$1" > mine.disas
        mmvm "$MODE" "$1" 2>&1 | head -n $LIMIT > other.disas 
    else
        ./mmvm "$MODE" "$1" > mine.disas
        mmvm "$MODE" "$1" > other.disas 2>&1
    fi
    local d=$(diff --suppress-common-lines mine.disas other.disas)
    echo "$d"
}

if [[ "$1" == "all" ]]; then
    TEST_FILES_DIR="${TEST_FILES_DIR:-test_programs}"
    local st=0
    for file in $TEST_FILES_DIR/*.out; do
        d=$(run_diff $file)
        if [[ -n "$d" ]]; then
            st=1
            echo "$d"
        fi
    done
    exit "$st"
else
    A="${A:-a.out}"
    d=$(run_diff $A)
    if [[ -n "$d" ]]; then
        echo "$d"
        exit 1
    fi
fi

