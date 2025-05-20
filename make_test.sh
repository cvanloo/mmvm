#!/usr/bin/env bash

A="${A:-a.out}"

if ! command -v mmvm 2>&1 >/dev/null
then
    echo "mmvm installation not found"
    exit 1
fi

go build mmvm.go
./mmvm -d "$A" > mine.disas
mmvm -d "$A" > other.disas 2>&1
diff mine.disas other.disas
