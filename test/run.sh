#!/bin/sh

cd $(dirname $0)

for file in *-test.el; do
    echo "=== $file ==="
    emacs --script $file
done
