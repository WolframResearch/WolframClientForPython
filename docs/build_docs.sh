#!/bin/sh

# clean up
if [[ "$1" == "-all" ]]; then
    [[ -r ./api ]] && rm -r ./api
    make clean
fi
# static analysis
sphinx-apidoc -o api ../wolframclient ../wolframclient/tests*
# build html
make html