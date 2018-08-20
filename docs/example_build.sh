#!/bin/sh

# clean up
[[ -r ./api ]] && rm -r ./api
make clean
# static analysis
sphinx-apidoc -o api ../wolframclient ../wolframclient/tests*
# build html
make html