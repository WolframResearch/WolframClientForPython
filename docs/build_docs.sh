#!/bin/sh

export LC_ALL="en_US.UTF-8"
export LC_CTYPE="en_US.UTF-8"

# clean up
if [[ "$1" == "-all" ]]; then
    [[ -r ./api ]] && rm -r ./api
    make clean
fi
# static analysis
sphinx-apidoc -o api ../wolframclient ../wolframclient/tests*
# build html
make html