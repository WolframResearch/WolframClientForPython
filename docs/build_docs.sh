#!/bin/bash

#> Wolfram Python documentation build script. (UNIX only)
#> This script builds the documentation static webpages from restructured files,
#> using sphinx make.
#> 
#> OPTIONS:
#> 
#>     -b path, --build=path
#>         set the output directory to path. It must be a valid directory.
#>         BUILDDIR is set to path if specified, otherwise the default 
#>         directory _build is used.
#> 
#>     -a, --all
#>         rebuild the api directory. This is not equivalent to `make clean`,
#>         because the later is to aggressiv and also delete directories such
#>         as .git, which is likely be an issue.
#> 
#>     -h display this page.
#> 

function help(){
    less $0 | grep -e "^#>" | sed 's/^#> \(.*\)$/\1/g'
    exit $1 || 0
}

export LC_ALL="en_US.UTF-8"
export LC_CTYPE="en_US.UTF-8"

target='_build'

while [ "$#" -gt 0 ]; do
    case "$1" in
        -a) all=1; shift 1;;
        -b) target="$2"; shift 2;;
        -h) help 0;;
        --all) all=1; shift 1;;
        --build=*) target="${1#*=}"; shift 1;;
        --help) help 0;;
        -*) echo "unknow option: $1"; exit 1;;
        *) echo "unexpected argument: $1"; exit 1;;
    esac
done

if [[ -d "${target}" ]]; then
    export BUILDDIR="${target}"
else
    echo "Invalid build directory: ${target}"
    exit 1
fi

# clean up
if [[ $all == 1 ]]; then
    [[ -r ./api ]] && rm -r ./api
fi
# static analysis
sphinx-apidoc -o api ../wolframclient ../wolframclient/tests*

# build html
make html