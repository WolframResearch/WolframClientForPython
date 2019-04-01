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
#>         rebuild from scratch. Call `make clean` and remove the api directory.
#>         Useful when the codebase has changed and some source files were removed.
#> 
#>     -h display this page.
#> 

function help(){
    less $0 | grep -e "^#>" | sed 's/^#> \(.*\)$/\1/g'
    exit $1 || 0
}

export LC_ALL="en_US.UTF-8"
export LC_CTYPE="en_US.UTF-8"

target=''

while [ "$#" -gt 0 ]; do
    case "$1" in
        -a) all=1; shift 1;;
        -b) target="$2"; shift 2;;
        -h) help 0;;
        --all) all=1; shift 1;;
        --build=*) target="${1#*=}"; shift 1;;
        --help) help 0;;
        -*) echo "unknown option: $1"; exit 1;;
        *) echo "unexpected argument: $1"; exit 1;;
    esac
done

if [[ ! -z "${target}" ]]; then
    if [[ ! -d "${target}" ]]; then
        echo "Invalid build directory: ${target}"
        exit 1
    fi 
    # normalize path
    target="`dirname \"${target}\"`/`basename \"${target}\"`"
fi

# clean up
if [[ $all == 1 ]]; then
    [[ -r ./api ]] && rm -r ./api
    if [[ ! -z "${target}" ]]; then
        echo "Removing ${target}/html"
        rm -r "${target}/html"
        echo "Removing ${target}/*.js"
        rm "${target}/*.js"
        echo "Removing ${target}/*.html"
        rm "${target}/*.html"
    fi
fi
# static analysis
sphinx-apidoc -o api ../wolframclient ../wolframclient/tests*

# build html
make html

echo "Compile mma.scss to ./_build/html/_static/mma.css"
sass ./wri_theme/static/mma.scss > "./_build/html/_static/mma.css"

if [[ ! -z "${target}" ]]; then
    echo "Copying _build to target: ${target}"
    cp -r "./_build/html/." "${target}"
fi