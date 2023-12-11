# wgslink

![zig-version](https://img.shields.io/badge/dynamic/yaml?url=https%3A%2F%2Fraw.githubusercontent.com%2Fclickingbuttons%2Fwgslink%2Fmaster%2F.github%2Fworkflows%2Ftest.yml&query=%24.jobs.test.steps%5B1%5D.with.version&label=zig-version)
![tests](https://github.com/clickingbuttons/wgslink/actions/workflows/test.yml/badge.svg)

WGSL bundler and minifier.

## Installation

`npm i --save-dev wgslink`

## Usage

`./node_modules/.bin/wgslink --help`

```sh
    -m, --minify
            Remove whitespace.

    -l, --layout
            Extract bind group layout to JSON file or stderr.

    -o, --outdir <str>
            Output directory (otherwise will print to stdout).

    -e, --entry <str>...
            Symbols in entry files' global scope to NOT tree shake.

            	If not specified will default to functions with @vertex, @fragment, or @compute attributes.

            	If any entry is "*" no tree shaking will occur.

    <str>...
            Entry files
```
