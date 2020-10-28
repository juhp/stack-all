# stack-all

A CLI tool for building Haskell projects easily over Stackage major versions.

This is how I do my Haskell "build ci" now locally.

## Usage

`stack-all` runs `stack build` over recent Stackage LTS major versions
and Nightly: by default currently: nightly, lts-16, ..., lts-11.

Note that stack-all will automatically use `stack-ltsXX.yaml`, even for older lts releases: eg say you have `stack-lts13.yaml` in your project, then it will also be used for building lts-12 (unless you have a `stack-lts12.yaml` config file of course).

You can specify the oldest working LTS for a project with `stack-all -o lts-13` or set it in a `.stack-all` file containing:
```
[versions]
oldest = lts-13
```
which can be created with `stack-all -c -o lts-13`.

Happy stack building!

## Install
Run `stack install` or `cabal install` in the source.

## Contribute
See https://github.com/juhp/stack-all
