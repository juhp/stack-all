# stack-all

A CLI tool for building Haskell projects easily over Stackage major versions.

This is how I do my Haskell "build ci" now locally.

## Usage

`stack-all` runs `stack build` over recent Stackage LTS major versions
and Nightly: by default currently: nightly, lts-16, ..., lts-11.

You can set the oldest working LTS for the project using `stack-all -o lts-13`
or in a `.stack-all` file containing:

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
