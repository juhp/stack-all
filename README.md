# stack-all

A CLI tool for building Haskell projects easily over Stackage major versions.

## Usage

`stack-all` runs `stack build` over recent Stackage LTS major versions
and Nightly.

You can set the oldest working LTS for the project using `stack-all -o lts-13`
or a `.stack-all` file:

```
$ cat .stack-all
[versions]
oldest = lts-13
```

which can be created with `stack-all -c -o lts-13`.

Happy stack building!

This is how I do my "build ci" now locally.

## Install
Run `stack install` or `cabal install` in the source.

## Contribute
See https://github.com/juhp/stack-all
