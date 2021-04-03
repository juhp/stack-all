# stack-all

A CLI tool for building Haskell projects easily over Stackage major versions.

This is how I do my Haskell "build ci" now locally.

## Usage

`stack-all` by default runs `stack build` over
recent Stackage LTS major versions and Nightly
(current default is nightly, lts-17, lts-16, lts-14,... , lts-11)
corresponding to latest major ghc minor verions.

Note that stack-all will automatically use `stack-ltsXX.yaml`, even for older lts releases: eg say you have `stack-lts13.yaml` in your project, then it will also be used for building lts-12 (unless you have a `stack-lts12.yaml` config file of course).  (Other versioned stack.yaml filenames like stack-ghc-8.8.yaml are not supported currently.)

You can specify the oldest working LTS for a project with `stack-all -o lts-13` or set it in a `.stack-all` file containing:
```
[versions]
# lts-12 foo-bar too old
oldest = lts-13
```
which can be created with `stack-all -c -o lts-13`.

You can also pass stack commands and options on the command line: eg
```
$ stack-all test
```
will run `stack test` over the LTS versions, etc (instead of `stack build`).

Happy stack building!

## Install
Run `stack install` or `cabal install` in the source.

## Contribute or discuss
at https://github.com/juhp/stack-all
