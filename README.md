# stack-all

A CLI tool for building Haskell projects easily over Stackage major versions.

This is how I do my Haskell "build ci" now locally.

## Usage

`stack-all` by default runs `stack build` over
recent Stackage LTS major versions and Nightly
(current default is nightly, lts-17, lts-16, lts-14,... , lts-11)
corresponding to the latest major ghc minor verions.

`stack-all` can use `stack-ltsXX.yaml` files to override the default `stack.yaml`
file for particular lts major versions. Note that a `stack-ltsXX.yaml` file
will also be used for earlier lts major version until
another `stack-ltsYY.yaml` file is found.
For example if you have `stack-lts14.yaml` and `stack-lts12.yaml` files in your
project, then `stack.yaml` will be used as normal to build nightly, lts-17 and lts-16, `stack-lts14.yaml` will be used for building lts-14 and lts-13,
and then `stack-lts12.yaml` will be used for lts-12, lts-11 (and earlier).

(Note that other versioned stack.yaml filenames like stack-ghc-8.8.yaml
are not currently supported.)

You can set the oldest working LTS major version for a project in
a `.stack-all` config file in your project. For example:
```
[versions]
# lts-12 foo-bar lib too old
oldest = lts-13
```
specifies that the oldest version to build for is lts-13.
This config file can be generated with `stack-all -c -o lts-13`.
You can also specify the oldest lts to build on the commandline,
eg `stack-all -o lts-13`.

stack commands and options can also be passed on the command line: eg
```
$ stack-all test
```
will run `stack test` over the LTS versions (instead of `stack build`).

Happy stack building!

## Install
Run `stack install` or `cabal install` in the source.

## Contribute or discuss
at https://github.com/juhp/stack-all
