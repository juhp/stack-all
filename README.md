# stack-all

A CLI tool for building Haskell projects easily over Stackage major versions.

This is how I do my Haskell "build ci" now locally.

## Usage

`stack-all` by default runs `stack build` over
recent Stackage LTS major versions and Nightly
(current default is nightly, lts-17, lts-16, lts-14,... , lts-11)
corresponding to latest major ghc minor versions,
using the stack `--resolver` option.

`stack-all` can use `stack-ltsXX.yaml` files to override the default `stack.yaml`
file for particular lts major versions. Note that a `stack-ltsXX.yaml` file
will also be used for earlier lts major versions until
another `stack-ltsYY.yaml` file is found. For example if you have
`stack-lts14.yaml` and `stack-lts12.yaml` files in your project,
then `stack.yaml` will be used as normal to build nightly, lts-17 and lts-16,
`stack-lts14.yaml` will be used for building lts-14 and lts-13,
and `stack-lts12.yaml` will be used for lts-12, lts-11 (and earlier).
Since `stack-all` overrides the resolver with `--resolver lts-XX` the exact minor
lts version specified in the `stack*.yaml` files doesn't actually
matter.

(Other versioned stack.yaml filenames like stack-ghc-8.8.yaml
are not currently supported.)

You can set the oldest working LTS major version for your project
by running for example `stack-all -c -o lts-13` which generates a `.stack-all`
project config file like this:
```
[versions]
# lts-12 too old
oldest = lts-13
```
(the comment line can be used to document why the older lts doesn't work).
This specifies that the oldest lts version to build for is lts-13.
You can also specify the oldest major lts to build for on the commandline,
with just `stack-all -o lts-13`.

stack commands and options can also be passed on the command line: eg
```
$ stack-all test --no-rerun-tests
```
will run `stack test` over the LTS versions (instead of `stack build`).

Note you can abbreviate `lts-XX` to `ltsXX` and you can also specify specific lts version(s) to build for, like `stack-all lts14` if you only wish to build that version.

Happy stack building!

## Install
The project is released on Hackage.

You can also build from source with `stack install` or `cabal install`.

## Collaboration
The project is hosted at https://github.com/juhp/stack-all under BSD license.
