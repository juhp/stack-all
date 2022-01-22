# stack-all

A CLI tool for building Haskell projects easily over Stackage major versions.

This is how I do my Haskell build CI for projects locally with stack.

## Usage

`stack-all` by default runs `stack build` over Stackage Nightly and
LTS major versions (current default is nightly & LTS 18, 16, 14, 13, 12, 11)
corresponding to latest major ghc minor versions,
with appropriate stack `--resolver` options.

Note that `stack` only works if a `stack.yaml` file exists.
If no `stack.yaml` file is found, `stack-all` will create one.
Of course it may still fail to build, but this allows for
quickly trying to build a package that does not include stack support.

### Overriding stack.yaml
`stack-all` can use `stack-ltsXX.yaml` files to override the default
`stack.yaml` file for particular Stackage major versions.
Note that a `stack-ltsXX.yaml` file will also be used for
older LTS major versions until another `stack-ltsYY.yaml` file is found.
`stack-nightly.yaml` is also supported, but used only for nightly.

For example if you have `stack-lts14.yaml` and `stack-lts12.yaml` files
in your project,
then `stack.yaml` will be used as normal to build nightly, lts-18 and lts-16,
but `stack-lts14.yaml` will be used for building lts-14 and lts-13,
and `stack-lts12.yaml` will be used for lts-12, lts-11 (and older).
Since `stack-all` overrides the exact resolver with the latest minor snapshot,
the exact minor Stackage version specified in the `stack*.yaml` files
doesn't actually matter: `stack-all` always uses the latest published
minor releases of Stackage major versions.

`stack-ltsXX.yaml` files can be easily created using
`stack-all --make-lts ltsXX` (or `-s ltsXX` for short).

(Other versioned stack.yaml filenames like stack-ghc-8.8.yaml
are not currently supported.)

### Specifying LTS versions
You can abbreviate `lts-XX` args to `ltsXX` on the commandline.
`lts` is also accepted and resolved to the latest major LTS version.

There are `--oldest`  and `--newest` options to specify the range of
lts versions to build over:

You can specify the oldest major LTS to build for with eg `stack-all -o lts13`.
Otherwise if not configured the default oldest LTS is currently `lts-11`.

Similarly you can specify the newest LTS version to build from with
eg `stack-all -n lts16`. (The default is to build from nightly.)

Alternatively, one can give one or more explicit LTS major versions to build
for as arguments: eg `stack-all lts14` if you only wish to build that version.

### Configuring the oldest and/or newest LTS to build
You can configure the oldest working LTS major version for your project
by running for example `stack-all -c -o lts-13` which generates a `.stack-all`
project config file like this:
```
[versions]
# lts-12 too old
oldest = lts-13
```
(the comment line can be used to document why the older LTS doesn't work).
This specifies that the oldest LTS version to build for is lts-13.

The newest LTS to build with stack-all can similarly be configured:
`stack-all -c -n lts18` or setting `newest = lts-18`.

### Running other stack commands
By default `stack-all` just runs the stack `build` command over
Stackage major versions.

You can also specify a stack command to run with options on the commandline:
eg
```
$ stack-all test --no-rerun-tests
```
will run `stack test` over the LTS versions instead.

Happy stack building!

## Install
The project is released on Hackage.

You can also build from git source with `stack install` or `cabal install`.

## Collaboration
The project is hosted at https://github.com/juhp/stack-all
under a BSD license.
