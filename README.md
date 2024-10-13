# stack-all

A CLI tool for building Haskell projects easily
over several Stackage major versions.

I use this to do Haskell build CI for projects locally with stack.

## Usage

`stack-all` by default runs `stack build` over Stackage Nightly and
LTS major versions
(the current default range is nightly & major LTS versions back to lts-18)
corresponding to latest major ghc minor versions,
using appropriate stack `--resolver` options.

Note that `stack` only works in a project if a `stack.yaml` file exists.
If no `stack.yaml` file is found in a .cabal project,
`stack-all` will create one if there is a .cabal or "package.yaml" file.
Of course it may still fail to build, but this allows for quick attempts
to build a package that does not include a stack.yaml file.

Since 0.6, stack-all also works outside projects, like stack itself does.

### Help output
`$ stack-all --version`

```
0.6.4
```
`$ stack-all --help`

```
Build project over Stackage major versions

Usage: stack-all [--version] [-k|--keep-going] [-D|--debug] [--refresh-cache] 
                 [-n|--newest MAJOR] [(-o|--oldest MAJOR) | (-a|--all-lts)] 
                 [(-c|--create-config) | (-d|--default-resolver MAJOR) | 
                   (-u|--update-resolver) | (-s|--make-lts MAJOR) | 
                   [MAJORVER... COMMAND...]]

  https://github.com/juhp/stack-all#readme

Available options:
  -h,--help                Show this help text
  --version                Show version
  -k,--keep-going          Keep going even if an LTS fails
  -D,--debug               Verbose stack build output on error
  --refresh-cache          Force refresh of stackage snapshots.json cache
  -n,--newest MAJOR        Newest LTS release to build from
  -o,--oldest MAJOR        Oldest compatible LTS release
  -a,--all-lts             Try to build back to LTS 1 even
  -c,--create-config       Create a project .stack-all file
  -d,--default-resolver MAJOR
                           Set stack.yaml resolver
  -u,--update-resolver     Update stack.yaml resolver
  -s,--make-lts MAJOR      Create a stack-ltsXX.yaml file
```

### Overriding stack.yaml
`stack-all` can use `stack-ltsXX.yaml` files to override the default
`stack.yaml` file for particular Stackage major versions.
Note that a `stack-ltsXX.yaml` file will also be used for
older LTS major versions until another `stack-ltsYY.yaml` file is found.
`stack-nightly.yaml` is also supported, but used only for nightly.

For example if you have `stack-lts20.yaml` and `stack-lts18.yaml` files
in your project,
then `stack.yaml` will be used as normal to build nightly, lts-22 and lts-21,
but `stack-lts20.yaml` will be used for building lts-20 and lts-19,
and `stack-lts18.yaml` will be used for lts-18, lts-16 (and older).
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
`lts` is also accepted and resolves to the latest major LTS version.

You can also use ghc major version aliases:
eg `ghc9.6` corresponds to `lts22` or `ghc-9.2` to `lts-20`.

There are `--oldest`  and `--newest` options to specify the range of
lts versions to build over:

You can specify the oldest major LTS to build for with eg `stack-all -o lts16`.
Otherwise if not configured the default oldest LTS is currently `lts-18`.

Similarly you can specify the newest LTS version to build from with
eg `stack-all -n lts20`. (The default is to build from nightly.)

Alternatively, one can give one or more explicit LTS major versions to build
for as arguments: eg `stack-all lts19` if you only wish to build that version.

### Configuring the oldest and/or newest LTS to build
You can configure the oldest working LTS major version for your project
by running for example `stack-all -c -o lts-19` which generates a `.stack-all`
project config file like this:
```
[versions]
# lts-18 too old
oldest = lts-19
```
(the comment line can be used to document why the older LTS doesn't work).
This specifies that the oldest LTS version to build for is lts-19.

The newest LTS to build with stack-all can similarly be configured:
`stack-all -c -n lts21` or setting `newest = lts-21`.

### Running other stack commands
By default `stack-all` just runs the stack `build` command over
Stackage major versions.

You can also specify a stack command to run with options on the commandline:
eg
```
$ stack-all test --no-rerun-tests
```
will run `stack test` over the LTS versions instead.


Any stack command can be used, possibly outside a project,
eg: `stack-all list aeson`

Happy stack building!

## Install
The project is released on Hackage.

You can also build from a git clone with `stack install` or `cabal install`.

## Collaboration
The project is hosted at https://github.com/juhp/stack-all under a BSD license.
