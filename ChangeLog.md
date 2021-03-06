# Release history for stack-all

## 0.3 (2021-06-26)
- use hackage.stackage.org/snapshots.json:
  - to determine the latest LTS major version
  - to resolve major LTS version to latest minor snapshot and also print it
- cache snapshots.json locally in ~/.cache/stack-all/ for a few hours
- new --make-lts command to generate a stack-ltsXX.yaml file from newer one

## 0.2.2 (2021-06-07)
- create a stack.yaml file if only an .cabal file is found

## 0.2.1 (2021-04-30)
- if stack fails, print the ~/.stack snapshot path for reference

## 0.2 (2021-04-03)
- better error messages when reading snapshot option/arg (TristanCacqueray)
- search parent dirs for stack project dir, like stack
- snapshot options/args can now be in compact ltsXY form as well as lts-XY
- support stack commands and options

## 0.1.2 (2021-02-05)
- --create-config comment line mentions older version
- show error for unversioned "stack-lts.yaml"
- add lts-17

## 0.1.1 (2020-12-04)
- fix ordering of stack-lts-*.yaml
- allow --newest to override oldest lts config

## 0.1.0 (2020-11-14)
- initial release with --create-config, --debug, --cmd, --newest
- VersionSpec: --all-lts, --oldest, and lts args
