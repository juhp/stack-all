# Revision history for stack-all

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
