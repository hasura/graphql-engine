# ci-info-hs

Checks if the current environment is a Continuous Integration server.

This is a Haskell port of [watson/ci-info](https://github.com/watson/ci-info).

## Usage

Check if the current environment is a CI server:

```haskell
isCI :: IO Bool
```

Get the name of the CI vendor. Returns `Nothing` if no CI could be detected. Returns `Just CI_UNKNOWN_VENDOR` if a CI was detected, but the vendor name could not be determined:

```haskell
getCI :: IO (Maybe CI)
```
