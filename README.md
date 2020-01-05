# archive-backpack

This is a demo of GHC's backpack, used to fuse the functionality of
[libarchive](http://hackage.haskell.org/package/libarchive) and
[tar](http://hackage.haskell.org/package/tar).

## Development

### Building

`archive-backpack` is built exclusively using
[cabal-install](https://www.haskell.org/cabal/).


```
cabal build all
```

### CI

CI is provided via
[github-actions-dhall](https://github.com/vmchale/github-actions-dhall). To edit
the CI scripts, edit `haskell-ci.dhall` and run

```
make ci
```
