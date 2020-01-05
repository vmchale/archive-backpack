let haskellCi =
      https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/haskell-ci.dhall sha256:049b967041f7959e86ef70d1d6f82cc826602adfb97839c1e73160a3b55d4249

in  haskellCi.hlintAction [ "." ] : haskellCi.CI.Type
