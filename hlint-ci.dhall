let haskellCi =
      https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/haskell-ci.dhall sha256:abbcf1ffd0630835e80fe7c953e6fd2cacc8f8a2f70a0250b7e8f5a68171b232

in  haskellCi.hlintAction [ "." ] : haskellCi.CI.Type
