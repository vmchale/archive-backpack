let haskellCi =
      https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/haskell-ci.dhall sha256:48c4cdf0faac0d1fd40884ff938abb3f1e8049e09a032fbec24e8ca337ce6ff9

in  haskellCi.hlintAction [ "." ] : haskellCi.CI.Type
