let haskellCi =
      https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/haskell-ci.dhall sha256:0436a5c08f98e083bfb147330ff273f2b182a9e695e0e67441c0f22c3bf4dc4d

in  haskellCi.hlintAction [ "." ] : haskellCi.CI.Type
