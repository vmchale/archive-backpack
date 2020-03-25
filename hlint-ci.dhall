let haskellCi =
      https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/haskell-ci.dhall sha256:2de95c8bd086c21660c2849dfe2d9af72e675bed44396159d647292d329a20e4

in  haskellCi.hlintAction [ "." ] : haskellCi.CI.Type
