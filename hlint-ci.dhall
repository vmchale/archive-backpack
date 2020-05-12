let haskellCi =
      https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/haskell-ci.dhall sha256:2f7f742e67407a569b4b4749692d29d520131fb146a6e096012ff31c5f43bb03

in  haskellCi.hlintAction [ "." ] : haskellCi.CI.Type
