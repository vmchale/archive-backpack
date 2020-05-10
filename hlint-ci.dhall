let haskellCi =
      https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/haskell-ci.dhall sha256:ef332e5a6a293a84ebc6b52fe5889f7000b4621378970dc304671125d4a5259c

in  haskellCi.hlintAction [ "." ] : haskellCi.CI.Type
