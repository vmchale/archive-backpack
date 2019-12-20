let haskellCi =
      https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/haskell-ci.dhall sha256:5690c3ba762328bbe4409015cc1ebf2706c0b2e367733740deb7bee9d4252eac

in  haskellCi.hlintAction [ "." ] : haskellCi.CI.Type
