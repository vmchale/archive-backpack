let haskellCi =
      https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/haskell-ci.dhall sha256:32bae084b52a84d8f5d52727b09717baf4b353a2cbf56c3c66d523ca905b237a

in  haskellCi.hlintAction [ "." ] : haskellCi.CI.Type
