let haskellCi =
      https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/haskell-ci.dhall sha256:e48dbc8852b9a41fe15486b9963dd30940675219054939c7bb7a296030814b18

in    haskellCi.generalCi
        [ haskellCi.checkout
        , haskellCi.haskellEnv haskellCi.matrixEnv
        , haskellCi.cabalBuildWithFlags [ "all" ]
        ]
        ( Some
            { ghc =
                [ haskellCi.GHC.GHC844
                , haskellCi.GHC.GHC865
                ]
            , cabal = [ haskellCi.Cabal.Cabal30 ]
            }
        )
    : haskellCi.CI.Type
