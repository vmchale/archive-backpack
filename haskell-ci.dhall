let haskellCi =
      https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/haskell-ci.dhall sha256:049b967041f7959e86ef70d1d6f82cc826602adfb97839c1e73160a3b55d4249

let concatMapSep =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/9f259cd68870b912fbf2f2a08cd63dc3ccba9dc3/Prelude/Text/concatMapSep sha256:c272aca80a607bc5963d1fcb38819e7e0d3e72ac4d02b1183b1afb6a91340840

let showVersion = concatMapSep "." Natural Natural/show

let installLibarchive =
        λ(v : List Natural)
      → let versionString = showVersion v

        in  haskellCi.BuildStep.Name
              { name = "Install libarchive"
              , run =
                  ''
                  wget https://www.libarchive.org/downloads/libarchive-${versionString}.tar.gz
                  tar xvf libarchive-${versionString}.tar.gz
                  cd libarchive-${versionString}
                  ./configure
                  make -j
                  sudo make install
                  ''
              }

in      haskellCi.generalCi
          [ haskellCi.checkout
          , haskellCi.haskellEnv haskellCi.matrixEnv
          , installLibarchive [ 3, 4, 2 ]
          , haskellCi.cabalWithFlags "update" ([] : List Text)
          , haskellCi.cabalBuildWithFlags [ "all" ]
          , haskellCi.cabalBuildWithFlags
              [ "all", "--constraint='hstar +pure'" ]
          ]
          ( Some
              { ghc =
                [ haskellCi.GHC.GHC844
                , haskellCi.GHC.GHC865
                , haskellCi.GHC.GHC881
                ]
              , cabal = [ haskellCi.Cabal.Cabal30 ]
              }
          )
      ⫽ { on = [ haskellCi.Event.push, haskellCi.Event.pull_request ] }
    : haskellCi.CI.Type
