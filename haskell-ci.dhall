let haskellCi =
      https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/haskell-ci.dhall sha256:2f7f742e67407a569b4b4749692d29d520131fb146a6e096012ff31c5f43bb03

let concatMapSep =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/9f259cd68870b912fbf2f2a08cd63dc3ccba9dc3/Prelude/Text/concatMapSep sha256:c272aca80a607bc5963d1fcb38819e7e0d3e72ac4d02b1183b1afb6a91340840

let showVersion = concatMapSep "." Natural Natural/show

let installLibarchive =
      λ(v : List Natural) →
        let versionString = showVersion v

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
          , haskellCi.cabalBuildWithFlags
              [ "all", "--project-file", "cabal.project.ci" ]
          , haskellCi.cabalBuildWithFlags
              [ "all"
              , "--project-file"
              , "cabal.project.ci"
              , "--constraint='hstar +pure'"
              ]
          ]
          ( Some
              { ghc = [ haskellCi.GHC.GHC865, haskellCi.GHC.GHC883 ]
              , cabal = [ haskellCi.Cabal.Cabal30 ]
              }
          )
      ⫽ { on = [ haskellCi.Event.push, haskellCi.Event.pull_request ] }
    : haskellCi.CI.Type
