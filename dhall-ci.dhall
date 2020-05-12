let dhallCi =
      https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/dhall-ci.dhall sha256:e2a9c7e84d04fcfdf6b1862ad5c34847b0f5e5a308b8cf7a3513b5f2b36b7233

in    dhallCi.dhallSteps
        [ dhallCi.dhallYamlInstall
        , dhallCi.checkDhallYaml
            [ "dhall-ci.dhall", "haskell-ci.dhall", "hlint-ci.dhall" ]
        ]
    : dhallCi.CI
