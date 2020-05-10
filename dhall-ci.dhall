let dhallCi =
      https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/dhall-ci.dhall sha256:e5a66a2eed19fea8ee8d397ea25218b32fbab2abed758d31b7a2fe0fdea6f433

in    dhallCi.dhallSteps
        [ dhallCi.dhallYamlInstall
        , dhallCi.checkDhallYaml
            [ "dhall-ci.dhall", "haskell-ci.dhall", "hlint-ci.dhall" ]
        ]
    : dhallCi.CI
