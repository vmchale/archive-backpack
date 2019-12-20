let dhallCi =
      https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/dhall-ci.dhall sha256:6ab09069c0b558cee86b55e8243f35e648e76288e93a716c4b634275c230c4c4

in    dhallCi.dhallSteps
        [ dhallCi.dhallYamlInstall
        , dhallCi.checkDhallYaml
            [ "dhall-ci.dhall", "haskell-ci.dhall", "hlint-ci.dhall" ]
        ]
    : dhallCi.CI
