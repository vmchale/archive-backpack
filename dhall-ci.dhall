let dhallCi =
      https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/dhall-ci.dhall sha256:50c5c1017d3661f47c915e0c64e9735773dc4e22770c07f46bae168a2dffc44d

in      dhallCi.dhallSteps
          [ dhallCi.dhallYamlInstall
          , dhallCi.checkDhallYaml
              [ "dhall-ci.dhall", "haskell-ci.dhall", "hlint-ci.dhall" ]
          ]
    : dhallCi.CI
