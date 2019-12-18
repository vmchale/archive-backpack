.PHONY: ci

SHELL := bash
MAKEFLAGS += --warn-undefined-variables --no-builtin-rules -j
.SHELLFLAGS := -eu pipefail
.DELETE_ON_ERROR:

ci: .github/workflows/haskell.yml .github/workflows/dhall.yml .github/workflows/hlint.yml

.github/workflows:
	mkdir -p $@

.github/workflows/haskell.yml: haskell-ci.dhall .github/workflows
	dhall-to-yaml --file $< --output $@

.github/workflows/dhall.yml: dhall-ci.dhall .github/workflows
	dhall-to-yaml --file $< --output $@

.github/workflows/hlint.yml: hlint-ci.dhall .github/workflows
	dhall-to-yaml --file $< --output $@
