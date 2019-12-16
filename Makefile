.PHONY: ci

ci: .github/workflows/haskell.yml

.github/workflows:
	mkdir -p $@

.github/workflows/haskell.yml: haskell-ci.dhall .github/workflows
	dhall-to-yaml --file $< --output $@
