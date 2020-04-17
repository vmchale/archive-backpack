.PHONY: ci

MAKEFLAGS += --warn-undefined-variables --no-builtin-rules -j
.DELETE_ON_ERROR:

setup: ghc-8.8.2-x86_64-deb9-linux.tar.gz ghc-8.8.2-x86_64-deb9-linux.tar.xz ghc-8.8.2-x86_64-deb9-linux.tar.lz ghc-8.8.2-x86_64-deb9-linux.tar.bz2 ghc-8.8.2-x86_64-deb9-linux.tar.zst ghc-8.8.2-x86_64-deb9-linux.tar.lz4

clean:
	@rm -rf tags dist-newstyle *.tar* *.svg ghc*

ci: .github/workflows/haskell.yml .github/workflows/dhall.yml .github/workflows/hlint.yml

.github/workflows:
	mkdir -p $@

.github/workflows/haskell.yml: haskell-ci.dhall .github/workflows
	dhall-to-yaml --file $< --output $@

.github/workflows/dhall.yml: dhall-ci.dhall .github/workflows
	dhall-to-yaml --file $< --output $@

.github/workflows/hlint.yml: hlint-ci.dhall .github/workflows
	dhall-to-yaml --file $< --output $@

ghc-8.8.2-x86_64-deb9-linux.tar.xz:
	wget https://downloads.haskell.org/~ghc/8.8.2/ghc-8.8.2-x86_64-deb9-linux.tar.xz -O $@

ghc-8.8.2-x86_64-deb9-linux.tar.%: ghc-8.8.2-x86_64-deb9-linux.tar.xz
	sak transcode $< $@
