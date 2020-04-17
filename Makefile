.PHONY: ci

MAKEFLAGS += --warn-undefined-variables --no-builtin-rules -j
.DELETE_ON_ERROR:

setup: llvm-9.0.0.src.tar.lz llvm-9.0.0.src.tar.xz llvm-9.0.0.src.tar.bz2 llvm-9.0.0.src.tar.gz llvm-9.0.0.src.tar.lz4 llvm-9.0.0.src.tar.zst


clean:
	@rm -rf tags dist-newstyle *.tar* *.svg ghc* llvm*

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

llvm-9.0.0.src.tar.xz:
	wget http://releases.llvm.org/9.0.0/llvm-9.0.0.src.tar.xz -O $@

llvm-9.0.0.src.tar.%: llvm-9.0.0.src.tar.xz
	sak transcode $^ $@
