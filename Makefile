.PHONY: ci install docs

MAKEFLAGS += --warn-undefined-variables --no-builtin-rules -j
.DELETE_ON_ERROR:

setup: ghc-8.8.2-x86_64-deb9-linux.tar.gz ghc-8.8.2-x86_64-deb9-linux.tar.xz ghc-8.8.2-x86_64-deb9-linux.tar.lz ghc-8.8.2-x86_64-deb9-linux.tar.bz2 ghc-8.8.2-x86_64-deb9-linux.tar.zst ghc-8.8.2-x86_64-deb9-linux.tar.lz4 ghc-8.8.2-x86_64-deb9-linux.tar.br ghc-8.8.2-x86_64-deb9-linux.tar.sz ghc-8.8.2-x86_64-deb9-linux.tar.lzo ghc-8.8.2-x86_64-deb9-linux.tar.lrz

install: hstar/man/hstar.1
	cabal install exe:hstar -w ghc-8.10.1 --overwrite-policy=always --constraint='libarchive +static'
	cp hstar/man/hstar.1 $$HOME/.local/share/man/man1

docs: hstar/man/hstar.1

hstar/man/hstar.1: hstar/man/MANPAGE.md
	pandoc $< -s -t man -o $@


clean:
	@rm -rf tags dist-newstyle *.tar* *.svg ghc* llvm*

ghc-8.8.2-x86_64-deb9-linux.tar.xz:
	wget https://downloads.haskell.org/~ghc/8.8.2/ghc-8.8.2-x86_64-deb9-linux.tar.xz -O $@

ghc-8.8.2-x86_64-deb9-linux.tar: ghc-8.8.2-x86_64-deb9-linux.tar.xz
	sak decompress $^

ghc-8.8.2-x86_64-deb9-linux.tar.lrz: ghc-8.8.2-x86_64-deb9-linux.tar
	lrzip $< -f

ghc-8.8.2-x86_64-deb9-linux.tar.%: ghc-8.8.2-x86_64-deb9-linux.tar.xz
	sak transcode $< $@

llvm-9.0.0.src.tar.xz:
	wget http://releases.llvm.org/9.0.0/llvm-9.0.0.src.tar.xz -O $@

ghc-8.0.2-x86_64-deb8-linux.tar.xz:
	wget https://downloads.haskell.org/~ghc/8.0.2/ghc-8.0.2-x86_64-deb8-linux.tar.xz -O $@

sparc64-linux-dist.tar.gz:
	wget https://github.com/vmchale/dickinson/releases/download/1.1.0.1/sparc64-linux-dist.tar.gz -O $@

sparc64-linux-dist.tar.%: sparc64-linux-dist.tar.gz
	sak transcode $^ $@

llvm-9.0.0.src.tar.%: llvm-9.0.0.src.tar.xz
	sak transcode $^ $@
