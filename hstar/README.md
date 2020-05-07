# hstar

`hstar` is a command-line tool like [tar](https://www.gnu.org/software/tar/),
but written in Haskell and taking advantage of
[backpack](http://blog.ezyang.com/category/haskell/backpack/).

It can be built with
a [libarchive](http://hackage.haskell.org/package/libarchive) backend wrapping
the C library, or the [tar](http://hackage.haskell.org/package/tar) library
written in Haskell.

- [Use](#use)
- [Features](#features)
  - [Compression](#compression)
- [Performance](#performance)

## Use

To generate bash shell completions:

```
hstar --bash-completion-script hstar
```

## Features

Unlike other tar implementations, hstar has the `sanitize` subcommand which
converts a file into a pax-compatible archive.

### Compression

hstar has near-parity with other command-line tar implementations with respect
to compression support.

| Program | lzop | lzip | lzma | gzip | bzip2 | zstd | lz4 | brotli | snappy | lrzip | grzip |
| ------- | ---- | ---- | ---- | ---- | ----- | ---- | --- | ------ | ------ | ----- | ----- |
| [bsdtar](http://libarchive.org/) | x | x | x | x | x | x | x | | | x | x |
| hstar | x | x | x | x | x | x | x | x | x | | |
| [arc](https://github.com/mholt/archiver) | | | x | x | x | x | ½ | | x | | |
| [Schily tar](http://cdrtools.sourceforge.net/private/star.html) | x | x | x | x | x | x | | | | | |
| [star](https://crates.io/crates/star) | | | x | x | x | x | | | | |
| [busybox tar](https://www.busybox.net/) | | | x | x | x | | | | | | |
| [python3 tar module](https://docs.python.org/3/library/tarfile.html#command-line-interface) | | | x | x | x | | | | | | |

## Performance

Benchmarks run on Linux.

`hstar` built with the `libarchive` backend.

| Program | Compression | Command | Time |
| ------- | ----------- | ------- | ---: |
| [ptar](https://perldoc.perl.org/ptar.html) | gzip | `ptar xf ghc-8.8.2-x86_64-deb9-linux.tar.gz` | 68.49 s |
| python3 tar | gzip | `python3 -m tarfile -e ghc-8.8.2-x86_64-deb9-linux.tar.gz` | 8.742 s |
| bsdtar | gzip | `bsdtar -xf ghc-8.8.2-x86_64-deb9-linux.tar.gz` | 4.570 s |
| hstar | gzip | `hstar unpack ghc-8.8.2-x86_64-deb9-linux.tar.gz` | 6.250 s |
| GNU tar | gzip | `tar xf ghc-8.8.2-x86_64-deb9-linux.tar.gz` | 8.335 s |
| Schily tar | gzip | `star xf ghc-8.8.2-x86_64-deb9-linux.tar.gz` | 8.349 s |
| arc | gzip | `arc -overwrite unarchive ghc-8.8.2-x86_64-deb9-linux.tar.gz` | 15.69 s |
| busybox tar | gzip | `busybox tar xf ghc-8.8.2-x86_64-deb9-linux.tar.gz` | 10.88 s |
| [htar](http://hackage.haskell.org/package/htar) | gzip | `htar -xzf ghc-8.8.2-x86_64-deb9-linux.tar.gz` | 23.94 s |
| star | gzip | `star x ghc-8.8.2-x86_64-deb9-linux.tar.gz` | 15.50 s |
| python3 tar | lzma | `python3 -m tarfile -e ghc-8.8.2-x86_64-deb9-linux.tar.xz` | 16.74 s |
| bsdtar | lzma | `bsdtar -xf ghc-8.8.2-x86_64-deb9-linux.tar.xz` | 14.81 s |
| hstar | lzma | `hstar unpack ghc-8.8.2-x86_64-deb9-linux.tar.xz` | 15.18 s |
| GNU tar | lzma | `tar xf ghc-8.8.2-x86_64-deb9-linux.tar.xz` | 14.30 s |
| Schily tar | lzma | `star xf ghc-8.8.2-x86_64-deb9-linux.tar.xz` | 14.33 s |
| arc | lzma | `arc -overwrite unarchive ghc-8.8.2-x86_64-deb9-linux.tar.xz` | 50.27 s |
| busybox tar | lzma | `busybox tar xf ghc-8.8.2-x86_64-deb9-linux.tar.xz` | 16.05 s |
| star | lzma | `star x ghc-8.8.2-x86_64-deb9-linux.tar.xz` | 15.57 s |
| python3 tar | bzip2 | `python3 -m tarfile -e ghc-8.8.2-x86_64-deb9-linux.tar.bz2` | 41.57 s |
| bsdtar | bzip2 | `bsdtar -xf ghc-8.8.2-x86_64-deb9-linux.tar.bz2` | 37.27 s |
| hstar | bzip2 | `hstar unpack ghc-8.8.2-x86_64-deb9-linux.tar.bz2` | 39.02 s |
| GNU tar | bzip2 | `tar xf ghc-8.8.2-x86_64-deb9-linux.tar.bz2` | 38.08 s |
| Schily tar | bzip2 | `star xf ghc-8.8.2-x86_64-deb9-linux.tar.bz2` | 38.05 s |
| arc | bzip2 | `arc -overwrite unarchive ghc-8.8.2-x86_64-deb9-linux.tar.bz2` | 84.67 s |
| busybox tar | bzip2 | `busybox tar xf ghc-8.8.2-x86_64-deb9-linux.tar.bz2` | 38.10 s |
| htar | bzip2 | `htar -xjf ghc-8.8.2-x86_64-deb9-linux.tar.bz2` | 49.20 s |
| bsdtar | lz4 | `bsdtar -xf ghc-8.8.2-x86_64-deb9-linux.tar.lz4` | 2.080 s |
| hstar | lz4 | `hstar unpack ghc-8.8.2-x86_64-deb9-linux.tar.lz4` | 2.364 s |
| GNU tar | lz4 | `lz4 -cd ghc-8.8.2-x86_64-deb9-linux.tar.lz4 \| tar xf -` | 2.577 s |
| Schily tar | lz4 | `lz4 -cd ghc-8.8.2-x86_64-deb9-linux.tar.lz4 \| star xf -` | 1.581 s |
| bsdtar | lzo | `bsdtar -xf ghc-8.8.2-x86_64-deb9-linux.tar.lzo` | 3.870 s |
| hstar | lzo | `hstar unpack ghc-8.8.2-x86_64-deb9-linux.tar.lzo` | 4.854 s |
| bsdtar | lzip | `bsdtar -xf ghc-8.8.2-x86_64-deb9-linux.tar.lz` | 12.35 s |
| hstar | lzip | `hstar unpack ghc-8.8.2-x86_64-deb9-linux.tar.lz` | 15.98 s |
| GNU tar | lzip | `tar xf ghc-8.8.2-x86_64-deb9-linux.tar.lz` | 16.70 s |
| Schily tar | lzip | `star xf ghc-8.8.2-x86_64-deb9-linux.tar.lz` | 15.80 s |
| busybox tar | lzip | `lzip -cd ghc-8.8.2-x86_64-deb9-linux.tar.lz \| busybox tar xf -` | 17.95 s |
| bsdtar | ztsd | `bsdtar -xf ghc-8.8.2-x86_64-deb9-linux.tar.zst` | 2.154 s |
| hstar | ztsd | `hstar unpack ghc-8.8.2-x86_64-deb9-linux.tar.zst` | 2.819 s |
| GNU tar | zstd | `tar xf ghc-8.8.2-x86_64-deb9-linux.tar.zst` | 2.512 s |
| Schily tar | zstd | `star xf ghc-8.8.2-x86_64-deb9-linux.tar.zst` | 2.095 s |
| busybox tar | zstd | `zstd -cd ghc-8.8.2-x86_64-deb9-linux.tar.zst \| busybox tar xf -` | 3.529 s |
| star | zstd | `star x ghc-8.8.2-x86_64-deb9-linux.tar.zst` | 2.990 s |
| hstar | brotli | `hstar unpack ghc-8.8.2-x86_64-deb9-linux.tar.br` | 4.060 s |
| busybox tar | brotli | `brotli -cd ghc-8.8.2-x86_64-deb9-linux.tar.br \| busybox tar xf -` | 5.270 s |
| GNU tar | brotli | `brotli -cd ghc-8.8.2-x86_64-deb9-linux.tar.br \| tar xf -` | 4.375 s |
| Schily tar | brotli | `brotli -cd ghc-8.8.2-x86_64-deb9-linux.tar.br \| star xf -` | 2.991 s |
| bsdtar | brotli | `brotli -cd ghc-8.8.2-x86_64-deb9-linux.tar.br \| bsdtar -xf -` | 4.524 s |
| bsdtar | snappy | `cat ghc-8.8.2-x86_64-deb9-linux.tar.sz \| szip -d \| bsdtar -xf -` | 2.357 s |
| hstar | snappy | `hstar unpack ghc-8.8.2-x86_64-deb9-linux.tar.sz` | 2.384 s |
| arc | snappy | `arc -overwrite unarchive ghc-8.8.2-x86_64-deb9-linux.tar.sz` | 4.371 s |
| busybox tar | snappy | `cat ghc-8.8.2-x86_64-deb9-linux.tar.sz \| szip -d \| busybox tar xf -` | 2.729 s |
| GNU tar | snappy | `cat ghc-8.8.2-x86_64-deb9-linux.tar.sz \| szip -d \| tar xf -` | 2.188 s |
| Schily tar | snappy | `cat ghc-8.8.2-x86_64-deb9-linux.tar.sz \| szip -d \| star xf -` | 2.305 s |
