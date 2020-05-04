# hstar

`hstar` is a command-line tool like [tar](https://www.gnu.org/software/tar/),
but written in Haskell and taking advantage of
[backpack](http://blog.ezyang.com/category/haskell/backpack/).

It can be built with
a [libarchive](http://hackage.haskell.org/package/libarchive) backend wrapping
the C library, or the [tar](http://hackage.haskell.org/package/tar) library
written in Haskell.

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

| Program | lzop | lzip | lzma | gzip | bzip2 | zstd | lz4 | brotli | snappy | lrzip |
| ------- | ---- | ---- | ---- | ---- | ----- | ---- | --- | ------ | ------ | ----- |
| [bsdtar](http://libarchive.org/) | x | x | x | x | x | x | x | | | x |
| hstar | x | x | x | x | x | x | x | x | x | |
| [arc](https://github.com/mholt/archiver) | | | x | x | x | x | x | | x | |
| [Schily tar](http://cdrtools.sourceforge.net/private/star.html) | x | x | x | x | x | x | | | | |
| [star](https://crates.io/crates/star) | | | x | x | x | x | | | |
| [busybox tar](https://www.busybox.net/) | | | x | x | x | | | | | |
| [python3 tar module](https://docs.python.org/3/library/tarfile.html#command-line-interface) | | | x | x | x | | | | | |

## Performance

Benchmarks run on Linux.

`hstar` built with the `libarchive` backend.

| Program | Compression | Command | Time |
| ------- | ----------- | ------- | ---: |
| bsdtar | lz4 | `bsdtar -xf ghc-8.8.2-x86_64-deb9-linux.tar.lz4` | 2.411 s |
| hstar | lz4 | `hstar unpack ghc-8.8.2-x86_64-deb9-linux.tar.lz4` | 2.529 s |
| GNU tar | lz4 | `lz4 -cd ghc-8.8.2-x86_64-deb9-linux.tar.lz4 \| tar -xf -` | 2.539 s |
| Schily tar | lz4 | `lz4 -cd ghc-8.8.2-x86_64-deb9-linux.tar.lz4 \| star -xf -` | 1.572 s |
| bsdtar | gzip | `bsdtar -xf ghc-8.8.2-x86_64-deb9-linux.tar.gz` | 4.625 s |
| hstar | gzip | `hstar unpack ghc-8.8.2-x86_64-deb9-linux.tar.gz` | 6.378 s |
| GNU tar | gzip | `tar xf ghc-8.8.2-x86_64-deb9-linux.tar.gz` | 8.335 s |
| Schily tar | gzip | `star xf ghc-8.8.2-x86_64-deb9-linux.tar.gz` | 8.379 s |
| bsdtar | lzip | `bsdtar -xf ghc-8.8.2-x86_64-deb9-linux.tar.lz` | 12.49 s |
| hstar | lzip | `hstar unpack ghc-8.8.2-x86_64-deb9-linux.tar.lz` | 16.12 s |
| GNU tar | lzip | `tar xf ghc-8.8.2-x86_64-deb9-linux.tar.lz` | 16.71 s |
| Schily tar | lzip | `star xf ghc-8.8.2-x86_64-deb9-linux.tar.lz` | 15.81 s |
| bsdtar | lzma | `bsdtar -xf ghc-8.8.2-x86_64-deb9-linux.tar.xz` | 14.95 s |
| hstar | lzma | `hstar unpack ghc-8.8.2-x86_64-deb9-linux.tar.xz` | 15.37 s |
| GNU tar | lzma | `tar xf ghc-8.8.2-x86_64-deb9-linux.tar.xz` | 14.32 s |
| Schily tar | lzma | `star xf ghc-8.8.2-x86_64-deb9-linux.tar.xz` | 14.37 s |
| bsdtar | bzip2 | `bsdtar -xf ghc-8.8.2-x86_64-deb9-linux.tar.bz2` | 37.85 s |
| hstar | bzip2 | `hstar unpack ghc-8.8.2-x86_64-deb9-linux.tar.bz2` | 38.99 s |
| GNU tar | bzip2 | `tar xf ghc-8.8.2-x86_64-deb9-linux.tar.bz2` | 37.90 s |
| Schily tar | bzip2 | `star xf ghc-8.8.2-x86_64-deb9-linux.tar.bz2` | 38.27 s |
| bsdtar | zstd | `bsdtar -xf ghc-8.8.2-x86_64-deb9-linux.tar.zst` | 2.232 s |
| hstar | zstd | `hstar unpack ghc-8.8.2-x86_64-deb9-linux.tar.zst` | 3.053 s |
| GNU tar | zstd | `tar xf ghc-8.8.2-x86_64-deb9-linux.tar.zst` | 2.644 s |
| Schily tar | zstd | `star xf ghc-8.8.2-x86_64-deb9-linux.tar.zst` | 2.237 s |
