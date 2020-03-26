# hstar

`hstar` is a command-line tool like [tar](https://www.gnu.org/software/tar/),
but written in Haskell and taking advantage of
[backpack](http://blog.ezyang.com/category/haskell/backpack/).

It can be built with
a [libarchive](http://hackage.haskell.org/package/libarchive) backend wrapping
the C library, or the [tar](http://hackage.haskell.org/package/tar) library
written in Haskell.

## Performance

Benchmarks run on Linux.

`hstar` built with the `libarchive` backend.

| Program | Compression | Command | Time |
| ------- | ----------- | ------- | ---: |
| bsdtar | lz4 | `bsdtar -xf ghc-8.8.2-x86_64-deb9-linux.tar.lz4` | 2.771 s |
| hstar | lz4 | `hstar unpack ghc-8.8.2-x86_64-deb9-linux.tar.lz4` | 2.780 s |
| GNU tar | lz4 | `lz4 -cd ghc-8.8.2-x86_64-deb9-linux.tar.lz4 \| tar -xf -` | 2.842 s |
| bsdtar | gzip | `bsdtar -xf ghc-8.8.2-x86_64-deb9-linux.tar.gz` | 4.862 s |
| hstar | gzip | `hstar unpack ghc-8.8.2-x86_64-deb9-linux.tar.gz` | 6.756 s |
| GNU tar | gzip | `tar xf ghc-8.8.2-x86_64-deb9-linux.tar.gz` | 8.501 s |
| bsdtar | lzip | `bsdtar -xf ghc-8.8.2-x86_64-deb9-linux.tar.lz` | 13.15 s |
| hstar | lzip | `hstar unpack ghc-8.8.2-x86_64-deb9-linux.tar.lz` | 16.46 s |
| GNU tar | lzip | `tar xf ghc-8.8.2-x86_64-deb9-linux.tar.lz` | 17.67 s |
| bsdtar | lzma | `bsdtar -xf ghc-8.8.2-x86_64-deb9-linux.tar.xz` | 15.34 s |
| hstar | lzma | `hstar unpack ghc-8.8.2-x86_64-deb9-linux.tar.xz` | 15.97 s |
| GNU tar | lzma | `tar xf ghc-8.8.2-x86_64-deb9-linux.tar.xz` | 14.92 s |
| bsdtar | bzip2 | `bsdtar -xf ghc-8.8.2-x86_64-deb9-linux.tar.bz2` | 37.81 s |
| hstar | bzip2 | `hstar unpack ghc-8.8.2-x86_64-deb9-linux.tar.bz2` | 39.27 s |
| GNU tar | bzip2 | `tar xf ghc-8.8.2-x86_64-deb9-linux.tar.bz2` | 38.28 s |
| bsdtar | zstd | `bsdtar -xf ghc-8.8.2-x86_64-deb9-linux.tar.zst` | 2.527 s |
| hstar | zstd | `hstar unpack ghc-8.8.2-x86_64-deb9-linux.tar.zst` | 3.354 s |
| GNU tar | zstd | `tar xf ghc-8.8.2-x86_64-deb9-linux.tar.zst` | 2.924 s |
