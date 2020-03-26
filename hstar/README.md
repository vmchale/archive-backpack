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
| bsdtar | lz4 | `bsdtar -xf ghc-8.8.2-x86_64-deb9-linux.tar.lz4` | 4.697 s |
