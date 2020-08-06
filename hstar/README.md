# hstar

`hstar` is a command-line tool like [tar](https://www.gnu.org/software/tar/),
with several more features.

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
converts a file into a pax-compatible archive and the `lint` subcommand which
points out suspicious archives.

`hstar` can change the compression based on command-line flags, like `bsdtar`
and `arc` (this feature is missing from GNU tar, busybox tar, and seemingly Schily tar).

### Compression

hstar has near-parity with other command-line tar implementations with respect
to compression support.

| Program | lzop | lzip | lzma | gzip | bzip2 | zstd | lz4 | brotli | snappy | lrzip | grzip |
| ------- | ---- | ---- | ---- | ---- | ----- | ---- | --- | ------ | ------ | ----- | ----- |
| [bsdtar](http://libarchive.org/) | x | x | x | x | x | x | x | | | x | x |
| hstar | x | x | x | x | x | x | x | x | x | ½ | |
| [arc](https://github.com/mholt/archiver) | | | x | x | x | x | ½ | | x | | |
| [Schily tar](http://cdrtools.sourceforge.net/private/star.html) | x | x | x | x | x | x | | | | | |
| [busybox tar](https://www.busybox.net/) | | | x | x | x | | | | | | |
| [python3 tar module](https://docs.python.org/3/library/tarfile.html#command-line-interface) | | | x | x | x | | | | | | |
| [GNU tar](https://www.gnu.org/software/tar/) | x | x | x | x | x | x | | | | | |

## Performance

Benchmarks run on Linux. Performance should be near GNU tar, bsdtar, or Schily tar.

| Program | Compression | Command | Time |
| ------- | ----------- | ------- | ---: |
| bsdtar | zstd | `bsdtar -xf sparc64-linux-dist.tar.zst` | 61.28 ms |
| hstar | zstd | `hstar unpack sparc64-linux-dist.tar.zst` | 69.63 ms |
| GNU tar | zstd | `tar xf sparc64-linux-dist.tar.zst` | 303.8 ms |
| Schily tar | zstd | `star xf sparc64-linux-dist.tar.zst` | 291.0 ms |
| bsdtar | lzip | `bsdtar -xf sparc64-linux-dist.tar.lz` | 345.3 ms |
| hstar | lzip | `hstar unpack sparc64-linux-dist.tar.lz` | 433.2 ms |
| GNU tar | lzip | `tar xf sparc64-linux-dist.tar.lz` | 456.8 ms |
| Schily tar | lzip | `star xf sparc64-linux-dist.tar.lz` | 440.0 ms | 
| busybox tar | lzip | `lzip -cd sparc64-linux-dist.tar.lz \| busybox tar xf -` | 481.0 ms |
| busybox tar | zstd | `zstd -cd sparc64-linux-dist.tar.zst \| busybox tar xf -` | 319.0 ms |
| python3 tar | gzip | `python3 -m tarfile -e sparc64-linux-dist.tar.gz` | 245.3 ms |
| bsdtar | gzip | `bsdtar -xf sparc64-linux-dist.tar.gz` | 122.8 ms |
| hstar | gzip | `hstar unpack sparc64-linux-dist.tar.gz` | 172.4 ms |
| GNU tar | gzip | `tar xf sparc64-linux-dist.tar.gz` | 233.9 ms |
| Schily tar | gzip | `star xf sparc64-linux-dist.tar.gz` | 234.8 ms |
| arc | gzip | `arc -overwrite unarchive sparc64-linux-dist.tar.gz` | 362.0 ms |
| python3 tar | lzma | `python3 -m tarfile -e sparc64-linux-dist.tar.xz` | 414.9 ms |
| bsdtar | lzma | `bsdtar -xf sparc64-linux-dist.tar.xz` | 349.4 ms |
| hstar | lzma | `hstar unpack sparc64-linux-dist.tar.xz` | 360.6 ms |
| GNU tar | lzma | `tar xf sparc64-linux-dist.tar.xz` | 364.3 ms |
| Schily tar | lzma | `star xf sparc64-linux-dist.tar.xz` | 353.6 ms |
| arc | lzma | `arc -overwrite unarchive sparc64-linux-dist.tar.xz` | 1.191 s |
| python3 tar | bzip2 | `python3 -m tarfile -e sparc64-linux-dist.tar.bz2` | 1.242 s |
| bsdtar | bzip2 | `bsdtar -xf sparc64-linux-dist.tar.bz2` | 1.149 s |
| hstar | bzip2 | `hstar unpack sparc64-linux-dist.tar.bz2` | 1.136 s |
| GNU tar | bzip2 | `tar xf sparc64-linux-dist.tar.bz2` | 1.092 s |
| Schily tar | bzip2 | `star xf sparc64-linux-dist.tar.bz2` | 1.103 s |
| arc | bzip2 | `arc -overwrite unarchive sparc64-linux-dist.tar.bz2` | 2.544 s |
| busybox tar | gzip | `busybox tar xzf sparc64-linux-dist.tar.gz` | 307.5 ms |
| busybox tar | bzip2 | `busybox tar xjf sparc64-linux-dist.tar.bz2` | 1.018 s |
| busybox tar | lzma | `busybox tar xJf sparc64-linux-dist.tar.xz` | 385.6 ms |
| bsdtar | lz4 | `bsdtar -xf sparc64-linux-dist.tar.lz4` | 40.69 ms |
| hstar | lz4 | `hstar unpack sparc64-linux-dist.tar.lz4` | 49.87 ms |
| busybox tar | lz4 | `lz4 -cd sparc64-linux-dist.tar.lz4 \| busybox tar xf -` | 56.00 ms |
| GNU tar | lz4 | `lz4 -cd sparc64-linux-dist.tar.lz4 \| tar xf -` | 39.19 ms |
| Schily tar | lz4 | `lz4 -cd sparc64-linux-dist.tar.lz4 \| star xf -` | 30.88 ms |
