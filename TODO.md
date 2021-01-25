- [ ] http://hackage.haskell.org/package/ztar-1.0.1/docs/Codec-Archive-Tar.html
- [ ] cpio: https://en.wikipedia.org/wiki/Cpio
- [ ] http://hackage.haskell.org/package/tar-conduit-0.3.2/docs/Data-Conduit-Tar.html
- [ ] http://hackage.haskell.org/package/pure-zlib
- [ ] remote downloads?
# Features
- [x] Abstract fptype so tar-bytestring is smarter
- [x] snappy (`.sz`)
  - [x] szip!
- [ ] Check that an archive is POSIX-compatible
- [ ] Linter? check that an archive doesn't have hardlinks to itself...
  - [ ] Sanitize by reordering tar archive
  - [ ] all executable with sdists?
  - [ ] Warn when symlink points to file that hasn't been unpacked yet
  - [ ]
  ```
  star: Unknown extended header keyword 'GNU.sparse.major' ignored at -1.
  star: Unknown extended header keyword 'GNU.sparse.minor' ignored at -1.
  star: Unknown extended header keyword 'GNU.sparse.name' ignored at -1.
  star: Unknown extended header keyword 'GNU.sparse.realsize' ignored at -1.
  x 8052640 -rwxr-xr-x  vanessa/vanessa Aug 25 17:16 2020 language-dickinson-1.3.0.1/bin/GNUSparseFile.0/emd
  ```
- [ ] `inspect` subcommand?
# Upstream
- [ ] libarchive-hs segfaults when trying to check/verify archives
- [ ] Report Schily tar feature request (linter)
# Compare
- [ ] IBM tar/pax https://www.ibm.com/support/knowledgecenter/en/ssw_aix_72/p_commands/pax.html
- [ ] ztar
- [x] ptar
- [x] minitar
- [x] busybox tar
- [x] python tar
- [x] haskell tar
- [x] rust tar
  - [ ] https://rust-lang-nursery.github.io/rust-cookbook/compression/tar.html
- [x] go tar ?
  - [x] https://github.com/mholt/archiver
- [ ] tarlz
- [ ] lrzip http://ck.kolivas.org/apps/lrzip/
- [ ] https://www.peazip.org/brotli-compression-utility.html
- [ ] https://directory.fsf.org/wiki/Libtar
- [ ] jtar (tar.ijs)
- [ ] paxmirabilis https://www.mirbsd.org/pax.htm
