# 1.1.0.0

  * Require `Show` instance for `Entry`
  * Add `lintEntry`

# 1.0.2.0

  * Re-export `unpackToDir` and `packFromFiles` from `Archive.Generic`

# 1.0.1.0

  * Export `unpackToDir` and `packFromFiles` from `Archive.Compression`

# 1.0.0.0

  * Add `FP` type to allow the filepath type to be abstract
  * Add `toFP`, `packFromFilesRaw`, `unpackToDirRaw`

# 0.2.3.0

  * Add `archiveSigVersion` function

# 0.2.2.0

  * Add `versionInfo` to `Archive` signature

# 0.2.1.2

  * Bugfix

# 0.2.1.1

  * Improved documentation

# 0.2.1.0

  * Add `packSrcDirAndCompress`

# 0.2.0.2

  * Export `Compressor` type for docs

# 0.2.0.1

  * Use `dir-traverse` to provide recursive directory traversal

# 0.2.0.0

  * Add `unpackFileToDirAndDecompress`
  * Move `packFromFiles` to `Archive.Generic` and add `packFiles`
  * Remove `dlist` dependency
