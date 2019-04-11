module Archive.FFI ( Entry
                   , readArchiveBytes
                   , unpackToDir
                   , packFromFiles
                   , writeArchiveBytes
                   ) where

import qualified Codec.Archive        as FFI
import qualified Data.ByteString.Lazy as BSL

type Entry = FFI.Entry

writeArchiveBytes :: [Entry] -> BSL.ByteString
writeArchiveBytes = FFI.entriesToBSL

readArchiveBytes :: BSL.ByteString -> [Entry]
readArchiveBytes = FFI.readArchiveBSL

unpackToDir :: FilePath -> BSL.ByteString -> IO ()
unpackToDir = FFI.unpackToDirLazy

packFromFiles :: FilePath -> [FilePath] -> IO ()
packFromFiles = undefined
