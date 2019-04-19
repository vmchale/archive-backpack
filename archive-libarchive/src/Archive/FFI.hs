module Archive.FFI ( Entry
                   , Error
                   , readArchiveBytes
                   , unpackToDir
                   , writeArchiveBytes
                   , packFiles
                   ) where

import qualified Codec.Archive        as FFI
import           Control.Composition  ((.*))
import qualified Data.ByteString.Lazy as BSL

type Entry = FFI.Entry

type Error = FFI.ArchiveResult

writeArchiveBytes :: [Entry] -> BSL.ByteString
writeArchiveBytes = FFI.entriesToBSL

readArchiveBytes :: BSL.ByteString -> Either Error [Entry]
readArchiveBytes = FFI.readArchiveBSL

unpackToDir :: FilePath -> BSL.ByteString -> IO ()
unpackToDir = fmap (either (error . show) id) .* FFI.runArchiveM .* FFI.unpackToDirLazy

packFiles :: [FilePath] -> IO BSL.ByteString
packFiles = FFI.packFiles
