module Archive.FFI ( Entry
                   , Error
                   , readArchiveBytes
                   , unpackToDir
                   , packFromFiles
                   , writeArchiveBytes
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

packFromFiles :: FilePath -> [FilePath] -> IO ()
packFromFiles tar fps = BSL.writeFile tar =<< FFI.packFiles fps
