module Archive.FFI ( Entry
                   , packToFile
                   , readArchiveBytes
                   , unpackToDir
                   , packFromFiles
                   ) where

import qualified Codec.Archive        as FFI
import qualified Data.ByteString.Lazy as BSL

type Entry = FFI.Entry

packToFile :: FilePath -> [Entry] -> IO ()
packToFile = FFI.entriesToFile

readArchiveBytes :: BSL.ByteString -> [Entry]
readArchiveBytes = FFI.readArchiveBSL

unpackToDir :: FilePath -> BSL.ByteString -> IO ()
unpackToDir = FFI.unpackToDirLazy

packFromFiles :: FilePath -> [FilePath] -> IO ()
packFromFiles = undefined
