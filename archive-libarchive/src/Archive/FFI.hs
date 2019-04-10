module Archive.FFI ( Entry
                   , packToFile
                   , unpackFromFile
                   , readArchiveBytes
                   , unpackToDir
                   , unpackFileToDir
                   ) where

import qualified Codec.Archive        as FFI
import qualified Data.ByteString.Lazy as BSL

type Entry = FFI.Entry

packToFile :: Foldable t => FilePath -> t Entry -> IO ()
packToFile = FFI.entriesToFile

unpackFromFile :: FilePath -> IO [Entry]
unpackFromFile = FFI.readArchiveFile

readArchiveBytes :: BSL.ByteString -> [Entry]
readArchiveBytes = FFI.readArchiveBSL

unpackToDir :: FilePath -> BSL.ByteString -> IO ()
unpackToDir = FFI.unpackToDirLazy

unpackFileToDir :: FilePath -> FilePath -> IO ()
unpackFileToDir = FFI.unpackArchive
