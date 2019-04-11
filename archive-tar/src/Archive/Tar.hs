module Archive.Tar ( Entry
                   , packToFile
                   , unpackFromFile
                   , unpackToDir
                   , unpackFileToDir
                   , readArchiveBytes
                   , packFromFiles
                   ) where

import           Codec.Archive.Tar    (Entries (..))
import qualified Codec.Archive.Tar    as Tar
import           Control.Composition  ((.@))
import           Control.Exception    (Exception, throw)
import qualified Data.ByteString.Lazy as BSL

type Entry = Tar.Entry

-- this is bad but libarchive's error handling is vaguely fucked
coerceToList :: Exception a => Entries a -> [Entry]
coerceToList (Next e es) = e : coerceToList es
coerceToList Done        = []
coerceToList (Fail ex)   = throw ex

packToFile :: FilePath -> [Entry] -> IO ()
packToFile = Tar.write .@ BSL.writeFile

unpackFileToDir :: FilePath -> FilePath -> IO ()
unpackFileToDir = Tar.extract

unpackFromFile :: FilePath -> IO [Entry]
unpackFromFile = fmap readArchiveBytes . BSL.readFile

readArchiveBytes :: BSL.ByteString -> [Entry]
readArchiveBytes = coerceToList . Tar.read

unpackToDir :: FilePath -> BSL.ByteString -> IO ()
unpackToDir = Tar.read .@ Tar.unpack

packFromFiles :: FilePath -> [FilePath] -> IO ()
packFromFiles arc = Tar.create arc "."
