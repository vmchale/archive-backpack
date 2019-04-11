module Archive.Tar ( Entry
                   , writeArchiveBytes
                   , unpackToDir
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

writeArchiveBytes :: [Entry] -> BSL.ByteString
writeArchiveBytes = Tar.write

readArchiveBytes :: BSL.ByteString -> [Entry]
readArchiveBytes = coerceToList . Tar.read

unpackToDir :: FilePath -> BSL.ByteString -> IO ()
unpackToDir = Tar.read .@ Tar.unpack

packFromFiles :: FilePath -> [FilePath] -> IO ()
packFromFiles arc = Tar.create arc "."
