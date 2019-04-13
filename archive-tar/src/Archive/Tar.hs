module Archive.Tar ( Entry
                   , Error
                   , writeArchiveBytes
                   , unpackToDir
                   , readArchiveBytes
                   , packFromFiles
                   ) where

import           Codec.Archive.Tar    (Entries (..))
import qualified Codec.Archive.Tar    as Tar
import           Control.Composition  ((.@))
import qualified Data.ByteString.Lazy as BSL

type Entry = Tar.Entry

type Error = Tar.FormatError

-- this is bad but libarchive's error handling is vaguely fucked
coerceToList :: Entries a -> Either a [Entry]
coerceToList (Next e es) = (e :) <$> coerceToList es
coerceToList Done        = Right []
coerceToList (Fail ex)   = Left ex

writeArchiveBytes :: [Entry] -> BSL.ByteString
writeArchiveBytes = Tar.write

readArchiveBytes :: BSL.ByteString -> Either Error [Entry]
readArchiveBytes = coerceToList . Tar.read

unpackToDir :: FilePath -> BSL.ByteString -> IO ()
unpackToDir = Tar.read .@ Tar.unpack

packFromFiles :: FilePath -> [FilePath] -> IO ()
packFromFiles arc = Tar.create arc "."
