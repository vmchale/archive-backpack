module Archive.Compression ( Decompressor
                           , unpackFileToDirAndDecompress
                           ) where

import           Archive
import qualified Data.ByteString.Lazy as BSL

type Decompressor = BSL.ByteString -> BSL.ByteString

-- | @since 0.2.0.0
unpackFileToDirAndDecompress :: Decompressor -- ^ Decompression to use
                             -> FilePath -- ^ Filepath pointing to archive
                             -> FilePath -- ^ Directory
                             -> IO ()
unpackFileToDirAndDecompress f tar dir = unpackToDir dir =<< (f <$> BSL.readFile tar)
