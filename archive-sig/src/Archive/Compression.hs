module Archive.Compression ( Decompressor
                           , unpackFileToDirAndDecompress
                           , packFromFilesAndCompress
                           , packFromDirAndCompress
                           ) where

import           Archive
import qualified Data.ByteString.Lazy       as BSL
import           Data.Foldable              (toList)
import           System.Directory.Recursive

type Decompressor = BSL.ByteString -> BSL.ByteString
type Compressor = BSL.ByteString -> BSL.ByteString

-- | @since 0.2.0.0
unpackFileToDirAndDecompress :: Decompressor -- ^ Decompression to use
                             -> FilePath -- ^ Filepath pointing to archive
                             -> FilePath -- ^ Directory
                             -> IO ()
unpackFileToDirAndDecompress f tar dir = unpackToDir dir =<< (f <$> BSL.readFile tar)

-- | @since 0.2.0.0
packFromDirAndCompress :: Compressor -> FilePath -> FilePath -> IO ()
packFromDirAndCompress f dir tar = packFromFilesAndCompress f tar =<< fmap toList (getDirRecursive dir)

-- | @since 0.2.0.0
packFromFilesAndCompress :: Compressor -> FilePath -> [FilePath] -> IO ()
packFromFilesAndCompress f tar fps = BSL.writeFile tar =<< (f <$> packFiles fps)
