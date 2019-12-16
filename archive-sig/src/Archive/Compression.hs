module Archive.Compression ( Decompressor
                           , Compressor
                           , unpackFileToDirAndDecompress
                           , packFromFilesAndCompress
                           , packFromDirAndCompress
                           , packSrcDirAndCompress
                           ) where

import           Archive
import qualified Data.ByteString.Lazy       as BSL
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
packFromDirAndCompress f dir tar = packFromFilesAndCompress f tar =<< getDirRecursive dir

-- | Pack up source files, ignoring version control directories and common
-- artifact directories
--
-- @since 0.2.1.0
packSrcDirAndCompress :: Compressor -> FilePath -> FilePath -> IO ()
packSrcDirAndCompress f dir tar = packFromFilesAndCompress f tar =<< getDirFiltered (pure.srcFilter) dir

srcFilter :: FilePath -> Bool
srcFilter ".git"          = False
srcFilter "_dargs"        = False
srcFilter ".hg"           = False
srcFilter ".pijul"        = False
srcFilter "dist"          = False
srcFilter "dist-newstyle" = False
srcFilter ".stack-work"   = False
srcFilter "target"        = False -- rust/cargo
srcFilter ".atspkg"       = False
srcFilter ".shake"        = False
srcFilter ".vagrant"      = False
srcFilter _               = False

-- | @since 0.2.0.0
-- | @since 0.2.0.0
packFromFilesAndCompress :: Compressor -> FilePath -> [FilePath] -> IO ()
packFromFilesAndCompress f tar fps = BSL.writeFile tar =<< (f <$> packFiles fps)
