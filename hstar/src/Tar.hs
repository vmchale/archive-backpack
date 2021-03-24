module Tar ( unpackToDir
           , packFromDirAndCompress
           , packFromFilesAndCompress
           , unpackFileToDirAndDecompress
           , packSrcDirAndCompress
           ) where

import           Codec.Archive              (packFiles, packFiles7zip, packFilesCpio, packFilesShar, packFilesZip, throwArchiveM, unpackToDirLazy)
import           Compression.Level          (CompressionLevel, compressor)
import           Compression.Type           (Archive (..))
import           Control.Composition        ((.*))
import qualified Data.ByteString.Lazy       as BSL
import           Data.List                  (isSuffixOf)
import           System.Directory.Recursive (getDirFiltered, getDirRecursive)

type Decompressor = BSL.ByteString -> BSL.ByteString

unpackToDir :: FilePath -> BSL.ByteString -> IO ()
unpackToDir = throwArchiveM .* unpackToDirLazy

packFromDirAndCompress :: Archive
                       -> CompressionLevel
                       -> FilePath -- ^ Directory to pack up
                       -> FilePath -- ^ Destination tarball
                       -> IO ()
packFromDirAndCompress a lvl dir tar = packFromFilesAndCompress a lvl tar =<< getDirRecursive dir

packFromFilesAndCompress :: Archive -> CompressionLevel -> FilePath -> [FilePath] -> IO ()
packFromFilesAndCompress (Tar c) lvl tar fps  = BSL.writeFile tar . compressor c lvl =<< packFiles fps
packFromFilesAndCompress SevenZip _ tar fps   = BSL.writeFile tar =<< packFiles7zip fps
packFromFilesAndCompress (Cpio c) lvl tar fps = BSL.writeFile tar . compressor c lvl =<< packFilesCpio fps
packFromFilesAndCompress (Shar c) lvl tar fps = BSL.writeFile tar . compressor c lvl =<< packFilesShar fps
packFromFilesAndCompress Zip _ tar fps        = BSL.writeFile tar =<< packFilesZip fps

unpackFileToDirAndDecompress :: Decompressor -- ^ Decompression to use
                             -> FilePath -- ^ Filepath pointing to archive
                             -> FilePath -- ^ Directory
                             -> IO ()
unpackFileToDirAndDecompress f tar dir = unpackToDir dir . f =<< BSL.readFile tar

packSrcDirAndCompress :: Archive -> CompressionLevel -> FilePath -> FilePath -> IO ()
packSrcDirAndCompress a lvl dir tar = packFromFilesAndCompress a lvl tar =<< getDirFiltered (pure.srcFilter) dir

srcFilter :: FilePath -> Bool
srcFilter fp | ".git" `isSuffixOf` fp = False
             | "_darcs" `isSuffixOf` fp = False
             | ".hg" `isSuffixOf` fp = False
             | ".pijul" `isSuffixOf` fp = False
             | "dist" `isSuffixOf` fp = False
             | "dist-newstyle" `isSuffixOf` fp = False
             | ".stack-work" `isSuffixOf` fp = False
             | "target" `isSuffixOf` fp = False
             | ".atspkg" `isSuffixOf` fp = False
             | ".shake" `isSuffixOf` fp = False
             | ".vagrant" `isSuffixOf` fp = False
             | "tags" `isSuffixOf` fp = False
             | "hspec-failures" `isSuffixOf` fp = False
             | ".github" `isSuffixOf` fp = False
             | ".travis.yml" `isSuffixOf` fp = False
             | "TODO.md" `isSuffixOf` fp = False
             | ".yamllint" `isSuffixOf` fp = False
             | ".ctags" `isSuffixOf` fp = False
             | ".atsfmt.toml" `isSuffixOf` fp = False
             | ".gitignore" `isSuffixOf` fp = False
             | ".clang-format" `isSuffixOf` fp = False
             | "stack.yaml.lock" `isSuffixOf` fp = False
             | "appveyor.yml" `isSuffixOf` fp = False
             | ".terraform" `isSuffixOf` fp = False
             | otherwise = True

