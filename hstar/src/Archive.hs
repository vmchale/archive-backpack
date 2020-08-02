module Archive ( unpackToDir
               , packFromDirAndCompress
               , packFromFilesAndCompress
               , unpackFileToDirAndDecompress
               , packSrcDirAndCompress
               ) where

import           Codec.Archive              (packFiles, throwArchiveM, unpackToDirLazy)
import           Control.Composition        ((.*))
import qualified Data.ByteString.Lazy       as BSL
import           Data.List                  (isSuffixOf)
import           System.Directory.Recursive (getDirFiltered, getDirRecursive)

type Decompressor = BSL.ByteString -> BSL.ByteString
type Compressor = BSL.ByteString -> BSL.ByteString

unpackToDir :: FilePath -> BSL.ByteString -> IO ()
unpackToDir = throwArchiveM .* unpackToDirLazy

packFromDirAndCompress :: Compressor
                       -> FilePath -- ^ Directory to pack up
                       -> FilePath -- ^ Destination tarball
                       -> IO ()
packFromDirAndCompress f dir tar = packFromFilesAndCompress f tar =<< getDirRecursive dir

packFromFilesAndCompress :: Compressor -> FilePath -> [FilePath] -> IO ()
packFromFilesAndCompress f tar fps = BSL.writeFile tar =<< (f <$> packFiles fps)

unpackFileToDirAndDecompress :: Decompressor -- ^ Decompression to use
                             -> FilePath -- ^ Filepath pointing to archive
                             -> FilePath -- ^ Directory
                             -> IO ()
unpackFileToDirAndDecompress f tar dir = unpackToDir dir =<< (f <$> BSL.readFile tar)

packSrcDirAndCompress :: Compressor -> FilePath -> FilePath -> IO ()
packSrcDirAndCompress f dir tar = packFromFilesAndCompress f tar =<< getDirFiltered (pure.srcFilter) dir

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

