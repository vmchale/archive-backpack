module Archive.Generic ( packFromDir
                       , unpackFileToDir
                       , unpackFromFile
                       , packToFile
                       , packFromFiles
                       ) where

import           Archive
import           Archive.Compression
import           Control.Composition  ((.@))
import qualified Data.ByteString.Lazy as BSL

packFromFiles :: FilePath -- ^ Path of @.tar@ file to write
              -> [FilePath] -- ^ Files and directories to archive
              -> IO ()
packFromFiles = packFromFilesAndCompress id

packToFile :: FilePath -> [Entry] -> IO ()
packToFile = writeArchiveBytes .@ BSL.writeFile

unpackFromFile :: FilePath -> IO [Entry]
unpackFromFile = fmap (either (error . show) id . readArchiveBytes) . BSL.readFile

unpackFileToDir :: FilePath -- ^ Filepath pointing to archive
                -> FilePath -- ^ Directory
                -> IO ()
unpackFileToDir = unpackFileToDirAndDecompress id

packFromDir :: FilePath -- ^ Directory to be packed up
            -> FilePath -- ^ @.tar@ archive file
            -> IO ()
packFromDir = packFromDirAndCompress id
