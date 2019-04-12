module Archive.Generic ( packFromDir
                       , unpackFileToDir
                       , unpackFromFile
                       , packToFile
                       ) where

import           Archive
import           Control.Composition  ((.*), (.@))
import           Control.Monad        (filterM)
import qualified Data.ByteString.Lazy as BSL
import           Data.DList           (DList, fromList)
import           Data.Foldable        (fold, toList)
import           System.Directory     (doesDirectoryExist, getDirectoryContents)

packToFile :: FilePath -> [Entry] -> IO ()
packToFile = writeArchiveBytes .@ BSL.writeFile

unpackFromFile :: FilePath -> IO [Entry]
unpackFromFile = fmap readArchiveBytes . BSL.readFile

unpackFileToDir :: FilePath -- ^ Filepath pointing to archive
                -> FilePath -- ^ Directory
                -> IO ()
unpackFileToDir tar dir = unpackToDir dir =<< BSL.readFile tar

packFromDir :: FilePath -- ^ Directory to be packed up
            -> FilePath -- ^ @.tar@ archive file
            -> IO ()
packFromDir dir tar = packFromFiles tar =<< fmap toList (getDirRecursive dir)

getDirRecursive :: FilePath -> IO (DList FilePath)
getDirRecursive fp = do
    all' <- getDirectoryContents fp
    dirs <- filter (\x -> x /= "." && x /= "..") <$> filterM doesDirectoryExist all'
    case dirs of
        [] -> pure $ fromList all'
        ds -> do
            next <- foldMapA getDirRecursive ds
            pure $ next <> fromList all'

    where foldMapA = fmap fold .* traverse
