module Archive.Generic ( packFromDir ) where

import           Archive
import           Control.Composition ((.*))
import           Control.Monad       (filterM)
import           Data.DList          (DList, fromList)
import           Data.Foldable       (fold, toList)
import           System.Directory    (doesDirectoryExist, getDirectoryContents)

packFromDir :: FilePath -- ^ Directory to be packed up
            -> FilePath -- ^ @.tar@ archive file
            -> IO ()
packFromDir dir tar = packFromFiles tar =<< fmap toList (getDirRecursive dir)

getDirRecursive :: FilePath -> IO (DList FilePath)
getDirRecursive fp = do
    all' <- getDirectoryContents fp
    dirs <- filterM doesDirectoryExist all'
    case dirs of
        [] -> pure $ fromList all'
        ds -> do
            next <- foldMapA getDirRecursive ds
            pure $ next <> fromList all'

    where foldMapA = fmap fold .* traverse
