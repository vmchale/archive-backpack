module System.Directory.Recursive ( getDirRecursive ) where

import           Control.Composition ((.*))
import           Control.Monad       (filterM)
import           Data.Foldable       (fold)
import           System.Directory    (doesDirectoryExist, getDirectoryContents)
import           System.FilePath     ((</>))

getDirRecursive :: FilePath -> IO [FilePath]
getDirRecursive fp = do
    all' <- exclude <$> getDirectoryContents fp
    dirs <- exclude <$> filterM doesDirectoryExist (mkRel <$> all')
    case dirs of
        [] -> pure (mkRel <$> all')
        ds -> do
            next <- foldMapA getDirRecursive ds
            pure $ next <> (mkRel <$> all')

    where foldMapA = fmap fold .* traverse
          exclude = filter (\x -> x /= "." && x /= "..")
          mkRel = (fp </>)
