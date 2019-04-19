module Archive.Generic ( packFromDir
                       , unpackFileToDir
                       , unpackFromFile
                       , packToFile
                       , packFromFiles
                       ) where

import           Archive
import           Control.Composition  ((.*), (.@))
import           Control.Monad        (filterM)
import qualified Data.ByteString.Lazy as BSL
import           Data.DList           (DList, fromList)
import           Data.Foldable        (fold, toList)
import           System.Directory     (doesDirectoryExist, getDirectoryContents)
import           System.FilePath      ((</>))

packFromFiles :: FilePath -- ^ Path of @.tar@ file to write
              -> [FilePath] -- ^ Files and directories to archive
              -> IO ()
packFromFiles tar fps = BSL.writeFile tar =<< packFiles fps

packToFile :: FilePath -> [Entry] -> IO ()
packToFile = writeArchiveBytes .@ BSL.writeFile

unpackFromFile :: FilePath -> IO [Entry]
unpackFromFile = fmap (either (error . show) id . readArchiveBytes) . BSL.readFile

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
    all' <- exclude <$> getDirectoryContents fp
    dirs <- exclude <$> filterM doesDirectoryExist (mkRel <$> all')
    case dirs of
        [] -> pure $ fromList (mkRel <$> all')
        ds -> do
            next <- foldMapA getDirRecursive ds
            pure $ next <> fromList (mkRel <$> all')

    where foldMapA = fmap fold .* traverse
          exclude = filter (\x -> x /= "." && x /= "..")
          mkRel = (fp </>)
