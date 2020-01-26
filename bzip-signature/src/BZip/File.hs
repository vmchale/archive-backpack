-- | @since 0.1.1.0
module BZip.File ( decompressFromFile
                 , compressToFile
                 ) where

import           BZip
import           Control.Composition  ((.@))
import qualified Data.ByteString.Lazy as BSL

-- | Read data from a compressed file
decompressFromFile :: FilePath -> IO BSL.ByteString
decompressFromFile = fmap decompress . BSL.readFile

-- | Write data to a compressed file
compressToFile :: FilePath -> BSL.ByteString -> IO ()
compressToFile = compress .@ BSL.writeFile
