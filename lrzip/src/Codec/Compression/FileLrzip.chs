-- | Throw exceptions on error.
module Codec.Compression.FileLrzip ( decompress
                                   ) where

import Control.Monad (forM_, unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Coerce (coerce)
import Data.Functor (void)
import Data.Word (Word8)
import Foreign.C.Types (CChar, CFile, CSize (..), CUInt, CULong (..))
import Foreign.ForeignPtr (castForeignPtr, newForeignPtr)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Ptr (castPtr, Ptr)
import Foreign.Storable (peek, sizeOf)
import System.IO.Unsafe (unsafeInterleaveIO, unsafePerformIO)

-- see man tmpfile
-- I think is what we actually want lol

#include <stdio.h>
#include <Lrzip.h>

{# fun tmpfile as ^ { } -> `Ptr CFile' castPtr #}
{# fun fread as ^ { castPtr `Ptr a', coerce `CSize', coerce `CSize', castPtr `Ptr CFile' } -> `CSize' coerce #}
{# fun fwrite as ^ { castPtr `Ptr a', coerce `CSize', coerce `CSize', castPtr `Ptr CFile' } -> `CSize' coerce #}
{# fun lrzip_init as ^ { } -> `Bool' #}

{# enum Lrzip_Mode as LrzipMode {underscoreToCase} #}

data Lrzip

{# pointer *Lrzip as LrzipPtr foreign finalizer lrzip_free as ^ -> Lrzip #}

{# fun lrzip_new as ^ { `LrzipMode' } -> `Ptr Lrzip' id #}
{# fun lrzip_mode_set as ^ { `LrzipPtr', `LrzipMode' } -> `Bool' #}
{# fun lrzip_compression_level_set as ^ { `LrzipPtr', `CUInt' } -> `Bool' #}
{# fun lrzip_file_add as ^ { `LrzipPtr', castPtr `Ptr CFile' } -> `Bool' #}
{# fun lrzip_outfile_set as ^ { `LrzipPtr', castPtr `Ptr CFile' } -> `()' #}
{# fun lrzip_run as ^ { `LrzipPtr' } -> `Bool' #}

chunkWrite :: BS.ByteString -> Ptr CFile -> IO ()
chunkWrite bs fp =
    BS.unsafeUseAsCStringLen bs $ \(buf, sz) ->
        void $ fwrite buf size (fromIntegral sz) fp

bslWrite :: BSL.ByteString -> Ptr CFile -> IO ()
bslWrite bsl fp = forM_ (BSL.toChunks bsl) $ \b ->
    chunkWrite b fp

size :: CSize
size = fromIntegral (sizeOf (undefined :: Word8))

chunkRead :: Ptr CFile -> IO (Maybe BS.ByteString)
chunkRead fp =
    allocaBytes (32 * 1024) $ \outBuf ->
        do bRead <- fread outBuf size (32 * 1024) fp
           if bRead == 0
                then pure Nothing
                else Just <$> BS.packCStringLen (outBuf, fromIntegral bRead)

readBSL :: Ptr CFile -> IO BSL.ByteString
readBSL fp = BSL.fromChunks <$> loop
    where loop :: IO [BS.ByteString]
          loop = do
                next <- chunkRead fp
                case next of
                    Just b -> (b:) <$> unsafeInterleaveIO loop
                    Nothing -> pure []

decompress :: BSL.ByteString -> BSL.ByteString
decompress bsl = unsafePerformIO $ do
    lrzipInit
    fp <- tmpfile
    fpOut <- tmpfile
    bslWrite bsl fp
    preLr <- lrzipNew LrzipModeDecompress
    lr <- castForeignPtr <$> newForeignPtr lrzipFree (castPtr preLr)
    _ <- lrzipFileAdd lr fp
    _ <- lrzipOutfileSet lr fpOut
    _ <- lrzipRun lr
    readBSL fpOut

compress :: BSL.ByteString -> BSL.ByteString
compress bsl = unsafePerformIO $ do
    lrzipInit
    fp <- tmpfile
    fpOut <- tmpfile
    bslWrite bsl fp
    preLr <- lrzipNew LrzipModeCompressLzma
    lr <- castForeignPtr <$> newForeignPtr lrzipFree (castPtr preLr)
    _ <- lrzipFileAdd lr fp
    _ <- lrzipOutfileSet lr fpOut
    _ <- lrzipRun lr
    readBSL fpOut
