-- | None of these functions are lazy; however, lrzip decompresses taking advantage of random access and multiple threads.
module Codec.Compression.Lrzip ( decompress
                               ) where

import Control.Monad (unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import Foreign.C.Types (CChar, CULong)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (castPtr, Ptr)
import Foreign.Storable (peek)
import System.IO.Unsafe (unsafeDupablePerformIO)

#include <Lrzip.h>

{# fun lrzip_decompress as ^ { castPtr `Ptr CChar', alloca- `CULong' peek*, castPtr `Ptr CChar', `CULong' } -> `Bool' #}

libHarness :: (Ptr CChar -> Ptr CChar -> CULong -> IO (Bool, CULong)) -> BS.ByteString -> BS.ByteString
libHarness libFunction inBS = unsafeDupablePerformIO $
    BS.unsafeUseAsCStringLen inBS $ \(inBuf, inSz) ->
        alloca $ \outPtr -> do
            (res, outSz) <- libFunction outPtr inBuf (fromIntegral inSz)
            unless res $
                error "Error calling lrzip function."
            BS.packCStringLen (outPtr, fromIntegral outSz)

decompress :: BS.ByteString -> BS.ByteString
decompress = libHarness lrzipDecompress
