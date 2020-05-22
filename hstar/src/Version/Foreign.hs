module Version.Foreign ( zstd
                       , zlib
                       , lzma
                       ) where

import           Foreign.C.String (CString, peekCString)
import           System.IO.Unsafe (unsafeDupablePerformIO)

foreign import ccall unsafe "ZSTD_versionString" zstdVersionString :: CString
foreign import ccall unsafe zlibVersion :: CString
foreign import ccall unsafe lzma_version_string :: CString

zstd :: String
zstd = unsafeDupablePerformIO (peekCString zstdVersionString)

zlib :: String
zlib = unsafeDupablePerformIO (peekCString zlibVersion)

lzma :: String
lzma = unsafeDupablePerformIO (peekCString lzma_version_string)
