module Compression.Lrzip ( decompress
                         ) where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import           System.IO            (hSetBinaryMode)
import           System.IO.Unsafe     (unsafePerformIO)
import           System.Process       (CreateProcess, StdStream (CreatePipe, Inherit), createProcess, proc, std_err, std_in, std_out)

-- TODO:
-- http://hackage.haskell.org/package/temporary-1.3/docs/System-IO-Temp.html
-- handle?

decompress :: BS.ByteString -> BSL.ByteString
decompress = unsafePerformIO . decompressIO

bsProc :: CreateProcess -> BS.ByteString -> IO BSL.ByteString
bsProc p bs = do
    (Just stdin, Just stdout, _, _) <- createProcess p { std_in = CreatePipe, std_out = CreatePipe }
    BS.hPut stdin bs
    hSetBinaryMode stdout True
    BSL.hGetContents stdout

decompressIO :: BS.ByteString -> IO BSL.ByteString
decompressIO = bsProc ((proc "lrzip" ["-d", "-q"]) { std_err = Inherit })
