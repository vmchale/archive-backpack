module Compression ( compressionByFileExt
                   , decompressor
                   , compressor
                   ) where

import qualified Codec.Compression.BZip as BZip
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.Lzma as Lzma
import           Codec.Lzip             as Lzip
import qualified Data.ByteString.Lazy   as BSL
import           Data.List              (isSuffixOf)

data Compressor = Lzma
                | Lz
                | Bz2
                | GZip
                | None

compressionByFileExt :: FilePath -> Compressor
compressionByFileExt fp | ".tgz" `isSuffixOf` fp = GZip
                        | ".tar.bz2" `isSuffixOf` fp = Bz2
                        | ".tar.gz" `isSuffixOf` fp = GZip
                        | ".tar.xz" `isSuffixOf` fp = Lzma
                        | ".tar.lz" `isSuffixOf` fp = Lz
                        | ".tar" `isSuffixOf` fp = None
                        | otherwise = error "Suffix not supported or invalid."

decompressor :: Compressor -> (BSL.ByteString -> BSL.ByteString)
decompressor Lzma = Lzma.decompress
decompressor Bz2  = BZip.decompress
decompressor GZip = GZip.decompress
decompressor Lz   = Lzip.decompress . BSL.toStrict
decompressor None = id

compressor :: Compressor -> (BSL.ByteString -> BSL.ByteString)
compressor Lzma = Lzma.compress
compressor Bz2  = BZip.compress
compressor GZip = GZip.compress
compressor Lz   = Lzip.compress . BSL.toStrict
compressor None = id
