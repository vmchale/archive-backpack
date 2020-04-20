-- | Snappy frames
module Codec.Compression.Snappy.BSL ( compress
                                    , decompress
                                    ) where

import qualified Codec.Compression.Snappy         as Snappy
import qualified Codec.Compression.Snappy.Framing as Snappy
import           Data.Binary                      (decodeOrFail, encode)
import qualified Data.ByteString.Lazy             as BSL

-- | Throws exception on error.
decompress :: BSL.ByteString -> BSL.ByteString
decompress = BSL.fromChunks . loop
    where loop bs =
            let (res, _, chunk) = yeet $ decodeOrFail bs
                in if BSL.null res
                    then [extractUncompressed chunk]
                    else extractUncompressed chunk : loop res
          yeet = either (error.show) id
          extractUncompressed (Snappy.Compressed _ d)   = Snappy.decompress d
          extractUncompressed (Snappy.Uncompressed _ x) = x
          extractUncompressed Snappy.StreamIdentifier   = mempty
          extractUncompressed Snappy.Skippable{}        = mempty
          extractUncompressed _                         = error "Expected Uncompressed{}, Skippable{} or StreamIdentifier; possible corrupt stream"

compress :: BSL.ByteString -> BSL.ByteString
compress = (Snappy.streamIdentifier <>) . loop
    where loop bsl =
            let (chunk, res) = Snappy.encode bsl
                in case res of
                    Just x  -> extractCompressed chunk <> loop x
                    Nothing -> extractCompressed chunk
          extractCompressed c@Snappy.Compressed{}   = encode c
          extractCompressed c@Snappy.Uncompressed{} = encode c
          -- see: http://hackage.haskell.org/package/snappy-framing-0.1.2/docs/Codec-Compression-Snappy-Framing.html#v:encode
          extractCompressed _                       = error "Expected Compressed{}; possible corrupt stream"
