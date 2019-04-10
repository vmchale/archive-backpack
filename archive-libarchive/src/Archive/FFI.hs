module Archive.FFI ( Entry
                   , packToFile
                   , unpackFromFile
                   ) where

import qualified Codec.Archive as FFI

type Entry = FFI.Entry

packToFile = FFI.entriesToFile

unpackFromFile = FFI.readArchiveFile
