module Main ( main ) where

import           Archive.Generic
import           Compression
import           Options.Applicative

-- pack a directory/list of files?
data Command = PackDir FilePath FilePath
             | Pack [FilePath] FilePath
             | Unpack FilePath FilePath

run :: Command -> IO ()
run (Unpack src dest) =
    let dec = decompressor (compressionByFileExt dest)
        in unpackFileToDirAndDecompress dec src dest
run (PackDir dir tar) = packFromDir dir tar
run (Pack fs tar)     = packFromFiles tar fs

unpack :: Parser Command
unpack = Unpack
    <$> argument str
        (metavar "SRC"
        -- <> completer (bashCompleter "file -X '!*.*tar' -o plusdirs")
        <> help "Archive to unpack")
    <*> argument str
        (metavar "DEST"
        <> help "Where to unpack it")

packDir :: Parser Command
packDir = PackDir
    <$> argument str
        (metavar "DIR"
        <> help "Directory to pack up")
    <*> argument str
        (metavar "ARCHIVE"
        <> help "File to pack it to")

pack :: Parser Command
pack = Pack
    <$> some (strOption
        (metavar "FILE"
        <> long "file"
        <> short 'f'
        <> help "File to add to archive"))
    <*> argument str
        (metavar "ARCHIVE"
        <> help "File to pack into")

cmd :: Parser Command
cmd = hsubparser
    (command "unpack" (info unpack (progDesc "Unpack an archive"))
    <> command "pack-dir" (info packDir (progDesc "Pack a directory's contents into an archive"))
    <> command "pack" (info pack (progDesc "Pack an archive from a list of files"))
    )

topLevel :: ParserInfo Command
topLevel = info (helper <*> cmd)
    (fullDesc
    <> progDesc "A Haskell implementation of tar"
    <> header "hstar - a flexible archiving tool")

main :: IO ()
main = run =<< execParser topLevel
