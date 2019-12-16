module Main ( main ) where

import           Archive.Compression
import           Compression
import           Options.Applicative

-- pack a directory/list of files?
data Command = PackDir !FilePath !FilePath
             | Pack ![FilePath] !FilePath
             | Unpack !FilePath !FilePath
             | PackSrc !FilePath !FilePath

run :: Command -> IO ()
run (Unpack src dest) =
    let dec = decompressor (compressionByFileExt src)
        in unpackFileToDirAndDecompress dec src dest
run (PackDir dir' tar) =
    let comp = compressor (compressionByFileExt tar)
        in packFromDirAndCompress comp dir' tar
run (Pack fs tar) =
    let comp = compressor (compressionByFileExt tar)
        in packFromFilesAndCompress comp tar fs
run (PackSrc dir' tar) =
    let comp = compressor (compressionByFileExt tar)
        in packSrcDirAndCompress comp dir' tar

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
    <$> dir
    <*> archive

packSrc :: Parser Command
packSrc = PackSrc
    <$> dir
    <*> archive

dir :: Parser FilePath
dir = argument str
        (metavar "DIR"
        <> help "Directory to pack up")

archive :: Parser FilePath
archive = argument str
        (metavar "ARCHIVE"
        <> help "File to pack it to")

pack :: Parser Command
pack = Pack
    <$> some (strOption
        (metavar "FILE"
        <> long "file"
        <> short 'f'
        <> help "File to add to archive"))
    <*> archive

cmd :: Parser Command
cmd = hsubparser
    (command "unpack" (info unpack (progDesc "Unpack an archive"))
    <> command "pack-dir" (info packDir (progDesc "Pack a directory's contents into an archive"))
    <> command "pack" (info pack (progDesc "Pack an archive from a list of files"))
    <> command "pack-src" (info packSrc (progDesc "Pack up a source directory, ignoring version control and artifact directories"))
    )

topLevel :: ParserInfo Command
topLevel = info (helper <*> cmd)
    (fullDesc
    <> progDesc "A Haskell implementation of tar"
    <> header "hstar - a flexible archiving tool")

main :: IO ()
main = run =<< execParser topLevel
