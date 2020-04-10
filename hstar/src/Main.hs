module Main ( main ) where

import           Archive.Compression
import           Compression
import           Data.Maybe          (fromMaybe)
import           Options.Applicative
import           Version

-- pack a directory/list of files?
data Command = PackDir !FilePath !FilePath
    | Pack ![FilePath] !FilePath
    | Unpack !FilePath !(Maybe FilePath)
    | PackSrc !FilePath !FilePath

run :: Command -> IO ()
run (Unpack src dest) =
    let dec = decompressor (compressionByFileExt src)
        in unpackFileToDirAndDecompress dec src (fromMaybe "." dest)
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
        <> fileCompletions
        <> help "Archive to unpack")
    <*> optional (argument str
        (metavar "DEST"
        <> dirCompletions
        <> help "Where to unpack it"))

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
        <> dirCompletions
        <> help "Directory to pack up")

archive :: Parser FilePath
archive = argument str
        (metavar "ARCHIVE"
        <> fileCompletions
        <> help "File to pack it to")

pack :: Parser Command
pack = Pack
    <$> some (strOption
        (metavar "FILE"
        <> long "file"
        <> short 'f'
        <> fileCompletions
        <> help "File to add to archive"))
    <*> archive

fileCompletions :: HasCompleter f => Mod f a
fileCompletions = completer (bashCompleter "file -o plusdirs")

dirCompletions :: HasCompleter f => Mod f a
dirCompletions = completer (bashCompleter "directory")

cmd :: Parser Command
cmd = hsubparser
    (command "unpack" (info unpack (progDesc "Unpack an archive"))
    <> command "pack-dir" (info packDir (progDesc "Pack a directory's contents into an archive"))
    <> command "pack" (info pack (progDesc "Pack an archive from a list of files"))
    <> command "pack-src" (info packSrc (progDesc "Pack up a source directory as a bundle, ignoring version control and artifact directories"))
    )

versionMod :: Parser (a -> a)
versionMod = infoOption allVersionsString (short 'V' <> long "version" <> help "Show version")

topLevel :: ParserInfo Command
topLevel = info (helper <*> versionMod <*> cmd)
    (fullDesc
    <> progDesc "A Haskell archiver tool"
    <> header "hstar - a flexible archiving tool")

main :: IO ()
main = run =<< execParser topLevel
