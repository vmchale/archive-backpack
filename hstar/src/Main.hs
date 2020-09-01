module Main ( main ) where

import           Codec.Archive        (Entry (Entry), EntryContent (Hardlink), entriesToBSL, readArchiveBSL)
import           Compression
import           Compression.Level
import           Control.Exception    (throw)
import qualified Data.ByteString.Lazy as BSL
import           Data.Maybe           (fromMaybe)
import           Options.Applicative
import           Tar
import           Version

-- pack a directory/list of files?
-- repack .tar.gz to .cpio.gz or whatnot?
data Command = PackDir !FilePath !FilePath !CompressionLevel
    | Pack ![FilePath] !FilePath !CompressionLevel
    | Unpack !FilePath !(Maybe FilePath)
    | PackSrc !FilePath !FilePath !CompressionLevel
    | Sanitize !FilePath !CompressionLevel

forceLast :: [a] -> IO ()
forceLast = (`seq` mempty) . last

forceBSL :: BSL.ByteString -> IO ()
forceBSL = forceLast . BSL.toChunks

sanitize :: FilePath -> CompressionLevel -> IO ()
sanitize fp lvl = do
    let enc = compressionByFileExt fp
    contents <- BSL.readFile fp
    decoded <- decompressor enc contents <$ forceBSL contents
    let es = either throw id $ readArchiveBSL decoded
        -- also removes hardlinks pointing to themselves
        paxContents = entriesToBSL (filter (not.selfLink) es)
    BSL.writeFile fp (compressor enc lvl paxContents)


selfLink :: Eq fp => Entry fp e -> Bool
selfLink (Entry fp (Hardlink fp') _ _ _) = (fp == fp')
selfLink _                               = False

run :: Command -> IO ()
run (Sanitize src lvl) = sanitize src lvl
run (Unpack src dest) =
    let dec = decompressor (compressionByFileExt src)
        in unpackFileToDirAndDecompress dec src (fromMaybe "." dest)
run (PackDir dir' tar lvl) =
    let comp = compressor (compressionByFileExt tar) lvl
        in packFromDirAndCompress comp dir' tar
run (Pack fs tar lvl) =
    let comp = compressor (compressionByFileExt tar) lvl
        in packFromFilesAndCompress comp tar fs
run (PackSrc dir' tar lvl) =
    let comp = compressor (compressionByFileExt tar) lvl
        in packSrcDirAndCompress comp dir' tar

sanitizeP :: Parser Command
sanitizeP = Sanitize
    <$> argument str
        (metavar "SRC"
        <> fileCompletions
        <> help "Archive to pax-ify")
    <*> compressionLevel

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
    <*> compressionLevel

packSrc :: Parser Command
packSrc = PackSrc
    <$> dir
    <*> archive
    <*> compressionLevel

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
    <*> compressionLevel

compressionLevel :: Parser CompressionLevel
compressionLevel =
        compressCustom
    <|> compressBest
    <|> compressFast
    <|> flag Default Default mempty

compressCustom :: Parser CompressionLevel
compressCustom =
    Custom <$>
        option auto
        (long "compression-level"
        <> short 'l'
        <> metavar "LVL"
        <> help "Compression level (usually 0-9)"
        <> completer (listCompleter (show <$> [(0::Int)..22]))
        )

compressBest :: Parser CompressionLevel
compressBest =
    flag' Best (long "best")

compressFast :: Parser CompressionLevel
compressFast =
    flag' Fastest (long "fastest")

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
    <> command "sanitize" (info sanitizeP (progDesc "Sanitize a tar archive so it is pax-compatible"))
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
