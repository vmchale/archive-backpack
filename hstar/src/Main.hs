module Main ( main ) where

import           Codec.Archive        (Entry (Entry), EntryContent (..), entriesToBSL, readArchiveBSL)
import           Compression
import           Compression.Level
import           Control.Exception    (throw, throwIO)
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable        (traverse_)
import           Data.Maybe           (fromMaybe)
import           Lint
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
    | Verify !FilePath
    | Lint !FilePath
    | Inspect !FilePath

forceLast :: [a] -> IO ()
forceLast = (`seq` mempty) . last

forceBSL :: BSL.ByteString -> IO ()
forceBSL = forceLast . BSL.toChunks

inspect :: FilePath -> IO ()
inspect fp = do
    let enc = compressionByFileExt fp
    contents <- decompressor enc <$> BSL.readFile fp
    let es = either throw id $ readArchiveBSL contents
    traverse_ (putStrLn . printEntry) es

lint :: FilePath -> IO ()
lint fp = do
    let enc = compressionByFileExt fp
    contents <- decompressor enc <$> BSL.readFile fp
    let es = either throw id $ readArchiveBSL contents
    traverse_ lintEntry es

verify :: FilePath -> IO ()
verify fp = do
    let enc = compressionByFileExt fp
    contents <- decompressor enc <$> BSL.readFile fp
    -- FIXME: forceLast
    either throwIO forceLast $ readArchiveBSL contents

sanitize :: FilePath -> CompressionLevel -> IO ()
sanitize fp lvl = do
    let enc = compressionByFileExt fp
    contents <- BSL.readFile fp
    decoded <- decompressor enc contents <$ forceBSL contents
    let es = either throw id $ readArchiveBSL decoded
        -- also removes hardlinks pointing to themselves
        paxContents = entriesToBSL (filter (not.selfLink) es)
    BSL.writeFile fp (compressor enc lvl paxContents)

run :: Command -> IO ()
run (Verify fp) = verify fp
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
run (Lint fp) = lint fp
run (Inspect fp) = inspect fp

sanitizeP :: Parser Command
sanitizeP = Sanitize
    <$> argument str
        (metavar "SRC"
        <> fileCompletions
        <> help "Archive to pax-ify")
    <*> compressionLevel

inspectP :: Parser Command
inspectP = Inspect
    <$> argument str
        (metavar "ARCHIVE"
        <> fileCompletions
        <> help "Archive to open")

lintP :: Parser Command
lintP = Lint
    <$> argument str
        (metavar "ARCHIVE"
        <> fileCompletions
        <> help "Archive to check")

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

check :: Parser Command
check = Verify
    <$> argument str
        (metavar "SRC"
        <> fileCompletions
        <> help "Archive to verify")

cmd :: Parser Command
cmd = hsubparser
    (command "unpack" (info unpack (progDesc "Unpack an archive"))
    <> command "pack-dir" (info packDir (progDesc "Pack a directory's contents into an archive"))
    <> command "pack" (info pack (progDesc "Pack an archive from a list of files"))
    <> command "pack-src" (info packSrc (progDesc "Pack up a source directory as a bundle, ignoring version control and artifact directories"))
    <> command "sanitize" (info sanitizeP (progDesc "Sanitize a tar archive so it is pax-compatible"))
    <> command "check" (info check (progDesc "Check that a tar archive is valid"))
    <> command "lint" (info lintP (progDesc "Lint an archive"))
    <> command "inspect" (info inspectP (progDesc "Inspect an archive"))
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

printEntry :: Entry -> String
printEntry (Entry fp Directory _ _ _)        = "dir " ++ fp
printEntry (Entry fp (NormalFile bsl) _ _ _) = fp ++ " (" ++ show (BSL.length bsl) ++ " bytes)" -- TODO: upstream get size?
printEntry (Entry fp (Symlink fp' _) _ _ _)  = fp ++ " -> " ++ fp'
printEntry (Entry fp (Hardlink fp') _ _ _)   = fp ++ " link to " ++ fp'
