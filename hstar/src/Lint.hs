module Lint ( lintEntry
            , selfLink
            ) where

import           Codec.Archive
import           Control.Exception (Exception, throwIO)
import           Control.Monad     (when)
import           Data.List         (isPrefixOf)

data LintException = SelfHardlink FilePath
                   | InsecureLink FilePath

instance Show LintException where
    show (SelfHardlink fp) = "Entry " ++ fp ++ " is a hardlink pointing to itself."
    show (InsecureLink fp) = "Entry " ++ fp ++ " is an insecure link"

instance Exception LintException

selfLink :: Eq fp => Entry fp e -> Bool
selfLink (Entry fp (Hardlink fp') _ _ _) = (fp == fp')
selfLink _                               = False

insecurePath :: FilePath -> Bool
insecurePath fp | "/" `isPrefixOf` fp = True
                | otherwise = False

lintEntry :: Entry FilePath e -> IO ()
lintEntry (Entry fp (Hardlink fp') _ _ _) =
    when (fp == fp') $
        throwIO (SelfHardlink fp)
lintEntry (Entry fp (Hardlink fp') _ _ _) =
    when (insecurePath fp') $
        print (InsecureLink fp)
lintEntry (Entry fp (Symlink fp' _) _ _ _) =
    when (insecurePath fp') $
        print (InsecureLink fp)
lintEntry _ = pure ()
