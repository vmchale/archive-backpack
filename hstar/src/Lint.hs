module Lint ( lintEntry
            , selfLink
            ) where

import           Codec.Archive
import           Control.Exception (Exception, throwIO)
import           Control.Monad     (when)

newtype LintException = SelfHardlink FilePath

instance Show LintException where
    show (SelfHardlink fp) = "Entry " ++ fp ++ " is a hardlink pointing to itself."

instance Exception LintException

selfLink :: Entry -> Bool
selfLink (Entry fp (Hardlink fp') _ _ _) = (fp == fp')
selfLink _                               = False

lintEntry :: Entry -> IO ()
lintEntry (Entry fp (Hardlink fp') _ _ _) =
    when (fp == fp') $
        throwIO (SelfHardlink fp)
lintEntry _ = pure ()
