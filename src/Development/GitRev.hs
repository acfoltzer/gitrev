-- |
-- Module      :  $Header$
-- Copyright   :  (c) 2015 Adam C. Foltzer
-- License     :  BSD3
-- Maintainer  :  acfoltzer@galois.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Some handy Template Haskell splices for including the current git
-- hash and branch in the code of your project. Useful for including
-- in panic messages, @--version@ output, or diagnostic info for more
-- informative bug reports.
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- > import Development.GitRev
-- >
-- > panic :: String -> a
-- > panic msg = error panicMsg
-- >   where panicMsg =
-- >           concat [ "[panic ", $(gitBranch), "@", $(gitHash)
-- >                  , " (", $(gitCommitCount), " commits in HEAD)"
-- >                  , dirty, "] ", msg ]
-- >         dirty | $(gitDirty) = " (uncommitted files present)"
-- >               | otherwise   = ""
-- >
-- > main = panic "oh no!"
--
-- > % cabal exec runhaskell Example.hs
-- > Example.hs: [panic master@2702e69355c978805064543489c351b61ac6760b (6 commits in HEAD) (uncommitted files present)] oh no!

module Development.GitRev (gitHash, gitBranch, gitDirty, gitCommitCount) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Maybe
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Directory
import System.Exit
import System.FilePath
import System.Process

-- | Run git with the given arguments and no stdin, returning the
-- stdout output. If git isn't available or something goes wrong,
-- return the second argument.
runGit :: [String] -> String -> IndexUsed -> Q String
runGit args def useIdx = do
  let oops :: SomeException -> IO (ExitCode, String, String)
      oops _e = return (ExitFailure 1, def, "")
  gitFound <- runIO $ isJust <$> findExecutable "git"
  if gitFound
    then do
      -- a lot of bookkeeping to record the right dependencies
      pwd <- runIO getCurrentDirectory
      let hd         = pwd </> ".git" </> "HEAD"
          index      = pwd </> ".git" </> "index"
          packedRefs = pwd </> ".git" </> "packed-refs"
      hdExists  <- runIO $ doesFileExist hd
      when hdExists $ do
        -- the HEAD file either contains the hash of a detached head
        -- or a pointer to the file that contains the hash of the head
        hdRef <- runIO $ readFile hd
        case splitAt 5 hdRef of
          -- pointer to ref
          ("ref: ", relRef) -> do
            let ref = pwd </> ".git" </> relRef
            refExists <- runIO $ doesFileExist ref
            when refExists $ addDependentFile ref
          -- detached head
          _hash -> addDependentFile hd
      -- add the index if it exists to set the dirty flag
      indexExists <- runIO $ doesFileExist index
      when (indexExists && useIdx == IdxUsed) $ addDependentFile index
      -- if the refs have been packed, the info we're looking for
      -- might be in that file rather than the one-file-per-ref case
      -- handled above
      packedExists <- runIO $ doesFileExist packedRefs
      when packedExists $ addDependentFile packedRefs
      runIO $ do
        (code, out, _err) <- readProcessWithExitCode "git" args "" `catch` oops
        case code of
          ExitSuccess   -> return (takeWhile (/= '\n') out)
          ExitFailure _ -> return def
    else return def

-- | Type to flag if the git index is used or not in a call to runGit
data IndexUsed = IdxUsed -- ^ The git index is used
               | IdxNotUsed -- ^ The git index is /not/ used
    deriving (Eq)

-- | Return the hash of the current git commit, or @UNKNOWN@ if not in
-- a git repository
gitHash :: ExpQ
gitHash =
  stringE =<< runGit ["rev-parse", "HEAD"] "UNKNOWN" IdxNotUsed

-- | Return the branch (or tag) name of the current git commit, or @UNKNOWN@
-- if not in a git repository. For detached heads, this will just be
-- "HEAD"
gitBranch :: ExpQ
gitBranch =
  stringE =<< runGit ["rev-parse", "--abbrev-ref", "HEAD"] "UNKNOWN" IdxNotUsed

-- | Return @True@ if there are non-committed files present in the
-- repository
gitDirty :: ExpQ
gitDirty = do
  output <- runGit ["status", "--porcelain"] "" IdxUsed
  case output of
    "" -> conE $ mkName "Prelude.False"
    _  -> conE $ mkName "Prelude.True"

-- | Return the number of commits in the current head
gitCommitCount :: ExpQ
gitCommitCount =
  stringE =<< runGit ["rev-list", "HEAD", "--count"] "UNKNOWN" IdxNotUsed
