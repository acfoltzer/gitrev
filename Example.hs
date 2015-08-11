{-# LANGUAGE TemplateHaskell #-}
module Example where
import Development.GitRev

panic :: String -> a
panic msg = error panicMsg
  where panicMsg =
          concat [ "[panic ", $(gitBranch), "@", $(gitHash)
                 , " (", $(gitCommitCount), " commits in HEAD)"
                 , dirty, "] ", msg ]
        dirty | $(gitDirty) = " (uncommitted files present)"
              | otherwise   = ""

main = panic "oh no!"
