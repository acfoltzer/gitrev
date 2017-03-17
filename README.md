[![Build Status](https://travis-ci.org/acfoltzer/gitrev.svg?branch=master)](https://travis-ci.org/acfoltzer/gitrev)

Some handy Template Haskell splices for including the current git hash
and branch in the code of your project. Useful for including in panic
messages, `--version` output, or diagnostic info for more informative
bug reports.

Most of the complication in the `GitRev` module is due to the various
places the current git hash might be stored:

1. Detached HEAD: the hash is in `.git/HEAD`
2. On a branch or tag: the hash is in a file pointed to by `.git/HEAD`
in a location like `.git/refs/heads`
3. On a branch or tag but in a repository with packed refs: the hash
is in `.git/packed-refs`
4. In any of the above situations, if the current repo is checked out
as a submodule, follow the reference to its `.git` directory first

These files are added as dependencies to modules that use `GitRev`, and
so the module should be rebuilt automatically whenever these files
change.

If you run into further scenarios that cause problems, let me know!
