{-# LANGUAGE OverloadedStrings #-}

module Shh.Command.Git where

import           Data.Text    (Text)
import           Shh.Command  (git)
import           Shh.Internal (Proc)

clone, add, commit, push, fetch, pull, branch, checkout, remote :: [Text] -> Proc ()
clone    = git "clone"
add      = git "add"
commit   = git "commit"
push     = git "push"
fetch    = git "fetch"
pull     = git "pull"
branch   = git "branch"
checkout = git "checkout"
remote   = git "remote"
