module Git.Cmd where

import           RIO

import           Shelly hiding (FilePath)

clone :: [Text] -> Sh ()
clone = command1_ "git" [] "clone"

fetch :: [Text] -> Sh ()
fetch = command1_ "git" [] "fetch"

remote :: [Text] -> Sh ()
remote = command1_ "git" [] "remote"

push :: [Text] -> Sh ()
push = command1_ "git" [] "push"

checkout :: [Text] -> Sh ()
checkout = command1_ "git" [] "checkout"

commit :: [Text] -> Sh ()
commit = command1_ "git" [] "commit"
