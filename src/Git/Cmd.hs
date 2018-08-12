module Git.Cmd where

import           RIO

import           Shelly hiding (FilePath)

clone :: [Text] -> Sh ()
clone = command1_ "git" [] "clone"

fetch :: [Text] -> Sh ()
fetch = command1_ "git" [] "fetch"
