{-# LANGUAGE TemplateHaskell #-}

module Mix.Plugin.Shell.Internal
    ( cat
    , echo
    , mkdir
    , ls
    , pwd
    , rm
    , test
    , touch
    ) where

import           Shh

$(load SearchPath ["touch", "cat", "rm", "pwd", "mkdir", "test", "echo", "ls"])
