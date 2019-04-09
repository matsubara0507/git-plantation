{-# LANGUAGE TemplateHaskell #-}

module Mix.Plugin.Shell.Internal
    ( cat
    , git
    , mkdir
    , pwd
    , rm
    , touch
    ) where

import           Shh

$(load SearchPath ["touch", "cat", "rm", "git", "pwd", "mkdir"])
