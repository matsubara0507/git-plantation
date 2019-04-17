{-# LANGUAGE OverloadedStrings #-}

module Shh.Command
    ( echo
    , pwd
    , cat
    , mkdir
    , ls
    , rm
    , touch
    , test_d
    , test_f
    , git
    ) where

import           Prelude

import           Data.Text    (Text)
import qualified Data.Text    as Text
import           Shh.Internal

echo :: Text -> Proc ()
echo str = mkProc "echo" [Text.unpack str]

pwd :: Proc ()
pwd = mkProc "pwd" []

cat, mkdir, ls, rm, touch :: [Text] -> Text -> Proc ()
cat   args path = mkProc "cat"   $ Text.unpack <$> args <> [path]
mkdir args path = mkProc "mkdir" $ Text.unpack <$> args <> [path]
ls    args path = mkProc "ls"    $ Text.unpack <$> args <> [path]
rm    args path = mkProc "rm"    $ Text.unpack <$> args <> [path]
touch args path = mkProc "touch" $ Text.unpack <$> args <> [path]

test_d :: (ProcFailure m, Functor m) => FilePath -> m Bool
test_d path = (== 0) <$> catchCode (mkProc "test" ["-d", path])

test_f :: (ProcFailure m, Functor m) => FilePath -> m Bool
test_f path = (== 0) <$> catchCode (mkProc "test" ["-f", path])

git :: Text -> [Text] -> Proc ()
git cmd args = mkProc "git" $ Text.unpack <$> (cmd : args)
