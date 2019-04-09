{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Main where

import           RIO

import           Data.Proxy              (Proxy (..))
import           Elm                     (ElmType, Spec (Spec), specsToDir,
                                          toElmDecoderSource,
                                          toElmEncoderSource, toElmTypeSource)
import           Git.Plantation          (Config, Link, Problem, Repo, Score,
                                          ScoreBoardConfig, Status, Team, User)
import           Git.Plantation.API.CRUD (CRUD)
import           Servant                 ((:>))
import           Servant.Elm             (defElmImports, generateElmForAPI)
import qualified Shh                     as Shell

spec :: Spec
spec = Spec ["Generated", "API"] $ concat
            [ [defElmImports]
            , toElmTypeAll      (Proxy @ Team)
            , toElmTypeAll      (Proxy @ User)
            , toElmTypeAll      (Proxy @ Repo)
            , toElmTypeAll      (Proxy @ Problem)
            , toElmTypeAll      (Proxy @ Config)
            , toElmTypeAll      (Proxy @ ScoreBoardConfig)
            , toElmTypeAll      (Proxy @ Score)
            , toElmTypeAll      (Proxy @ Status)
            , toElmTypeAll      (Proxy @ Link)
            , generateElmForAPI (Proxy @ ("api" :> CRUD))
            ]

toElmTypeAll :: ElmType a => Proxy a -> [Text]
toElmTypeAll proxy =
  [ toElmTypeSource    proxy
  , toElmDecoderSource proxy
  , toElmEncoderSource proxy
  ]

main :: IO ()
main = do
  specsToDir [spec] "elm-src"
  Shell.runProc $ do
    Shell.mkProc "elm" ["make", "elm-src/Main.elm", "--output=static/main.js"]
    Shell.mkProc "elm-format" ["--yes", "elm-src/Generated/"]
