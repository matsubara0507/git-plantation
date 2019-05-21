{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Main where

import           RIO

import           Data.Proxy              (Proxy (..))
import           Elm                     (ElmType, Spec (Spec), specsToDir,
                                          toElmDecoderSource,
                                          toElmEncoderSource, toElmTypeSource)
import           Git.Plantation          (Config, Link, Problem, Repo,
                                          ScoreBoardConfig, ScoreR, Status,
                                          Team, User)
import           Git.Plantation.API.CRUD (GetAPI)
import           Servant                 ((:>))
import           Servant.Elm             (defElmImports, generateElmForAPI)
import           Shelly                  (run_, shelly)

spec :: Spec
spec = Spec ["Generated", "API"] $ concat
            [ [defElmImports]
            , toElmTypeAll      (Proxy @ Team)
            , toElmTypeAll      (Proxy @ User)
            , toElmTypeAll      (Proxy @ Repo)
            , toElmTypeAll      (Proxy @ Problem)
            , toElmTypeAll      (Proxy @ Config)
            , toElmTypeAll      (Proxy @ ScoreBoardConfig)
            , toElmTypeAll      (Proxy @ ScoreR)
            , toElmTypeAll      (Proxy @ Status)
            , toElmTypeAll      (Proxy @ Link)
            , generateElmForAPI (Proxy @ ("api" :> GetAPI))
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
  shelly $ do
    run_ "elm" ["make", "elm-src/Main.elm", "--output=static/main.js"]
    run_ "elm-format" ["--yes", "elm-src/Generated/"]
