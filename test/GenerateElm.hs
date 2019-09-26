{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Main where

import           RIO

import           Data.Proxy              (Proxy (..))
import           Elm.Mapping
import           Git.Plantation          (Config, Link, Problem, Repo, Score,
                                          ScoreBoardConfig, Status, Team, User)
import           Git.Plantation.API.CRUD (GetAPI)
import           Servant                 ((:>))
import           Servant.Elm.Mapping     (defElmImports, defElmOptions,
                                          generateElmModuleWith)

main :: IO ()
main =
  generateElmModuleWith
    defElmOptions
    ["Generated", "API"]
    defElmImports
    "elm-src"
    [ DefineElm (Proxy @ Team)
    , DefineElm (Proxy @ User)
    , DefineElm (Proxy @ Repo)
    , DefineElm (Proxy @ Problem)
    , DefineElm (Proxy @ Config)
    , DefineElm (Proxy @ ScoreBoardConfig)
    , DefineElm (Proxy @ Score)
    , DefineElm (Proxy @ Status)
    , DefineElm (Proxy @ Link)
    ]
    (Proxy @ ("api" :> GetAPI))
