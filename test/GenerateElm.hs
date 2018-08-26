{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Main where

import           RIO

import           Data.Proxy              (Proxy (..))
import           Elm                     (Spec (Spec), specsToDir,
                                          toElmDecoderSource,
                                          toElmEncoderSource, toElmTypeSource)
import           Git.Plantation          (Problem, Team)
import           Git.Plantation.API.CRUD (CRUD)
import           Servant                 ((:>))
import           Servant.Elm             (defElmImports, generateElmForAPI)
import           Shelly                  (run_, shelly)


spec :: Spec
spec = Spec ["Generated", "API"]
            ( defElmImports
            : toElmTypeSource    (Proxy @ Team)
            : toElmDecoderSource (Proxy @ Team)
            : toElmEncoderSource (Proxy @ Team)
            : toElmTypeSource    (Proxy @ Problem)
            : toElmDecoderSource (Proxy @ Problem)
            : toElmEncoderSource (Proxy @ Problem)
            : generateElmForAPI  (Proxy @ ("api" :> CRUD))
            )

main :: IO ()
main = do
  specsToDir [spec] "elm-src"
  shelly $ do
    run_ "elm" ["make", "elm-src/Main.elm", "--output=static/main.js"]
    run_ "elm-format" ["--yes", "elm-src/Generated/"]
