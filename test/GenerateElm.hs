module Main where

import           RIO

import           Data.Proxy     (Proxy (..))
import           Elm            (Spec (Spec), specsToDir, toElmDecoderSource,
                                 toElmEncoderSource, toElmTypeSource)
import           Git.Plantation (Problem, Team)
import           Servant.Elm    (defElmImports, generateElmForAPI)
import           Shelly         (run_, shelly)


spec :: Spec
spec = Spec ["Generated", "API"]
            ( defElmImports
            : toElmTypeSource    (Proxy :: Proxy Team)
            : toElmDecoderSource (Proxy :: Proxy Team)
            : toElmEncoderSource (Proxy :: Proxy Team)
            : toElmTypeSource    (Proxy :: Proxy Problem)
            : toElmDecoderSource (Proxy :: Proxy Problem)
            : toElmEncoderSource (Proxy :: Proxy Problem)
            -- : generateElmForAPI  (Proxy :: Proxy CRUD)
            : []
            )

main :: IO ()
main = do
  specsToDir [spec] "elm-src"
  shelly $ do
    run_ "elm" ["make", "elm-src/Main.elm", "--output=static/main.js"]
    run_ "elm-format" ["--yes", "elm-src/Generated/"]
