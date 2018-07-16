module Main where

import           Lib
import           RIO

main :: IO ()
main = hPutBuilder stdout $ encodeUtf8Builder someFunc
