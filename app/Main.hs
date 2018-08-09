{-# LANGUAGE LambdaCase #-}

module Main where

import           RIO

import           Git.Plantation
import           System.Environment (getArgs)

main :: IO ()
main = (listToMaybe <$> getArgs) >>= \case
  Nothing   -> error "please input config file path."
  Just path -> do
    conf <- readConfig path
    hPutBuilder stdout $ show' conf
  where
    show' = encodeUtf8Builder . utf8BuilderToText . displayShow
