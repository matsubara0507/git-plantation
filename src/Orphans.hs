{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Orphans () where

import           RIO
import qualified RIO.List           as L
import qualified RIO.Text           as Text

import           Data.Extensible
import           GHC.TypeLits       (symbolVal)
import           Web.FormUrlEncoded
import           Web.HttpApiData

instance Forall (KeyValue KnownSymbol FromHttpApiData) xs => FromForm (Record xs) where
  fromForm form =
    hgenerateFor (Proxy @ (KeyValue KnownSymbol FromHttpApiData)) $ \m ->
      let k = Text.pack $ symbolVal (proxyAssocKey m) in Field <$> parseUnique k form

instance FromHttpApiData a => FromHttpApiData (Identity a) where
  parseUrlPiece = fmap pure . parseUrlPiece
