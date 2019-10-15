{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Orphans () where

import           RIO

import           Data.Extensible
import           Web.FormUrlEncoded
import           Web.HttpApiData

instance Forall (KeyTargetAre KnownSymbol FromHttpApiData) xs => FromForm (Record xs) where
  fromForm form =
    hgenerateFor (Proxy @ (KeyTargetAre KnownSymbol FromHttpApiData)) $ \m ->
      Field <$> parseUnique (stringKeyOf m) form

instance FromHttpApiData a => FromHttpApiData (Identity a) where
  parseUrlPiece = fmap pure . parseUrlPiece
