{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Orphans () where

import           RIO

import           Control.Monad.Error.Class (throwError)
import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Extensible
import           Elm.Mapping
import qualified Servant.Auth.Server       as Auth
import           Servant.Server            (runHandler)
import           Web.FormUrlEncoded
import           Web.HttpApiData

instance Forall (KeyTargetAre KnownSymbol FromHttpApiData) xs => FromForm (Record xs) where
  fromForm form =
    hgenerateFor (Proxy @ (KeyTargetAre KnownSymbol FromHttpApiData)) $ \m ->
      Field <$> parseUnique (stringKeyOf m) form

instance IsElmType Int64 where
  compileElmType _ = toElmType (Proxy @ Int)

instance ToJSON (xs :& Field h) => Auth.ToJWT (xs :& Field h)
instance FromJSON (xs :& Field h) => Auth.FromJWT (xs :& Field h)

instance Auth.ThrowAll (RIO env a) where
  throwAll err = liftIO (runHandler $ throwError err) >>= \case
    Right a -> pure a
    Left e  -> throwIO e
