{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Elm
  ( ElmType(..)
  , ToElmValue (..)
  , elmTypeToElmValue
  , toElmRecordType
  ) where

import           RIO

import           Data.Extensible
import           Elm
import           GHC.TypeLits    (KnownSymbol, symbolVal)

class ToElmValue a where
  toElmValue :: a -> ElmValue

instance ElmType a => ElmType (Identity a) where
  toElmType (Identity a) = toElmType a

elmTypeToElmValue :: ElmDatatype -> ElmValue
elmTypeToElmValue (ElmDatatype name _) = ElmRef name
elmTypeToElmValue (ElmPrimitive prim)  = ElmPrimitiveRef prim

toElmRecordType :: ToElmValue a => Text -> a -> ElmDatatype
toElmRecordType name = ElmDatatype name . RecordConstructor name . toElmValue

instance Forall (KeyValue KnownSymbol ElmType) xs => ToElmValue (Record xs) where
  toElmValue = hfoldrWithIndexFor
    (Proxy @ (KeyValue KnownSymbol ElmType))
    (\k v m -> toElmField k v `append` m)
    ElmEmpty
    where
      append :: ElmValue -> ElmValue -> ElmValue
      append ElmEmpty a        = a
      append a ElmEmpty        = a
      append (Values a1 a2) a3 = Values a1 $ append a2 a3
      append a1 a2             = Values a1 a2
      toElmField k v = ElmField
        (fromString . symbolVal $ proxyAssocKey k)
        (elmTypeToElmValue . toElmType $ getField v)
