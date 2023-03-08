{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.Types.SourceConfiguration
  ( HasSourceConfiguration (..),
    Representable,
  )
where

import Autodocodec (HasCodec)
import Data.Aeson.Extended
import Data.Kind (Type)
import Hasura.Prelude
import Hasura.SQL.Backend
import Hasura.SQL.Tag

type Representable a = (Show a, Eq a, Hashable a, NFData a)

class
  ( Representable (SourceConnConfiguration b),
    HasCodec (SourceConnConfiguration b),
    FromJSON (SourceConnConfiguration b),
    ToJSON (SourceConfig b),
    ToJSON (SourceConnConfiguration b),
    Eq (SourceConfig b),
    HasTag b
  ) =>
  HasSourceConfiguration (b :: BackendType)
  where
  -- types

  -- | User facing connection configuration for a database.
  type SourceConnConfiguration b :: Type

  -- | Internal connection configuration for a database - connection string,
  -- connection pool etc
  type SourceConfig b :: Type
