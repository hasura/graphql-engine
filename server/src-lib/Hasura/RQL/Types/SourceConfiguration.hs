{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.Types.SourceConfiguration
  ( HasSourceConfiguration (..),
    Representable,
  )
where

import Autodocodec (HasCodec)
import Data.Aeson.Extended
import Data.Has
import Data.Kind (Type)
import Hasura.Prelude
import Hasura.RQL.Types.BackendTag
import Hasura.RQL.Types.BackendType

type Representable a = (Show a, Eq a, Hashable a, NFData a)

class
  ( Representable (SourceConnConfiguration b),
    HasCodec (SourceConnConfiguration b),
    FromJSON (SourceConnConfiguration b),
    ToJSON (SourceConfig b),
    ToJSON (SourceConnConfiguration b),
    Eq (SourceConfig b),
    Has (ScalarTypeParsingContext b) (SourceConfig b),
    Eq (ScalarTypeParsingContext b),
    Ord (ScalarTypeParsingContext b),
    HasTag b
  ) =>
  HasSourceConfiguration (b :: BackendType)
  where
  -- types

  -- Extra context required for parsing values of the backend's scalar types
  type ScalarTypeParsingContext b
  type ScalarTypeParsingContext b = ()

  -- | User facing connection configuration for a database.
  type SourceConnConfiguration b :: Type

  -- | Internal connection configuration for a database - connection string,
  -- connection pool etc
  type SourceConfig b :: Type

  -- | The number of read replicas specified in the source configuration
  sourceConfigNumReadReplicas :: SourceConfig b -> Int

  -- | Whether the source configuration specifies the use of a connection
  -- template
  sourceConfigConnectonTemplateEnabled :: SourceConfig b -> Bool

  -- | Whether or not the source supports performing column redaction.
  -- See note [SQL generation for inherited roles] for more information
  sourceSupportsColumnRedaction :: SourceConfig b -> Bool

  sourceConfigBackendSourceKind :: SourceConfig b -> BackendSourceKind b
