{-# LANGUAGE ScopedTypeVariables #-}

-- | The RQL IR representation of an invocation of a native query.
module Hasura.NativeQuery.IR
  ( NativeQuery (..),
  )
where

import Data.Aeson (ToJSON (toEncoding), defaultOptions, genericToEncoding)
import Hasura.LogicalModel.IR
import Hasura.NativeQuery.Metadata
import Hasura.Prelude
import Hasura.RQL.Types.Backend

-- | The RQL IR representation of an invocation of a native query.
data NativeQuery b field = NativeQuery
  { -- | The graphql name of the native query.
    nqRootFieldName :: NativeQueryName,
    -- | The raw sql to use in the query
    nqInterpolatedQuery :: InterpolatedQuery field,
    -- | The return type of the native query
    nqLogicalModel :: LogicalModel b
  }
  deriving (Functor, Foldable, Traversable, Generic)

deriving instance (Backend b, Eq field) => Eq (NativeQuery b field)

deriving instance (Backend b, Show field) => Show (NativeQuery b field)

instance (Backend b, ToJSON field, field ~ ArgumentName) => ToJSON (NativeQuery b field) where
  toEncoding = genericToEncoding defaultOptions
