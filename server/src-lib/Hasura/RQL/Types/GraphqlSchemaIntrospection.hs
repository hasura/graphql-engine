{-# LANGUAGE TemplateHaskell #-}

module Hasura.RQL.Types.GraphqlSchemaIntrospection
  ( SetGraphqlIntrospectionOptions (..),
  )
where

import Autodocodec (HasCodec (codec), dimapCodec)
import Autodocodec.Extended (hashSetCodec)
import Data.Aeson.TH
import Data.HashSet qualified as Set
import Hasura.Prelude
import Hasura.Session

newtype SetGraphqlIntrospectionOptions = SetGraphqlIntrospectionOptions {_idrDisabledForRoles :: (Set.HashSet RoleName)}
  deriving (Show, Eq, Generic, Semigroup, Monoid)

instance NFData SetGraphqlIntrospectionOptions

instance Hashable SetGraphqlIntrospectionOptions

instance HasCodec SetGraphqlIntrospectionOptions where
  codec = dimapCodec SetGraphqlIntrospectionOptions _idrDisabledForRoles hashSetCodec

$(deriveJSON hasuraJSON ''SetGraphqlIntrospectionOptions)
