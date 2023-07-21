{-# LANGUAGE TemplateHaskell #-}

module Hasura.NativeQuery.Lenses
  ( nqiRootFieldName,
    nqiRelationships,
    nqiCode,
    nqiReturns,
    nqiArguments,
    nqiDescription,
    nqmArguments,
    nqmObjectRelationships,
    nqmCode,
    nqmDescription,
    nqmReturns,
    nqmArrayRelationships,
    nqmRootFieldName,
  )
where

import Control.Lens (makeLenses)
import Hasura.NativeQuery.Cache (NativeQueryInfo (..))
import Hasura.NativeQuery.Metadata (NativeQueryMetadata (..))

makeLenses ''NativeQueryMetadata
makeLenses ''NativeQueryInfo
