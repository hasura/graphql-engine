{-# LANGUAGE TemplateHaskell #-}

module Hasura.NativeQuery.Lenses
  ( nqiRootFieldName,
    nqiRelationships,
    nqiCode,
    nqiReturns,
    nqiArguments,
    nqiDescription,
  )
where

import Control.Lens (makeLenses)
import Hasura.NativeQuery.Cache (NativeQueryInfo (..))

makeLenses ''NativeQueryInfo
