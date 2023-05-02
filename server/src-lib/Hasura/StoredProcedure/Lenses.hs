{-# LANGUAGE TemplateHaskell #-}

module Hasura.StoredProcedure.Lenses
  ( spiStoredProcedure,
    spiGraphqlName,
    spiConfig,
    spiArrayRelationships,
    spiReturns,
    spiArguments,
    spiDescription,
  )
where

import Control.Lens (makeLenses)
import Hasura.StoredProcedure.Cache (StoredProcedureInfo (..))

makeLenses ''StoredProcedureInfo
