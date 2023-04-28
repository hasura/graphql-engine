{-# LANGUAGE TemplateHaskell #-}

module Hasura.StoredProcedure.Lenses
  ( spiRootFieldName,
    spiArrayRelationships,
    spiCode,
    spiReturns,
    spiArguments,
    spiDescription,
  )
where

import Control.Lens (makeLenses)
import Hasura.StoredProcedure.Cache (StoredProcedureInfo (..))

makeLenses ''StoredProcedureInfo
