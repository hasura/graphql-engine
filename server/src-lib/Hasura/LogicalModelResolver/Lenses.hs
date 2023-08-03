{-# LANGUAGE TemplateHaskell #-}

module Hasura.LogicalModelResolver.Lenses
  ( ilmmFields,
    ilmmSelectPermissions,
    _LMIInlineLogicalModel,
    _LMILogicalModelName,
  )
where

import Control.Lens (makeLenses, makePrisms)
import Hasura.LogicalModelResolver.Metadata (InlineLogicalModelMetadata, LogicalModelIdentifier)

makeLenses ''InlineLogicalModelMetadata
makePrisms ''LogicalModelIdentifier
