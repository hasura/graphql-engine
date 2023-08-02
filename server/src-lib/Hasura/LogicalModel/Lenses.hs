{-# LANGUAGE TemplateHaskell #-}

module Hasura.LogicalModel.Lenses
  ( lmiName,
    lmiPermissions,
    lmiDescription,
    lmiFields,
    lmmName,
    lmmFields,
    lmmDescription,
    lmmSelectPermissions,
  )
where

import Control.Lens (makeLenses)
import Hasura.LogicalModel.Cache (LogicalModelInfo (..))
import Hasura.LogicalModel.Metadata (LogicalModelMetadata (..))

makeLenses ''LogicalModelInfo
makeLenses ''LogicalModelMetadata
