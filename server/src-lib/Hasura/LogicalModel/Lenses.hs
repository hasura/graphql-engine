{-# LANGUAGE TemplateHaskell #-}

module Hasura.LogicalModel.Lenses
  ( lmiName,
    lmiPermissions,
    lmiDescription,
    lmiFields,
  )
where

import Control.Lens (makeLenses)
import Hasura.LogicalModel.Cache (LogicalModelInfo (..))

makeLenses ''LogicalModelInfo
