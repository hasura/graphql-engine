{-# LANGUAGE TemplateHaskell #-}

module Hasura.RQL.DDL.Action.Lenses
  ( caName,
    caDefinition,
    caComment,
    uaName,
    uaDefinition,
    uaComment,
  )
where

import Control.Lens (makeLenses)
import Hasura.RQL.DDL.Action (CreateAction (..), UpdateAction)

$(makeLenses ''CreateAction)

$(makeLenses ''UpdateAction)
