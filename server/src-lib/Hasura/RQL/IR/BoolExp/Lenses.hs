{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- `makeWrapped` creates orphans

module Hasura.RQL.IR.BoolExp.Lenses
  ( geWhere,
    geTable,
    _BoolExists,
    _NoRedaction,
    _RedactIfFalse,
  )
where

import Control.Lens.TH (makeLenses, makePrisms, makeWrapped)
import Hasura.RQL.IR.BoolExp (AnnRedactionExp (..), BoolExp (..), GBoolExp (..), GExists (..))

makeLenses ''GExists

-- this is why we need Orphan instances enabled, as this creates orphan
-- `Control.Lens.Wrapped.Wrapped` instances
makeWrapped ''BoolExp

makePrisms ''GBoolExp
makePrisms ''AnnRedactionExp
