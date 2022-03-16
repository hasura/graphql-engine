{-# LANGUAGE TemplateHaskell #-}

module Hasura.RQL.IR.Update
  ( AnnotatedUpdate,
    AnnotatedUpdateG (..),
    auTable,
    auWhere,
    auCheck,
    auBackend,
    auOutput,
    auAllCols,
  )
where

import Control.Lens.TH (makeLenses)
import Data.Kind (Type)
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.Returning
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
import Hasura.SQL.Backend

data AnnotatedUpdateG (b :: BackendType) (r :: Type) v = AnnotatedUpdateG
  { _auTable :: !(TableName b),
    _auWhere :: !(AnnBoolExp b v, AnnBoolExp b v),
    _auCheck :: !(AnnBoolExp b v),
    -- | All the backend-specific data related to an update mutation
    _auBackend :: BackendUpdate b v,
    -- we don't prepare the arguments for returning
    -- however the session variable can still be
    -- converted as desired
    _auOutput :: !(MutationOutputG b r v),
    _auAllCols :: ![ColumnInfo b]
  }
  deriving (Functor, Foldable, Traversable)

type AnnotatedUpdate b = AnnotatedUpdateG b Void (SQLExpression b)

$(makeLenses ''AnnotatedUpdateG)
