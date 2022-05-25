{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.IR.Delete
  ( AnnDel,
    AnnDelG (..),
    adTable,
    adWhere,
    adOutput,
    adAllCols,
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

data AnnDelG (b :: BackendType) (r :: Type) v = AnnDel
  { _adTable :: TableName b,
    _adWhere :: (AnnBoolExp b v, AnnBoolExp b v),
    _adOutput :: MutationOutputG b r v,
    _adAllCols :: [ColumnInfo b]
  }
  deriving (Functor, Foldable, Traversable)

type AnnDel b = AnnDelG b Void (SQLExpression b)

deriving instance (Show (MutationOutputG b r a), Backend b, Show (BooleanOperators b a), Show (FunctionArgumentExp b a), Show a) => Show (AnnDelG b r a)

$(makeLenses ''AnnDelG)
