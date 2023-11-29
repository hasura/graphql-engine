{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.IR.Delete
  ( AnnDel,
    AnnDelG (..),
    adTable,
    adWhere,
    adOutput,
    adAllCols,
    adNamingConvention,
    adValidateInput,
    adIsDeleteByPk,
  )
where

import Control.Lens.TH (makeLenses)
import Data.Kind (Type)
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.Returning
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.NamingCase (NamingCase)
import Hasura.RQL.Types.Permission

data AnnDelG (b :: BackendType) (r :: Type) v = AnnDel
  { _adTable :: TableName b,
    _adWhere :: (AnnBoolExp b v, AnnBoolExp b v),
    _adOutput :: MutationOutputG b r v,
    _adAllCols :: [ColumnInfo b],
    _adNamingConvention :: Maybe NamingCase,
    _adValidateInput :: Maybe (ValidateInput ResolvedWebhook),
    _adIsDeleteByPk :: Bool
  }
  deriving (Functor, Foldable, Traversable)

type AnnDel b = AnnDelG b Void (SQLExpression b)

deriving instance
  ( Backend b,
    Show (AnnBoolExp b a),
    Show (MutationOutputG b r a),
    Show a
  ) =>
  Show (AnnDelG b r a)

$(makeLenses ''AnnDelG)
