{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.IR.Update
  ( AnnotatedUpdate,
    AnnotatedUpdateG (..),
    auTable,
    auUpdatePermissions,
    auCheck,
    auUpdateVariant,
    auOutput,
    auAllCols,
    auNamingConvention,
    auValidateInput,
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

--------------------------------------------------------------------------------

data AnnotatedUpdateG (b :: BackendType) (r :: Type) v = AnnotatedUpdateG
  { _auTable :: TableName b,
    _auUpdatePermissions :: AnnBoolExp b v,
    _auCheck :: AnnBoolExp b v,
    _auUpdateVariant :: UpdateVariant b v,
    -- we don't prepare the arguments for returning
    -- however the session variable can still be
    -- converted as desired

    -- | Selection set
    _auOutput :: MutationOutputG b r v,
    _auAllCols :: [ColumnInfo b],
    _auNamingConvention :: Maybe NamingCase,
    _auValidateInput :: Maybe (ValidateInput ResolvedWebhook)
  }
  deriving stock (Functor, Foldable, Traversable)

deriving stock instance
  ( Backend b,
    Show v,
    Show r,
    Show (AnnBoolExp b v),
    Show (UpdateVariant b v),
    Show (MutationOutputG b r v)
  ) =>
  Show (AnnotatedUpdateG b r v)

deriving stock instance
  ( Backend b,
    Eq v,
    Eq r,
    Eq (AnnBoolExp b v),
    Eq (UpdateVariant b v),
    Eq (MutationOutputG b r v)
  ) =>
  Eq (AnnotatedUpdateG b r v)

type AnnotatedUpdate b = AnnotatedUpdateG b Void (SQLExpression b)

$(makeLenses ''AnnotatedUpdateG)
