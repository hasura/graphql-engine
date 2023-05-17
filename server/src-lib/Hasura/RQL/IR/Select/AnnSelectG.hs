{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

-- | More leaves from `RQL.IR.Select`
module Hasura.RQL.IR.Select.AnnSelectG
  ( AnnSelectG (..),
    AnnSelectStreamG (..),
    bifoldMapAnnSelectStreamG,
    bifoldMapAnnSelectG,
  )
where

import Data.Bifoldable
import Data.Kind (Type)
import Hasura.Prelude
import Hasura.RQL.IR.Select.Args
import Hasura.RQL.IR.Select.From
import Hasura.RQL.IR.Select.TablePerm
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.NamingCase (NamingCase)
import Hasura.RQL.Types.Schema.Options (StringifyNumbers)

-- Select

data AnnSelectG (b :: BackendType) (f :: Type -> Type) (v :: Type) = AnnSelectG
  { _asnFields :: Fields (f v),
    _asnFrom :: SelectFromG b v,
    _asnPerm :: TablePermG b v,
    _asnArgs :: SelectArgsG b v,
    _asnStrfyNum :: StringifyNumbers,
    _asnNamingConvention :: Maybe NamingCase
  }
  deriving stock (Functor, Foldable, Traversable)

deriving stock instance
  ( Backend b,
    Eq (Fields (f v)),
    Eq (SelectArgsG b v),
    Eq (SelectFromG b v),
    Eq (TablePermG b v)
  ) =>
  Eq (AnnSelectG b f v)

deriving stock instance
  ( Backend b,
    Show (Fields (f v)),
    Show (SelectArgsG b v),
    Show (SelectFromG b v),
    Show (TablePermG b v)
  ) =>
  Show (AnnSelectG b f v)

-- | IR type representing nodes for streaming subscriptions
data
  AnnSelectStreamG
    (b :: BackendType)
    (f :: Type -> Type)
    (v :: Type) = AnnSelectStreamG
  { -- | type to indicate if streaming subscription has been enabled in the `BackendType`.
    --   This type helps avoiding missing case match patterns for backends where it's disabled.
    _assnXStreamingSubscription :: XStreamingSubscription b,
    -- | output selection fields
    _assnFields :: Fields (f v),
    -- | table information to select from
    _assnFrom :: SelectFromG b v,
    -- | select permissions
    _assnPerm :: TablePermG b v,
    -- | streaming arguments
    _assnArgs :: SelectStreamArgsG b v,
    _assnStrfyNum :: StringifyNumbers
  }
  deriving (Functor, Foldable, Traversable)

deriving instance
  ( Backend b,
    Eq (SelectFromG b v),
    Eq (TablePermG b v),
    Eq (SelectStreamArgsG b v),
    Eq (f v)
  ) =>
  Eq (AnnSelectStreamG b f v)

deriving instance
  ( Backend b,
    Show (SelectFromG b v),
    Show (TablePermG b v),
    Show (SelectStreamArgsG b v),
    Show (f v)
  ) =>
  Show (AnnSelectStreamG b f v)

-- | We can't write a Bifoldable instance for AnnSelectG because the types don't line up.
-- Instead, we provide this function which can be used to help define Bifoldable instances of other types
-- containing AnnSelectG values.
bifoldMapAnnSelectG :: (Backend b, Bifoldable (f b), Monoid m) => (r -> m) -> (v -> m) -> AnnSelectG b (f b r) v -> m
bifoldMapAnnSelectG f g AnnSelectG {..} =
  foldMap (foldMap $ bifoldMap f g) _asnFields
    <> foldMap g _asnFrom
    <> foldMap g _asnPerm
    <> foldMap g _asnArgs

bifoldMapAnnSelectStreamG :: (Backend b, Bifoldable (f b), Monoid m) => (r -> m) -> (v -> m) -> AnnSelectStreamG b (f b r) v -> m
bifoldMapAnnSelectStreamG f g AnnSelectStreamG {..} =
  foldMap (foldMap $ bifoldMap f g) _assnFields
    <> foldMap g _assnFrom
    <> foldMap g _assnPerm
    <> foldMap g _assnArgs
