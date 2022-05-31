{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.Types.Permission
  ( DelPerm (..),
    DelPermDef,
    InsPerm (..),
    InsPermDef,
    PermColSpec (..),
    PermDef (..),
    PermType (..),
    SelPerm (..),
    SelPermDef,
    UpdPerm (..),
    UpdPermDef,
    pdComment,
    pdPermission,
    pdRole,
    permTypeToCode,
    PermDefPermission (..),
    unPermDefPermission,
    reflectPermDefPermission,
  )
where

import Control.Lens (makeLenses)
import Data.Aeson
import Data.Aeson.TH
import Data.Hashable
import Data.Kind (Type)
import Data.Text qualified as T
import Database.PG.Query qualified as Q
import Hasura.Incremental (Cacheable (..))
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.SQL.Backend
import Hasura.Session
import PostgreSQL.Binary.Decoding qualified as PD

data PermType
  = PTInsert
  | PTSelect
  | PTUpdate
  | PTDelete
  deriving (Eq, Generic)

instance NFData PermType

instance Cacheable PermType

instance Q.FromCol PermType where
  fromCol bs = flip Q.fromColHelper bs $
    PD.enum $ \case
      "insert" -> Just PTInsert
      "update" -> Just PTUpdate
      "select" -> Just PTSelect
      "delete" -> Just PTDelete
      _ -> Nothing

permTypeToCode :: PermType -> Text
permTypeToCode PTInsert = "insert"
permTypeToCode PTSelect = "select"
permTypeToCode PTUpdate = "update"
permTypeToCode PTDelete = "delete"

instance Hashable PermType where
  hashWithSalt salt a = hashWithSalt salt $ permTypeToCode a

instance Show PermType where
  show PTInsert = "insert"
  show PTSelect = "select"
  show PTUpdate = "update"
  show PTDelete = "delete"

instance FromJSON PermType where
  parseJSON (String "insert") = return PTInsert
  parseJSON (String "select") = return PTSelect
  parseJSON (String "update") = return PTUpdate
  parseJSON (String "delete") = return PTDelete
  parseJSON _ =
    fail "perm_type should be one of 'insert', 'select', 'update', 'delete'"

instance ToJSON PermType where
  toJSON = String . permTypeToCode

data PermColSpec b
  = PCStar
  | PCCols ![Column b]
  deriving (Generic)

deriving instance (Backend b) => Show (PermColSpec b)

deriving instance (Backend b) => Eq (PermColSpec b)

instance (Backend b) => Cacheable (PermColSpec b)

instance (Backend b) => FromJSON (PermColSpec b) where
  parseJSON (String "*") = return PCStar
  parseJSON x = PCCols <$> parseJSON x

instance (Backend b) => ToJSON (PermColSpec b) where
  toJSON (PCCols cols) = toJSON cols
  toJSON PCStar = "*"

data PermDef (b :: BackendType) (perm :: BackendType -> Type) = PermDef
  { _pdRole :: !RoleName,
    _pdPermission :: !(PermDefPermission b perm),
    _pdComment :: !(Maybe T.Text)
  }
  deriving (Show, Eq, Generic)

instance (Backend b, Cacheable (perm b)) => Cacheable (PermDef b perm)

-- | The permission data as it appears in a 'PermDef'.
-- Since this type is a GADT it facilitates that values which are polymorphic
-- may re-discover its specific type of permission by case analysis.
--
-- The fact that permission types are tracked in types are more accidental than
-- intentional and something we want to move away from, see
-- https://github.com/hasura/graphql-engine-mono/issues/4076.
data PermDefPermission (b :: BackendType) (perm :: BackendType -> Type) where
  SelPerm' :: SelPerm b -> PermDefPermission b SelPerm
  InsPerm' :: InsPerm b -> PermDefPermission b InsPerm
  UpdPerm' :: UpdPerm b -> PermDefPermission b UpdPerm
  DelPerm' :: DelPerm b -> PermDefPermission b DelPerm

instance Backend b => FromJSON (PermDefPermission b SelPerm) where
  parseJSON = fmap SelPerm' . parseJSON

instance Backend b => FromJSON (PermDefPermission b InsPerm) where
  parseJSON = fmap InsPerm' . parseJSON

instance Backend b => FromJSON (PermDefPermission b UpdPerm) where
  parseJSON = fmap UpdPerm' . parseJSON

instance Backend b => FromJSON (PermDefPermission b DelPerm) where
  parseJSON = fmap DelPerm' . parseJSON

instance Backend b => ToJSON (PermDefPermission b perm) where
  toJSON = \case
    SelPerm' p -> toJSON p
    InsPerm' p -> toJSON p
    UpdPerm' p -> toJSON p
    DelPerm' p -> toJSON p

deriving stock instance Backend b => Show (PermDefPermission b perm)

deriving stock instance Backend b => Eq (PermDefPermission b perm)

instance Backend b => Cacheable (PermDefPermission b perm) where
  unchanged accesses (SelPerm' p1) (SelPerm' p2) = unchanged accesses p1 p2
  unchanged accesses (InsPerm' p1) (InsPerm' p2) = unchanged accesses p1 p2
  unchanged accesses (UpdPerm' p1) (UpdPerm' p2) = unchanged accesses p1 p2
  unchanged accesses (DelPerm' p1) (DelPerm' p2) = unchanged accesses p1 p2

-----------------------------

unPermDefPermission :: PermDefPermission b perm -> perm b
unPermDefPermission = \case
  SelPerm' p -> p
  InsPerm' p -> p
  UpdPerm' p -> p
  DelPerm' p -> p

reflectPermDefPermission :: PermDefPermission b a -> PermType
reflectPermDefPermission = \case
  SelPerm' _ -> PTSelect
  InsPerm' _ -> PTInsert
  UpdPerm' _ -> PTUpdate
  DelPerm' _ -> PTDelete

instance (Backend b, ToJSON (perm b)) => ToJSON (PermDef b perm) where
  toJSON = object . toAesonPairs

instance Backend b => ToAesonPairs (PermDef b perm) where
  toAesonPairs (PermDef rn perm comment) =
    [ "role" .= rn,
      "permission" .= perm,
      "comment" .= comment
    ]

-- Insert permission
data InsPerm (b :: BackendType) = InsPerm
  { ipCheck :: !(BoolExp b),
    ipSet :: !(Maybe (ColumnValues b Value)),
    ipColumns :: !(Maybe (PermColSpec b)),
    ipBackendOnly :: !Bool -- see Note [Backend only permissions]
  }
  deriving (Show, Eq, Generic)

instance Backend b => Cacheable (InsPerm b)

instance Backend b => FromJSON (InsPerm b) where
  parseJSON = withObject "InsPerm" $ \o ->
    InsPerm
      <$> o .: "check"
      <*> o .:? "set"
      <*> o .:? "columns"
      <*> o .:? "backend_only" .!= False

instance Backend b => ToJSON (InsPerm b) where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}

type InsPermDef b = PermDef b InsPerm

-- Select constraint
data SelPerm (b :: BackendType) = SelPerm
  { -- | Allowed columns
    spColumns :: !(PermColSpec b),
    -- | Filter expression
    spFilter :: !(BoolExp b),
    -- | Limit value
    spLimit :: !(Maybe Int),
    -- | Allow aggregation
    spAllowAggregations :: !Bool,
    -- | Allowed computed fields
    spComputedFields :: ![ComputedFieldName]
  }
  deriving (Show, Eq, Generic)

instance Backend b => Cacheable (SelPerm b)

instance Backend b => ToJSON (SelPerm b) where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}

instance Backend b => FromJSON (SelPerm b) where
  parseJSON =
    withObject "SelPerm" $ \o ->
      SelPerm
        <$> o .: "columns"
        <*> o .: "filter"
        <*> o .:? "limit"
        <*> o .:? "allow_aggregations" .!= False
        <*> o .:? "computed_fields" .!= []

type SelPermDef b = PermDef b SelPerm

-- Delete permission
data DelPerm (b :: BackendType) = DelPerm
  { dcFilter :: !(BoolExp b),
    dcBackendOnly :: !Bool -- see Note [Backend only permissions]
  }
  deriving (Show, Eq, Generic)

instance Backend b => Cacheable (DelPerm b)

instance Backend b => FromJSON (DelPerm b) where
  parseJSON = withObject "DelPerm" $ \o ->
    DelPerm
      <$> o .: "filter"
      <*> o .:? "backend_only" .!= False

instance Backend b => ToJSON (DelPerm b) where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}

type DelPermDef b = PermDef b DelPerm

-- Update constraint
data UpdPerm (b :: BackendType) = UpdPerm
  { ucColumns :: !(PermColSpec b), -- Allowed columns
    ucSet :: !(Maybe (ColumnValues b Value)), -- Preset columns
    ucFilter :: !(BoolExp b), -- Filter expression (applied before update)

    -- | Check expression, which must be true after update.
    -- This is optional because we don't want to break the v1 API
    -- but Nothing should be equivalent to the expression which always
    -- returns true.
    ucCheck :: !(Maybe (BoolExp b)),
    ucBackendOnly :: !Bool -- see Note [Backend only permissions]
  }
  deriving (Show, Eq, Generic)

instance Backend b => Cacheable (UpdPerm b)

instance Backend b => FromJSON (UpdPerm b) where
  parseJSON = withObject "UpdPerm" $ \o ->
    UpdPerm
      <$> o .: "columns"
      <*> o .:? "set"
      <*> o .: "filter"
      <*> o .:? "check"
      <*> o .:? "backend_only" .!= False

instance Backend b => ToJSON (UpdPerm b) where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}

type UpdPermDef b = PermDef b UpdPerm

-- The Expression-level TemplateHaskell splices below fail unless there is a
-- declaration-level splice before to ensure phase separation.
-- See https://gitlab.haskell.org/ghc/ghc/-/issues/9813
$(return [])

instance Backend b => FromJSON (PermDef b SelPerm) where
  parseJSON = $(mkParseJSON hasuraJSON ''PermDef)

instance Backend b => FromJSON (PermDef b InsPerm) where
  parseJSON = $(mkParseJSON hasuraJSON ''PermDef)

instance Backend b => FromJSON (PermDef b UpdPerm) where
  parseJSON = $(mkParseJSON hasuraJSON ''PermDef)

instance Backend b => FromJSON (PermDef b DelPerm) where
  parseJSON = $(mkParseJSON hasuraJSON ''PermDef)

$(makeLenses ''PermDef)
