module Hasura.RQL.Types.Permission where

import           Hasura.Prelude

import qualified Data.Text                      as T
import qualified Database.PG.Query              as Q
import qualified PostgreSQL.Binary.Decoding     as PD

import           Control.Lens                   (makeLenses)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Hashable

import           Hasura.Incremental             (Cacheable)
import           Hasura.RQL.IR.BoolExp
import           Hasura.RQL.Types.Backend
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.ComputedField
import           Hasura.SQL.Backend
import           Hasura.Session

data PermType
  = PTInsert
  | PTSelect
  | PTUpdate
  | PTDelete
  deriving (Eq, Generic)
instance NFData PermType
instance Cacheable PermType

instance Q.FromCol PermType where
  fromCol bs = flip Q.fromColHelper bs $ PD.enum $ \case
    "insert" -> Just PTInsert
    "update" -> Just PTUpdate
    "select" -> Just PTSelect
    "delete" -> Just PTDelete
    _        -> Nothing

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
  parseJSON x            = PCCols <$> parseJSON x

instance (Backend b) => ToJSON (PermColSpec b) where
  toJSON (PCCols cols) = toJSON cols
  toJSON PCStar        = "*"

data PermDef a =
  PermDef
  { _pdRole       :: !RoleName
  , _pdPermission :: !a
  , _pdComment    :: !(Maybe T.Text)
  } deriving (Show, Eq, Generic)
instance (Cacheable a) => Cacheable (PermDef a)
$(deriveFromJSON hasuraJSON{omitNothingFields=True} ''PermDef)
$(makeLenses ''PermDef)

instance (ToJSON a) => ToJSON (PermDef a) where
  toJSON = object . toAesonPairs

instance (ToJSON a) => ToAesonPairs (PermDef a) where
 toAesonPairs (PermDef rn perm comment) =
  [ "role" .= rn
  , "permission" .= perm
  , "comment" .= comment
  ]

-- Insert permission
data InsPerm (b :: BackendType)
  = InsPerm
  { ipCheck       :: !(BoolExp b )
  , ipSet         :: !(Maybe (ColumnValues b Value))
  , ipColumns     :: !(Maybe (PermColSpec b))
  , ipBackendOnly :: !(Maybe Bool) -- see Note [Backend only permissions]
  } deriving (Show, Eq, Generic)
instance Backend b => Cacheable (InsPerm b)
instance Backend b => FromJSON (InsPerm b) where
  parseJSON = genericParseJSON hasuraJSON{omitNothingFields=True}
instance Backend b => ToJSON (InsPerm b) where
  toJSON = genericToJSON hasuraJSON{omitNothingFields=True}

type InsPermDef b = PermDef (InsPerm b)

-- Select constraint
data SelPerm (b :: BackendType)
  = SelPerm
  { spColumns           :: !(PermColSpec b)     -- ^ Allowed columns
  , spFilter            :: !(BoolExp b)         -- ^ Filter expression
  , spLimit             :: !(Maybe Int)         -- ^ Limit value
  , spAllowAggregations :: !Bool                -- ^ Allow aggregation
  , spComputedFields    :: ![ComputedFieldName] -- ^ Allowed computed fields
  } deriving (Show, Eq, Generic)
instance Backend b => Cacheable (SelPerm b)
instance Backend b => ToJSON (SelPerm b) where
  toJSON = genericToJSON hasuraJSON{omitNothingFields=True}

instance Backend b => FromJSON (SelPerm b) where
  parseJSON = withObject "SelPerm" $ \o ->
    SelPerm
    <$> o .: "columns"
    <*> o .: "filter"
    <*> o .:? "limit"
    <*> o .:? "allow_aggregations" .!= False
    <*> o .:? "computed_fields" .!= []

type SelPermDef b = PermDef (SelPerm b)

-- Delete permission
data DelPerm (b :: BackendType)
  = DelPerm { dcFilter :: !(BoolExp b) }
  deriving (Show, Eq, Generic)
instance Backend b => Cacheable (DelPerm b)
instance Backend b => FromJSON (DelPerm b) where
  parseJSON = genericParseJSON hasuraJSON{omitNothingFields=True}
instance Backend b => ToJSON (DelPerm b) where
  toJSON = genericToJSON hasuraJSON{omitNothingFields=True}

type DelPermDef b = PermDef (DelPerm b)

-- Update constraint
data UpdPerm (b :: BackendType)
  = UpdPerm
  { ucColumns :: !(PermColSpec b) -- Allowed columns
  , ucSet     :: !(Maybe (ColumnValues b Value)) -- Preset columns
  , ucFilter  :: !(BoolExp b) -- Filter expression (applied before update)
  , ucCheck   :: !(Maybe (BoolExp b))
  -- ^ Check expression, which must be true after update.
  -- This is optional because we don't want to break the v1 API
  -- but Nothing should be equivalent to the expression which always
  -- returns true.
  } deriving (Show, Eq, Generic)
instance Backend b => Cacheable (UpdPerm b)
instance Backend b => FromJSON (UpdPerm b) where
  parseJSON = genericParseJSON hasuraJSON{omitNothingFields=True}
instance Backend b => ToJSON (UpdPerm b) where
  toJSON = genericToJSON hasuraJSON{omitNothingFields=True}

type UpdPermDef b = PermDef (UpdPerm b)
