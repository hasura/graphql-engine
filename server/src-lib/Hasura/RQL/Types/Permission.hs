module Hasura.RQL.Types.Permission where

import           Hasura.Backends.Postgres.SQL.Types (PGCol, TableName, getTableTxt)
import           Hasura.Incremental                 (Cacheable)
import           Hasura.Prelude
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.ComputedField
import           Hasura.RQL.Types.DML
import           Hasura.Session
import           Hasura.SQL.Backend

import           Control.Lens                       (makeLenses)
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Hashable
import           Instances.TH.Lift                  ()
import           Language.Haskell.TH.Syntax         (Lift)

import qualified Data.Text                          as T
import qualified Database.PG.Query                  as Q
import qualified PostgreSQL.Binary.Decoding         as PD

data PermType
  = PTInsert
  | PTSelect
  | PTUpdate
  | PTDelete
  deriving (Eq, Lift, Generic)
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

data PermId
  = PermId
  { pidTable :: !TableName
  , pidRole  :: !RoleName
  , pidType  :: !PermType
  } deriving (Eq)

instance Show PermId where
  show (PermId tn rn pType) =
    show $ mconcat
    [ getTableTxt tn
    , "."
    , roleNameToTxt rn
    , "."
    , T.pack $ show pType
    ]

data PermColSpec
  = PCStar
  | PCCols ![PGCol]
  deriving (Show, Eq, Lift, Generic)
instance Cacheable PermColSpec

instance FromJSON PermColSpec where
  parseJSON (String "*") = return PCStar
  parseJSON x            = PCCols <$> parseJSON x

instance ToJSON PermColSpec where
  toJSON (PCCols cols) = toJSON cols
  toJSON PCStar        = "*"

data PermDef a =
  PermDef
  { _pdRole       :: !RoleName
  , _pdPermission :: !a
  , _pdComment    :: !(Maybe T.Text)
  } deriving (Show, Eq, Lift, Generic)
instance (Cacheable a) => Cacheable (PermDef a)
$(deriveFromJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''PermDef)
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
data InsPerm (b :: Backend)
  = InsPerm
  { ipCheck       :: !(BoolExp b)
  , ipSet         :: !(Maybe (ColumnValues Value))
  , ipColumns     :: !(Maybe PermColSpec)
  , ipBackendOnly :: !(Maybe Bool) -- see Note [Backend only permissions]
  } deriving (Show, Eq, Lift, Generic)
instance Cacheable (InsPerm 'Postgres)
instance FromJSON (InsPerm 'Postgres) where
  parseJSON = genericParseJSON (aesonDrop 2 snakeCase){omitNothingFields=True}
instance ToJSON (InsPerm 'Postgres) where
  toJSON = genericToJSON (aesonDrop 2 snakeCase){omitNothingFields=True}

type InsPermDef    b = PermDef    (InsPerm b)

-- Select constraint
data SelPerm (b :: Backend)
  = SelPerm
  { spColumns           :: !PermColSpec         -- ^ Allowed columns
  , spFilter            :: !(BoolExp b)         -- ^ Filter expression
  , spLimit             :: !(Maybe Int)         -- ^ Limit value
  , spAllowAggregations :: !Bool                -- ^ Allow aggregation
  , spComputedFields    :: ![ComputedFieldName] -- ^ Allowed computed fields
  } deriving (Show, Eq, Lift, Generic)
instance Cacheable (SelPerm 'Postgres)
instance ToJSON (SelPerm 'Postgres) where
  toJSON = genericToJSON (aesonDrop 2 snakeCase) {omitNothingFields=True}

instance FromJSON (SelPerm 'Postgres) where
  parseJSON = withObject "SelPerm" $ \o ->
    SelPerm
    <$> o .: "columns"
    <*> o .: "filter"
    <*> o .:? "limit"
    <*> o .:? "allow_aggregations" .!= False
    <*> o .:? "computed_fields" .!= []

type SelPermDef b = PermDef (SelPerm b)

-- Delete permission
data DelPerm (b :: Backend)
  = DelPerm { dcFilter :: !(BoolExp b) }
  deriving (Show, Eq, Lift, Generic)
instance Cacheable (DelPerm 'Postgres)
instance FromJSON (DelPerm 'Postgres) where
  parseJSON = genericParseJSON $ aesonDrop 2 snakeCase
instance ToJSON (DelPerm 'Postgres) where
  toJSON = genericToJSON (aesonDrop 2 snakeCase) {omitNothingFields=True}

type DelPermDef    b = PermDef    (DelPerm b)

-- Update constraint
data UpdPerm (b :: Backend)
  = UpdPerm
  { ucColumns :: !PermColSpec -- Allowed columns
  , ucSet     :: !(Maybe (ColumnValues Value)) -- Preset columns
  , ucFilter  :: !(BoolExp b) -- Filter expression (applied before update)
  , ucCheck   :: !(Maybe (BoolExp b))
  -- ^ Check expression, which must be true after update.
  -- This is optional because we don't want to break the v1 API
  -- but Nothing should be equivalent to the expression which always
  -- returns true.
  } deriving (Show, Eq, Lift, Generic)
instance Cacheable (UpdPerm 'Postgres)
instance FromJSON (UpdPerm 'Postgres) where
  parseJSON = genericParseJSON (aesonDrop 2 snakeCase){omitNothingFields=True}
instance ToJSON (UpdPerm 'Postgres) where
  toJSON = genericToJSON (aesonDrop 2 snakeCase){omitNothingFields=True}

type UpdPermDef    b = PermDef    (UpdPerm b)
