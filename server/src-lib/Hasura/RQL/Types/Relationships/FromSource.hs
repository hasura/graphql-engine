module Hasura.RQL.Types.Relationships.FromSource
  ( RemoteSourceFieldInfo (..),
    RemoteSchemaFieldInfo (..),
    RemoteFieldInfo (..),
    DBJoinField (..),
    ScalarComputedField (..),
    dbJoinFieldToName,
    graphQLValueToJSON,
  )
where

import Data.Aeson
import Data.HashMap.Strict qualified as HM
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.Instances ()
import Hasura.RQL.Types.Relationships.ToSchema
import Hasura.RQL.Types.RemoteSchema
import Hasura.RQL.Types.SourceCustomization
import Hasura.SQL.AnyBackend (AnyBackend)
import Hasura.SQL.Backend
import Language.GraphQL.Draft.Syntax qualified as G

--------------------------------------------------------------------------------
-- schema cache

-- | Resolved remote relationship, as stored in the schema cache.
data RemoteFieldInfo (b :: BackendType)
  = RFISchema !(RemoteSchemaFieldInfo b)
  | RFISource !(AnyBackend (RemoteSourceFieldInfo b))
  deriving (Generic)

deriving instance Backend b => Eq (RemoteFieldInfo b)

instance Backend b => Cacheable (RemoteFieldInfo b)

instance Backend b => ToJSON (RemoteFieldInfo b) where
  toJSON =
    \case
      RFISchema schema -> toJSON schema
      RFISource _ -> toJSON ()

-- | Schema cache information for a table field targeting a remote source.
data RemoteSourceFieldInfo src tgt = RemoteSourceFieldInfo
  { _rsfiName :: !RelName,
    _rsfiType :: !RelType,
    _rsfiSource :: !SourceName,
    _rsfiSourceConfig :: !(SourceConfig tgt),
    _rsfiSourceCustomization :: !SourceTypeCustomization,
    _rsfiTable :: !(TableName tgt), -- this is parsed from `Value`
    _rsfiMapping :: !(HM.HashMap FieldName (ColumnInfo src, ScalarType tgt, Column tgt))
  }
  deriving stock (Generic)

deriving instance (Backend src, Backend tgt) => Eq (RemoteSourceFieldInfo src tgt)

instance (Backend src, Backend tgt) => Cacheable (RemoteSourceFieldInfo src tgt)

-- | Schema cache information for a table field targeting a remote schema.
data RemoteSchemaFieldInfo (b :: BackendType) = RemoteSchemaFieldInfo
  { -- | Field name to which we'll map the remote in hasura; this becomes part
    --   of the hasura schema.
    _rrfiName :: !RelName,
    -- | Input arguments to the remote field info; The '_rfiParamMap' will only
    --   include the arguments to the remote field that is being joined. The
    --   names of the arguments here are modified, it will be in the format of
    --   <Original Field Name>_remote_rel_<hasura table schema>_<hasura table name><remote relationship name>
    _rrfiParamMap :: !(HashMap G.Name RemoteSchemaInputValueDefinition),
    -- | Hasura fields used to join the remote schema node
    _rrfiHasuraFields :: !(HashSet (DBJoinField b)),
    _rrfiRemoteFields :: !RemoteFields,
    _rrfiRemoteSchema :: !RemoteSchemaInfo,
    -- | The new input value definitions created for this remote field
    _rrfiInputValueDefinitions :: ![G.TypeDefinition [G.Name] RemoteSchemaInputValueDefinition],
    -- | Name of the remote schema, that's used for joining
    _rrfiRemoteSchemaName :: !RemoteSchemaName,
    -- | Name of the table and its source
    _rrfiTable :: !(TableName b, SourceName)
  }
  deriving (Generic)

deriving instance Backend b => Eq (RemoteSchemaFieldInfo b)

deriving instance Backend b => Show (RemoteSchemaFieldInfo b)

instance Backend b => Cacheable (RemoteSchemaFieldInfo b)

instance Backend b => ToJSON (RemoteSchemaFieldInfo b) where
  toJSON RemoteSchemaFieldInfo {..} =
    object
      [ "name" .= _rrfiName,
        "param_map" .= fmap toJsonInpValInfo _rrfiParamMap,
        "hasura_fields" .= _rrfiHasuraFields,
        "remote_fields" .= _rrfiRemoteFields,
        "remote_schema" .= _rrfiRemoteSchema
      ]
    where
      toJsonInpValInfo (RemoteSchemaInputValueDefinition (G.InputValueDefinition desc name type' defVal _directives) _preset) =
        object
          [ "desc" .= desc,
            "name" .= name,
            "def_val" .= fmap graphQLValueToJSON defVal,
            "type" .= type'
          ]

-- FIXME: deduplicate this
graphQLValueToJSON :: G.Value Void -> Value
graphQLValueToJSON = \case
  G.VNull -> Null
  G.VInt i -> toJSON i
  G.VFloat f -> toJSON f
  G.VString t -> toJSON t
  G.VBoolean b -> toJSON b
  G.VEnum (G.EnumValue n) -> toJSON n
  G.VList values -> toJSON $ graphQLValueToJSON <$> values
  G.VObject objects -> toJSON $ graphQLValueToJSON <$> objects

-- | Information about the field on the LHS of a join against a remote schema.
data DBJoinField (b :: BackendType)
  = JoinColumn !(Column b) !(ColumnType b)
  | JoinComputedField !(ScalarComputedField b)
  deriving (Generic)

deriving instance Backend b => Eq (DBJoinField b)

deriving instance Backend b => Show (DBJoinField b)

instance Backend b => Cacheable (DBJoinField b)

instance Backend b => Hashable (DBJoinField b)

instance (Backend b) => ToJSON (DBJoinField b) where
  toJSON = \case
    JoinColumn column columnType -> toJSON (column, columnType)
    JoinComputedField computedField -> toJSON computedField

dbJoinFieldToName :: forall b. (Backend b) => DBJoinField b -> FieldName
dbJoinFieldToName = \case
  JoinColumn column _ -> fromCol @b column
  JoinComputedField computedFieldInfo -> fromComputedField $ _scfName computedFieldInfo

-- | Information about a computed field appearing on the LHS of a remote join.
-- FIXME: why do we need all of this?
data ScalarComputedField (b :: BackendType) = ScalarComputedField
  { _scfXField :: !(XComputedField b),
    _scfName :: !ComputedFieldName,
    _scfFunction :: !(FunctionName b),
    _scfTableArgument :: !FunctionTableArgument,
    _scfSessionArgument :: !(Maybe FunctionSessionArgument),
    _scfType :: !(ScalarType b)
  }
  deriving (Generic)

deriving instance Backend b => Eq (ScalarComputedField b)

deriving instance Backend b => Show (ScalarComputedField b)

instance Backend b => Cacheable (ScalarComputedField b)

instance Backend b => Hashable (ScalarComputedField b)

instance Backend b => ToJSON (ScalarComputedField b) where
  toJSON ScalarComputedField {..} =
    object
      [ "name" .= _scfName,
        "function" .= _scfFunction,
        "table_argument" .= _scfTableArgument,
        "session_argument" .= _scfSessionArgument,
        "type" .= _scfType
      ]
