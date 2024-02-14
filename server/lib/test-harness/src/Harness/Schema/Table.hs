{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | Common interface for setup/teardown for all backends - schema and data
module Harness.Schema.Table
  ( Table (..),
    table,
    Reference (..),
    RelationshipType (..),
    NativeQueryRelationship (..),
    reference,
    nativeQueryArrayRelationship,
    nativeQueryObjectRelationship,
    InsertOrder (..),
    Column (..),
    ScalarType (..),
    defaultSerialType,
    ScalarValue (..),
    WKT (..),
    formatTableQualifier,
    TableQualifier (..),
    Constraint (..),
    UniqueIndex (..),
    BackendScalarType (..),
    BackendScalarValue (..),
    BackendScalarValueType (..),
    quotedValue,
    unquotedValue,
    backendScalarValue,
    column,
    columnNull,
    defaultBackendScalarType,
    getBackendScalarType,
    defaultBackendScalarValue,
    formatBackendScalarValueType,
    parseUTCTimeOrError,
  )
where

import Data.Time (defaultTimeLocale)
import Data.Time.Format (parseTimeOrError)
import Harness.Test.ScalarType
import Hasura.Prelude

-- | Generic type to use to specify schema tables for all backends.
-- Usually a list of these make up a "schema" to pass to the respective
-- @Harness.Backend.<Backend>.{setup,teardown}@ functions
--
-- NOTE: There is neither a type-level check to assert that the length of
-- tableColumns matches the length of each row in tableData, nor that the
-- tablePrimaryKey only contains names of columns already in tableColumns or
-- that tableReferences are valid references to other Tables. Test author will
-- need to be just a bit careful while constructing Tables.
data Table = Table
  { tableName :: Text,
    -- | Columns that are references (foreign keys) should be null-able
    tableColumns :: [Column],
    tablePrimaryKey :: [Text],
    tableReferences :: [Reference],
    tableManualRelationships :: [Reference],
    tableNativeQueryRelationships :: [NativeQueryRelationship],
    tableData :: [[ScalarValue]],
    tableConstraints :: [Constraint],
    tableUniqueIndexes :: [UniqueIndex],
    tableQualifiers :: [TableQualifier]
  }
  deriving (Show, Eq)

-- | Used to qualify a tracked table by schema (and additionally by GCP projectId, in the case
-- of BigQuery)
newtype TableQualifier = TableQualifier Text
  deriving (Show, Eq)

formatTableQualifier :: TableQualifier -> Text
formatTableQualifier (TableQualifier t) = t

data Constraint = UniqueConstraintColumns [Text] | CheckConstraintExpression Text
  deriving (Show, Eq)

data UniqueIndex = UniqueIndexColumns [Text] | UniqueIndexExpression Text
  deriving (Show, Eq)

-- | Create a table from just a name.
-- Use record updates to modify the result.
table :: Text -> Table
table tableName =
  Table
    { tableName = tableName,
      tableColumns = [],
      tablePrimaryKey = [],
      tableReferences = [],
      tableManualRelationships = [],
      tableNativeQueryRelationships = [],
      tableData = [],
      tableConstraints = [],
      tableUniqueIndexes = [],
      tableQualifiers = []
    }

data InsertOrder = BeforeParent | AfterParent
  deriving (Show, Eq)

-- | Foreign keys for backends that support it.
data Reference = Reference
  { referenceLocalColumn :: Text,
    referenceTargetTable :: Text,
    referenceTargetColumn :: Text,
    referenceTargetQualifiers :: [Text],
    referenceInsertionOrder :: InsertOrder,
    referenceCascade :: Bool
  }
  deriving (Show, Eq)

reference :: Text -> Text -> Text -> Reference
reference localColumn targetTable targetColumn =
  Reference
    { referenceLocalColumn = localColumn,
      referenceTargetTable = targetTable,
      referenceTargetColumn = targetColumn,
      referenceTargetQualifiers = mempty,
      referenceInsertionOrder = BeforeParent,
      referenceCascade = True
    }

data RelationshipType = ArrayRelationship | ObjectRelationship
  deriving (Eq, Show)

-- | Relationship to a Native Query
data NativeQueryRelationship = NativeQueryRelationship
  { nqRelationshipLocalColumn :: Text,
    nqRelationshipTarget :: Text,
    nqRelationshipTargetColumn :: Text,
    nqRelationshipType :: RelationshipType
  }
  deriving (Show, Eq)

nativeQueryObjectRelationship :: Text -> Text -> Text -> NativeQueryRelationship
nativeQueryObjectRelationship localColumn targetNativeQuery targetColumn =
  NativeQueryRelationship
    { nqRelationshipLocalColumn = localColumn,
      nqRelationshipTarget = targetNativeQuery,
      nqRelationshipTargetColumn = targetColumn,
      nqRelationshipType = ObjectRelationship
    }

nativeQueryArrayRelationship :: Text -> Text -> Text -> NativeQueryRelationship
nativeQueryArrayRelationship localColumn targetNativeQuery targetColumn =
  NativeQueryRelationship
    { nqRelationshipLocalColumn = localColumn,
      nqRelationshipTarget = targetNativeQuery,
      nqRelationshipTargetColumn = targetColumn,
      nqRelationshipType = ArrayRelationship
    }

-- | Generic type to construct columns for all backends
data Column = Column
  { columnName :: Text,
    columnType :: ScalarType,
    columnNullable :: Bool,
    columnDefault :: Maybe Text,
    columnGqlAlias :: Maybe Text
  }
  deriving (Show, Eq)

-- | Helper function to construct 'Column's with common defaults
column :: Text -> ScalarType -> Column
column name typ = Column name typ False Nothing Nothing

-- | Helper function to construct 'Column's that are null-able
columnNull :: Text -> ScalarType -> Column
columnNull name typ = (column name typ) {columnNullable = True}

-- | Helper to construct UTCTime using @%F %T@ format. For e.g. @YYYY-MM-DD HH:MM:SS@
parseUTCTimeOrError :: String -> ScalarValue
parseUTCTimeOrError = VUTCTime . parseTimeOrError True defaultTimeLocale "%F %T"
