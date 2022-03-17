{-# LANGUAGE QuasiQuotes #-}

-- | Common interface for setup/teardown for all backends - schema and data
module Harness.Test.Schema
  ( Table (..),
    Reference (..),
    Column (..),
    ScalarType (..),
    ScalarValue (..),
    serialize,
    column,
    columnNull,
    parseUTCTimeOrError,
    trackTable,
    untrackTable,
    trackObjectRelationships,
    trackArrayRelationships,
    untrackRelationships,
  )
where

import Data.Foldable (for_)
import Data.Text (Text, pack, replace)
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.Time.Format (parseTimeOrError)
import Harness.Exceptions
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.State (State)
import Harness.Test.Context (BackendType, defaultBackendTypeString, defaultSchema, defaultSource)
import Hasura.Prelude (tshow)
import Prelude

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
    tableData :: [[ScalarValue]]
  }
  deriving (Show, Eq)

-- | Foreign keys for backends that support it.
data Reference = Reference
  { referenceLocalColumn :: Text,
    referenceTargetTable :: Text,
    referenceTargetColumn :: Text
  }
  deriving (Show, Eq)

-- | Generic type to construct columns for all backends
data Column = Column
  { columnName :: Text,
    columnType :: ScalarType,
    columnNullable :: Bool,
    columnDefault :: Maybe Text
  }
  deriving (Show, Eq)

-- | Generic scalar type for all backends, for simplicity.
-- Ideally, we would be wiring in @'Backend@ specific scalar types here to make
-- sure all backend-specific scalar types are also covered by tests, perhaps in
-- a future iteration.
data ScalarType
  = TInt
  | TStr
  | TUTCTime
  | TBool
  | -- | Specialized. See: https://github.com/hasura/graphql-engine/issues/8158
    -- session variable string values are not truncated to default (30) length in Test.RequestHeadersSpec
    -- works with VStr
    TVarchar50
  deriving (Show, Eq)

-- | Generic scalar value type for all backends, that should directly correspond
-- to 'ScalarType'
data ScalarValue
  = VInt Int
  | VStr Text
  | VUTCTime UTCTime
  | VBool Bool
  | VNull
  deriving (Show, Eq)

-- | Generic 'ScalarValue' serializer.
--
-- NOTE: For serialization of 'ScalarType' we need to have backend-specific
-- functions as they correspond to the query language of the specific backend
serialize :: ScalarValue -> Text
serialize = \case
  VInt i -> tshow i
  VStr s -> "'" <> replace "'" "\'" s <> "'"
  VUTCTime t -> pack $ formatTime defaultTimeLocale "'%F %T'" t
  VBool b -> tshow @Int $ if b then 1 else 0
  VNull -> "NULL"

-- | Helper function to construct 'Column's with common defaults
column :: Text -> ScalarType -> Column
column name typ = Column name typ False Nothing

-- | Helper function to construct 'Column's that are null-able
columnNull :: Text -> ScalarType -> Column
columnNull name typ = Column name typ True Nothing

-- | Helper to construct UTCTime using @%F %T@ format. For e.g. @YYYY-MM-DD HH:MM:SS@
parseUTCTimeOrError :: String -> ScalarValue
parseUTCTimeOrError = VUTCTime . parseTimeOrError True defaultTimeLocale "%F %T"

-- | Unified track table
trackTable :: HasCallStack => BackendType -> String -> Table -> State -> IO ()
trackTable backend source Table {tableName} state = do
  let backendType = defaultBackendTypeString backend
      schema = defaultSchema backend
      requestType = backendType <> "_track_table"
  GraphqlEngine.postMetadata_
    state
    [yaml|
type: *requestType
args:
  source: *source
  table:
    schema: *schema
    name: *tableName
|]

-- | Unified untrack table
untrackTable :: HasCallStack => BackendType -> String -> Table -> State -> IO ()
untrackTable backend source Table {tableName} state = do
  let backendType = defaultBackendTypeString backend
      schema = defaultSchema backend
  let requestType = backendType <> "_untrack_table"
  GraphqlEngine.postMetadata_
    state
    [yaml|
type: *requestType
args:
  source: *source
  table:
    schema: *schema
    name: *tableName
|]

-- | Helper to create the object relationship name
mkObjectRelationshipName :: Reference -> Text
mkObjectRelationshipName Reference {referenceLocalColumn, referenceTargetTable} = referenceTargetTable <> "_by_" <> referenceLocalColumn

-- | Unified track object relationships
trackObjectRelationships :: HasCallStack => BackendType -> Table -> State -> IO ()
trackObjectRelationships backend Table {tableName, tableReferences} state = do
  let source = defaultSource backend
      schema = defaultSchema backend
      requestType = source <> "_create_object_relationship"
  for_ tableReferences $ \ref@Reference {referenceLocalColumn} -> do
    let relationshipName = mkObjectRelationshipName ref
    GraphqlEngine.postMetadata_
      state
      [yaml|
type: *requestType
args:
  source: *source
  table:
    name: *tableName
    schema: *schema
  name: *relationshipName
  using:
    foreign_key_constraint_on: *referenceLocalColumn
|]

-- | Helper to create the array relationship name
mkArrayRelationshipName :: Text -> Text -> Text
mkArrayRelationshipName tableName referenceLocalColumn = tableName <> "s_by_" <> referenceLocalColumn

-- | Unified track array relationships
trackArrayRelationships :: HasCallStack => BackendType -> Table -> State -> IO ()
trackArrayRelationships backend Table {tableName, tableReferences} state = do
  let source = defaultSource backend
      schema = defaultSchema backend
      requestType = source <> "_create_array_relationship"
  for_ tableReferences $ \Reference {referenceLocalColumn, referenceTargetTable} -> do
    let relationshipName = mkArrayRelationshipName tableName referenceLocalColumn
    GraphqlEngine.postMetadata_
      state
      [yaml|
type: *requestType
args:
  source: *source
  table:
    name: *referenceTargetTable
    schema: *schema
  name: *relationshipName
  using:
    foreign_key_constraint_on:
      table:
        name: *tableName
        schema: *schema
      column: *referenceLocalColumn
|]

-- | Unified untrack relationships
untrackRelationships :: HasCallStack => BackendType -> Table -> State -> IO ()
untrackRelationships backend Table {tableName, tableReferences} state = do
  let source = defaultSource backend
      schema = defaultSchema backend
      requestType = source <> "_drop_relationship"
  for_ tableReferences $ \ref@Reference {referenceLocalColumn, referenceTargetTable} -> do
    let arrayRelationshipName = mkArrayRelationshipName tableName referenceLocalColumn
        objectRelationshipName = mkObjectRelationshipName ref
    -- drop array relationships
    GraphqlEngine.postMetadata_
      state
      [yaml|
type: *requestType
args:
  source: *source
  table:
    schema: *schema
    name: *referenceTargetTable
  relationship: *arrayRelationshipName
|]
    -- drop object relationships
    GraphqlEngine.postMetadata_
      state
      [yaml|
type: *requestType
args:
  source: *source
  table:
    schema: *schema
    name: *tableName
  relationship: *objectRelationshipName
|]
