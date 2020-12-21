{-# LANGUAGE DuplicateRecordFields #-}

-- | Types for Transact-SQL aka T-SQL; the language of SQL Server.

module Hasura.Backends.MSSQL.Types.Internal where

import           Hasura.Prelude

import qualified Database.ODBC.SQLServer as ODBC

import           Hasura.SQL.Backend


--------------------------------------------------------------------------------
-- Phantom pretend-generic types that are actually specific

type Column (b :: BackendType) = ColumnName

type ColumnType (b :: BackendType) = ScalarType


--------------------------------------------------------------------------------
-- Metadata tweaks

data UserMetadata = UserMetadata
  { tables :: ![UserTableMetadata]
  }

data UserTableMetadata = UserTableMetadata
  { table                :: !UserTableName
  , object_relationships :: [UserObjectRelationship]
  , array_relationships  :: [UserArrayRelationship]
  }

data UserTableName = UserTableName
  { schema :: !Text
  , name   :: !Text
  }

data UserObjectRelationship = UserObjectRelationship
  { using :: !UserUsing
  , name  :: !Text
  }

data UserArrayRelationship = UserArrayRelationship
  { using :: !UserUsing
  , name  :: !Text
  }

data UserUsing = UserUsing
  { foreign_key_constraint_on :: !UserOn
  }

data UserOn = UserOn
  { table  :: !UserTableName
  , column :: !Text
  }

--------------------------------------------------------------------------------

data UnifiedMetadata = UnifiedMetadata
  { tables :: ![UnifiedTableMetadata]
  }

data UnifiedTableMetadata = UnifiedTableMetadata
  { table                :: !UnifiedTableName
  , object_relationships :: ![UnifiedObjectRelationship]
  , array_relationships  :: ![UnifiedArrayRelationship]
  , columns              :: ![UnifiedColumn]
  }

data UnifiedColumn = UnifiedColumn
  { name  :: !Text
  , type' :: !ScalarType
  }

data UnifiedTableName = UnifiedTableName
  { schema :: !Text
  , name   :: !Text
  }

data UnifiedObjectRelationship = UnifiedObjectRelationship
  { using :: !UnifiedUsing
  , name  :: !Text
  }

data UnifiedArrayRelationship = UnifiedArrayRelationship
  { using :: !UnifiedUsing
  , name  :: !Text
  }

data UnifiedUsing = UnifiedUsing
  { foreign_key_constraint_on :: !UnifiedOn
  }

data UnifiedOn = UnifiedOn
  { table  :: !UnifiedTableName
  , column :: !Text
  }

-------------------------------------------------------------------------------
-- AST types

data Select = Select
  { selectTop         :: !Top
  , selectProjections :: !(NonEmpty Projection)
  , selectFrom        :: !From
  , selectJoins       :: ![Join]
  , selectWhere       :: !Where
  , selectFor         :: !For
  , selectOrderBy     :: !(Maybe (NonEmpty OrderBy))
  , selectOffset      :: !(Maybe Expression)
  }

data Delete = Delete
  { deleteTable :: !(Aliased TableName)
  , deleteWhere :: !Where
  }

data Reselect = Reselect
  { reselectProjections :: !(NonEmpty Projection)
  , reselectFor         :: !For
  , reselectWhere       :: !Where
  }

data OrderBy = OrderBy
  { orderByFieldName  :: FieldName
  , orderByOrder      :: Order
  , orderByNullsOrder :: NullsOrder
  }

data Order
  = AscOrder
  | DescOrder

data NullsOrder
  = NullsFirst
  | NullsLast
  | NullsAnyOrder

data For
  = JsonFor ForJson
  | NoFor

data ForJson = ForJson
  { jsonCardinality :: JsonCardinality
  , jsonRoot        :: Root
  }

data Root
  = NoRoot
  | Root Text

data JsonCardinality
  = JsonArray
  | JsonSingleton

data Projection
  = ExpressionProjection (Aliased Expression)
  | FieldNameProjection (Aliased FieldName)
  | AggregateProjection (Aliased Aggregate)
  | StarProjection

data Join = Join
  { joinSource    :: !JoinSource
  , joinJoinAlias :: !JoinAlias
  }

data JoinSource
  = JoinSelect Select
  | JoinReselect Reselect


data JoinAlias = JoinAlias
  { joinAliasEntity :: Text
  , joinAliasField  :: Maybe Text
  }

newtype Where =
  Where [Expression]

data Top
  = NoTop
  | Top Int

data Expression
  = ValueExpression ODBC.Value
  | AndExpression [Expression]
  | OrExpression [Expression]
  | NotExpression Expression
  | ExistsExpression Select
  | SelectExpression Select
  | IsNullExpression Expression
  | IsNotNullExpression Expression
  | ColumnExpression FieldName
  | EqualExpression Expression Expression
  | NotEqualExpression Expression Expression
  | JsonQueryExpression Expression
    -- ^ This one acts like a "cast to JSON" and makes SQL Server
    -- behave like it knows your field is JSON and not double-encode
    -- it.
  | ToStringExpression Expression
  | JsonValueExpression Expression JsonPath
    -- ^ This is for getting actual atomic values out of a JSON
    -- string.
  | OpExpression Op Expression Expression

data JsonPath
  = RootPath
  | FieldPath JsonPath Text
  | IndexPath JsonPath Integer

data Aggregate
  = CountAggregate Countable
  | OpAggregate !Text (NonEmpty Expression)
  | TextAggregate !Text

data Countable
  = StarCountable
  | NonNullFieldCountable (NonEmpty FieldName)
  | DistinctCountable (NonEmpty FieldName)

data From
  = FromQualifiedTable (Aliased TableName)
  | FromOpenJson (Aliased OpenJson)

data OpenJson = OpenJson
  { openJsonExpression :: Expression
  , openJsonWith       :: NonEmpty JsonFieldSpec
  }

data JsonFieldSpec
  = IntField Text
  | JsonField Text

data Aliased a = Aliased
  { aliasedThing :: !a
  , aliasedAlias :: !Text
  }

newtype SchemaName = SchemaName
  { schemaNameParts :: [Text]
  }

data TableName = TableName
  { tableName       :: Text
  , tableNameSchema :: Text
  }

data FieldName = FieldName
  { fieldName       :: Text
  , fieldNameEntity :: !Text
  }

data Comment = DueToPermission | RequestedSingleObject

newtype EntityAlias = EntityAlias
  { entityAliasText :: Text
  }

data Op
  = LessOp
  | LessOrEqualOp
  | MoreOp
  | MoreOrEqualOp
  -- | SIN
  -- | SNE
  -- | SLIKE
  -- | SNLIKE
  -- | SILIKE
  -- | SNILIKE
  -- | SSIMILAR
  -- | SNSIMILAR
  -- | SGTE
  -- | SLTE
  -- | SNIN
  -- | SContains
  -- | SContainedIn
  -- | SHasKey
  -- | SHasKeysAny
  -- | SHasKeysAll

-- | Column name of some database table -- this differs to FieldName
-- that is used for referring to things within a query.
newtype ColumnName = ColumnName { columnNameText :: Text }

-- | Derived from the odbc package.
data ScalarType
  = CharType
  | NumericType
  | DecimalType
  | IntegerType
  | SmallintType
  | FloatType
  | RealType
  | DoubleType
  | DateType
  | Ss_time2Type
  | VarcharType
  | WcharType
  | WvarcharType
  | WlongvarcharType
  | TimeType
  | TimestampType
  | LongvarcharType
  | BinaryType
  | VarbinaryType
  | BigintType
  | TinyintType
  | BitType
  | GuidType

isComparableType, isNumType :: ScalarType -> Bool
isComparableType = \case
  BinaryType    -> False
  VarbinaryType -> False
  BitType       -> False
  GuidType      -> False
  _             -> True
isNumType = \case
  NumericType  -> True
  DecimalType  -> True
  IntegerType  -> True
  SmallintType -> True
  FloatType    -> True
  RealType     -> True
  DoubleType   -> True
  BigintType   -> True
  TinyintType  -> True
  _            -> False
