-- | Types for Transact-SQL aka T-SQL; the language of SQL Server.

module Hasura.SQL.Tsql.Types where

import           Data.Text (Text)
import qualified Database.ODBC.SQLServer as Odbc
import           Prelude

data Select = Select
  { selectExpression :: Expression
  , selectFrom :: From
  } deriving (Eq, Show)

data Expression =
  ValueExpression Odbc.Value
  deriving (Eq, Show)

data From =
  FromQualifiedTable (Aliased (Qualified TableName))
  deriving (Eq, Show)

data Qualified a = Qualified
  { qualifiedThing :: a
  , qualifiedSchemaName :: Maybe SchemaName
  } deriving (Eq, Show, Functor)

data Aliased a = Aliased
  { aliasedThing :: a
  , aliasedColumnAlias :: Maybe ColumnAlias
  } deriving (Eq, Show, Functor)

newtype SchemaName = SchemaName
  { schemaNameParts :: [Text]
  } deriving (Eq, Show)

newtype ColumnAlias = ColumnAlias
  { columnAliasText :: Text
  } deriving (Eq, Show)

newtype TableName = TableName
  { tableNameText :: Text
  } deriving (Eq, Show)
