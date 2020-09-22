-- | Types for Transact-SQL aka T-SQL; the language of SQL Server.

module Hasura.SQL.Tsql.Types where

import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Text (Text)
import qualified Database.ODBC.SQLServer as Odbc
import           Prelude

data Select = Select
  { selectTop :: !(Commented Top)
  , selectProjections :: !(NonEmpty Projection)
  , selectFrom :: !From
  } deriving (Eq, Show)

data Projection =
  ExpressionProjection (Aliased Expression)
  deriving (Eq, Show)

data Top
  = NoTop
  | Top Int
  deriving (Eq, Show)

data Expression =
  ValueExpression Odbc.Value
  deriving (Eq, Show)

data From =
  FromQualifiedTable (Aliased (Qualified TableName))
  deriving (Eq, Show)

data Qualified a = Qualified
  { qualifiedThing :: !a
  , qualifiedSchemaName :: !(Maybe SchemaName)
  } deriving (Eq, Show, Functor)

data Aliased a = Aliased
  { aliasedThing :: !a
  , aliasedAlias :: !(Maybe Alias)
  } deriving (Eq, Show, Functor)

newtype SchemaName = SchemaName
  { schemaNameParts :: [Text]
  } deriving (Eq, Show)

newtype Alias = Alias
  { aliasText :: Text
  } deriving (Eq, Show)

newtype TableName = TableName
  { tableNameText :: Text
  } deriving (Eq, Show)

data Commented a = Commented
  { commentedComment :: !(Maybe Comment)
  , commentedThing :: !a
  } deriving (Eq, Show, Functor)

data Comment = DueToPermission
  deriving (Eq, Show)
