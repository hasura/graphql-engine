module Hasura.RQL.DML.Types
  ( OrderByExp (..),
    DMLQuery (..),
    getSourceDMLQuery,
    SelectG (..),
    Wildcard (..),
    SelCol (..),
    SelectQ,
    SelectQT,
    SelectQuery,
    SelectQueryT,
    InsObj,
    InsertQuery (..),
    OnConflict (..),
    ConflictAction (..),
    ConstraintOn (..),
    UpdVals,
    UpdateQuery (..),
    DeleteQuery (..),
    CountQuery (..),
    QueryT (..),
  )
where

import Data.Aeson
import Data.Aeson.Casing
import Data.Attoparsec.Text qualified as AT
import Data.HashMap.Strict qualified as HashMap
import Hasura.Backends.Postgres.Instances.Types ()
import Hasura.Backends.Postgres.SQL.DML qualified as Postgres
import Hasura.Backends.Postgres.SQL.Types
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.OrderBy
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common

newtype OrderByExp = OrderByExp {getOrderByItems :: [OrderByItem ('Postgres 'Vanilla)]}
  deriving (Show, Eq)

instance FromJSON OrderByExp where
  parseJSON = \case
    String s -> OrderByExp . pure <$> parseString s
    Object o -> OrderByExp . pure <$> parseObject o
    Array a ->
      OrderByExp <$> for (toList a) \case
        String s -> parseString s
        Object o -> parseObject o
        _ -> fail "expecting an object or string for order by"
    _ -> fail "Expecting : array/string/object"
    where
      parseString s =
        AT.parseOnly orderByParser s
          `onLeft` const (fail "string format for 'order_by' entry : {+/-}column Eg : +posted")
      parseObject o =
        OrderByItemG
          <$> o
          .:? "type"
          <*> o
          .: "column"
          <*> o
          .:? "nulls"
      orderByParser =
        OrderByItemG
          <$> orderTypeParser
          <*> orderColumnParser
          <*> pure Nothing
      orderTypeParser =
        choice
          [ "+" *> pure (Just Postgres.OTAsc),
            "-" *> pure (Just Postgres.OTDesc),
            pure Nothing
          ]
      orderColumnParser = AT.takeText >>= orderByColFromTxt

data DMLQuery a
  = DMLQuery SourceName QualifiedTable a
  deriving (Show, Eq)

instance (FromJSON a) => FromJSON (DMLQuery a) where
  parseJSON = withObject "query" \o ->
    DMLQuery
      <$> o
      .:? "source"
      .!= defaultSource
      <*> o
      .: "table"
      <*> parseJSON (Object o)

getSourceDMLQuery :: forall a. DMLQuery a -> SourceName
getSourceDMLQuery (DMLQuery source _ _) = source

data SelectG a b c = SelectG
  { sqColumns :: [a], -- Postgres columns and relationships
    sqWhere :: Maybe b, -- Filter
    sqOrderBy :: Maybe OrderByExp, -- Ordering
    sqLimit :: Maybe c, -- Limit
    sqOffset :: Maybe c -- Offset
  }
  deriving (Show, Generic, Eq)

instance (FromJSON a, FromJSON b, FromJSON c) => FromJSON (SelectG a b c) where
  parseJSON = genericParseJSON hasuraJSON {omitNothingFields = True}

data Wildcard
  = Star
  | StarDot Wildcard
  deriving (Show, Eq, Ord)

parseWildcard :: AT.Parser Wildcard
parseWildcard =
  fromList <$> ((starParser `AT.sepBy1` AT.char '.') <* AT.endOfInput)
  where
    starParser = AT.char '*' *> pure Star
    fromList = foldr1 (\_ x -> StarDot x)

-- Columns in RQL
data SelCol
  = SCStar Wildcard
  | SCExtSimple PGCol
  | SCExtRel RelName (Maybe RelName) SelectQ
  deriving (Show, Eq)

instance FromJSON SelCol where
  parseJSON (String s) =
    case AT.parseOnly parseWildcard s of
      Left _ -> SCExtSimple <$> parseJSON (String s)
      Right x -> return $ SCStar x
  parseJSON v@(Object o) =
    SCExtRel
      <$> o
      .: "name"
      <*> o
      .:? "alias"
      <*> parseJSON v
  parseJSON _ =
    fail
      $ mconcat
        [ "A column should either be a string or an ",
          "object (relationship)"
        ]

type SelectQ = SelectG SelCol (BoolExp ('Postgres 'Vanilla)) Int

type SelectQT = SelectG SelCol (BoolExp ('Postgres 'Vanilla)) Value

type SelectQuery = DMLQuery SelectQ

type SelectQueryT = DMLQuery SelectQT

type InsObj b = ColumnValues b Value

data ConflictAction
  = CAIgnore
  | CAUpdate
  deriving (Show, Eq)

instance FromJSON ConflictAction where
  parseJSON (String "ignore") = return CAIgnore
  parseJSON (String "update") = return CAUpdate
  parseJSON _ = fail "Expecting 'ignore' or 'update'"

newtype ConstraintOn = ConstraintOn {getPGCols :: [PGCol]}
  deriving (Show, Eq)

instance FromJSON ConstraintOn where
  parseJSON v@(String _) =
    ConstraintOn . (: []) <$> parseJSON v
  parseJSON v@(Array _) =
    ConstraintOn <$> parseJSON v
  parseJSON _ =
    fail
      "Expecting String or Array"

data OnConflict = OnConflict
  { ocConstraintOn :: Maybe ConstraintOn,
    ocConstraint :: Maybe ConstraintName,
    ocAction :: ConflictAction
  }
  deriving (Show, Generic, Eq)

instance FromJSON OnConflict where
  parseJSON = genericParseJSON hasuraJSON {omitNothingFields = True}

data InsertQuery = InsertQuery
  { iqTable :: QualifiedTable,
    iqSource :: SourceName,
    iqObjects :: Value,
    iqOnConflict :: Maybe OnConflict,
    iqReturning :: Maybe [PGCol]
  }
  deriving (Show, Eq)

instance FromJSON InsertQuery where
  parseJSON = withObject "insert query" $ \o ->
    InsertQuery
      <$> o
      .: "table"
      <*> o
      .:? "source"
      .!= defaultSource
      <*> o
      .: "objects"
      <*> o
      .:? "on_conflict"
      <*> o
      .:? "returning"

type UpdVals b = ColumnValues b Value

data UpdateQuery = UpdateQuery
  { uqTable :: QualifiedTable,
    uqSource :: SourceName,
    uqWhere :: BoolExp ('Postgres 'Vanilla),
    uqSet :: UpdVals ('Postgres 'Vanilla),
    uqInc :: UpdVals ('Postgres 'Vanilla),
    uqMul :: UpdVals ('Postgres 'Vanilla),
    uqDefault :: [PGCol],
    uqReturning :: Maybe [PGCol]
  }
  deriving (Show, Eq)

instance FromJSON UpdateQuery where
  parseJSON = withObject "update query" \o ->
    UpdateQuery
      <$> o
      .: "table"
      <*> o
      .:? "source"
      .!= defaultSource
      <*> o
      .: "where"
      <*> ((o .: "$set" <|> o .:? "values") .!= HashMap.empty)
      <*> (o .:? "$inc" .!= HashMap.empty)
      <*> (o .:? "$mul" .!= HashMap.empty)
      <*> o
      .:? "$default"
      .!= []
      <*> o
      .:? "returning"

data DeleteQuery = DeleteQuery
  { doTable :: QualifiedTable,
    doSource :: SourceName,
    doWhere :: BoolExp ('Postgres 'Vanilla), -- where clause
    doReturning :: Maybe [PGCol] -- columns returning
  }
  deriving (Show, Eq)

instance FromJSON DeleteQuery where
  parseJSON = withObject "delete query" $ \o ->
    DeleteQuery
      <$> o
      .: "table"
      <*> o
      .:? "source"
      .!= defaultSource
      <*> o
      .: "where"
      <*> o
      .:? "returning"

data CountQuery = CountQuery
  { cqTable :: QualifiedTable,
    cqSource :: SourceName,
    cqDistinct :: Maybe [PGCol],
    cqWhere :: Maybe (BoolExp ('Postgres 'Vanilla))
  }
  deriving (Show, Eq)

instance FromJSON CountQuery where
  parseJSON = withObject "count query" $ \o ->
    CountQuery
      <$> o
      .: "table"
      <*> o
      .:? "source"
      .!= defaultSource
      <*> o
      .:? "distinct"
      <*> o
      .:? "where"

data QueryT
  = QTInsert InsertQuery
  | QTSelect SelectQueryT
  | QTUpdate UpdateQuery
  | QTDelete DeleteQuery
  | QTCount CountQuery
  | QTBulk [QueryT]
  deriving (Show, Generic, Eq)

instance FromJSON QueryT where
  parseJSON =
    genericParseJSON
      defaultOptions
        { constructorTagModifier = snakeCase . drop 2,
          sumEncoding = TaggedObject "type" "args"
        }
