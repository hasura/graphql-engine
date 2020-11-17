module Hasura.RQL.DML.Types
       ( OrderByExp(..)

       , DMLQuery(..)

       , SelectG(..)
       , selectGToPairs

       , Wildcard(..)
       , SelCol(..)
       , SelectQ
       , SelectQT
       , SelectQuery
       , SelectQueryT

       , InsObj
       , InsertQuery(..)
       , OnConflict(..)
       , ConflictAction(..)
       , ConstraintOn(..)

       , InsertTxConflictCtx(..)

       , UpdVals
       , UpdateQuery(..)

       , DeleteQuery(..)

       , CountQuery(..)

       , QueryT(..)
       ) where

import           Hasura.Prelude

import qualified Data.Attoparsec.Text               as AT
import qualified Data.HashMap.Strict                as M

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Instances.TH.Lift                  ()
import           Language.Haskell.TH.Syntax         (Lift)

import qualified Hasura.Backends.Postgres.SQL.DML   as PG

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.RQL.IR.BoolExp
import           Hasura.RQL.IR.OrderBy
import           Hasura.RQL.Instances               ()
import           Hasura.RQL.Types.Common            hiding (ConstraintName)
import           Hasura.SQL.Backend


newtype OrderByExp
  = OrderByExp { getOrderByItems :: [OrderByItem 'Postgres] }
  deriving (Show, Eq, Lift, ToJSON)

instance FromJSON OrderByExp where
  parseJSON = \case
    String s -> OrderByExp . pure <$> parseString s
    Object o -> OrderByExp . pure <$> parseObject o
    Array  a -> OrderByExp <$> for (toList a) \case
      String s -> parseString s
      Object o -> parseObject o
      _        -> fail "expecting an object or string for order by"
    _        -> fail "Expecting : array/string/object"
    where
      parseString s = AT.parseOnly orderByParser s `onLeft`
        const (fail "string format for 'order_by' entry : {+/-}column Eg : +posted")
      parseObject o =
        OrderByItemG
        <$> o .:? "type"
        <*> o .:  "column"
        <*> o .:? "nulls"
      orderByParser =
        OrderByItemG
        <$> orderTypeParser
        <*> orderColumnParser
        <*> pure Nothing
      orderTypeParser = choice
        [ "+" *> pure (Just PG.OTAsc)
        , "-" *> pure (Just PG.OTDesc)
        , pure Nothing
        ]
      orderColumnParser = AT.takeText >>= orderByColFromTxt


data DMLQuery a
  = DMLQuery !QualifiedTable a
  deriving (Show, Eq, Lift)

instance (FromJSON a) => FromJSON (DMLQuery a) where
  parseJSON o@(Object v) =
    DMLQuery
    <$> v .: "table"
    <*> parseJSON o
  parseJSON _          =
    fail "Expected an object for query"


data SelectG a b c
  = SelectG
  { sqColumns :: ![a]                -- Postgres columns and relationships
  , sqWhere   :: !(Maybe b)          -- Filter
  , sqOrderBy :: !(Maybe OrderByExp) -- Ordering
  , sqLimit   :: !(Maybe c)          -- Limit
  , sqOffset  :: !(Maybe c)          -- Offset
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''SelectG)

selectGToPairs :: (KeyValue kv, ToJSON a, ToJSON b, ToJSON c)
               => SelectG a b c -> [kv]
selectGToPairs (SelectG selCols mWh mOb mLt mOf) =
  [ "columns" .= selCols
  , "where" .= mWh
  , "order_by" .= mOb
  , "limit" .= mLt
  , "offset" .= mOf
  ]

data Wildcard
  = Star
  | StarDot !Wildcard
  deriving (Show, Eq, Ord, Lift)

wcToText :: Wildcard -> Text
wcToText Star         = "*"
wcToText (StarDot wc) = "*." <> wcToText wc

parseWildcard :: AT.Parser Wildcard
parseWildcard =
  fromList <$> ((starParser `AT.sepBy1` AT.char '.') <* AT.endOfInput)
  where
    starParser = AT.char '*' *> pure Star
    fromList   = foldr1 (\_ x -> StarDot x)

-- Columns in RQL
data SelCol (b :: BackendType)
  = SCStar !Wildcard
  | SCExtSimple !(Column b)
  | SCExtRel !RelName !(Maybe RelName) !(SelectQ b)
deriving instance Eq   (SelCol 'Postgres)
deriving instance Lift (SelCol 'Postgres)
deriving instance Show (SelCol 'Postgres)

instance FromJSON (SelCol 'Postgres) where
  parseJSON (String s) =
    case AT.parseOnly parseWildcard s of
    Left _  -> SCExtSimple <$> parseJSON (String s)
    Right x -> return $ SCStar x
  parseJSON v@(Object o) =
    SCExtRel
    <$> o .:  "name"
    <*> o .:? "alias"
    <*> parseJSON v
  parseJSON _ =
    fail $ mconcat
    [ "A column should either be a string or an "
    , "object (relationship)"
    ]

instance ToJSON (SelCol 'Postgres) where
  toJSON (SCStar wc) = String $ wcToText wc
  toJSON (SCExtSimple s) = toJSON s
  toJSON (SCExtRel rn mrn selq) =
    object $ [ "name" .= rn
             , "alias" .= mrn
             ] ++ selectGToPairs selq

type SelectQ  b = SelectG (SelCol b) (BoolExp b) Int
type SelectQT b = SelectG (SelCol b) (BoolExp b) Value

type SelectQuery  = DMLQuery (SelectQ  'Postgres)
type SelectQueryT = DMLQuery (SelectQT 'Postgres)

instance ToJSON a => ToJSON (DMLQuery (SelectG (SelCol 'Postgres) (BoolExp 'Postgres) a)) where
  toJSON (DMLQuery qt selQ) =
    object $ "table" .= qt : selectGToPairs selQ

type InsObj = ColumnValues Value

data ConflictAction
  = CAIgnore
  | CAUpdate
  deriving (Show, Eq, Lift)

instance FromJSON ConflictAction where
  parseJSON (String "ignore") = return CAIgnore
  parseJSON (String "update") = return CAUpdate
  parseJSON _ =
    fail "Expecting 'ignore' or 'update'"

instance ToJSON ConflictAction where
  toJSON CAUpdate = String "update"
  toJSON CAIgnore = String "ignore"

newtype ConstraintOn
  = ConstraintOn {getPGCols :: [PGCol]} deriving (Show, Eq, Lift, ToJSON)

instance FromJSON ConstraintOn where
  parseJSON v@(String _) =
    ConstraintOn . (:[]) <$> parseJSON v
  parseJSON v@(Array _) =
    ConstraintOn <$> parseJSON v
  parseJSON _ = fail
    "Expecting String or Array"

data OnConflict
  = OnConflict
  { ocConstraintOn :: !(Maybe ConstraintOn)
  , ocConstraint   :: !(Maybe ConstraintName)
  , ocAction       :: !ConflictAction
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''OnConflict)

data InsertQuery
  = InsertQuery
  { iqTable      :: !QualifiedTable
  , iqObjects    :: !Value
  , iqOnConflict :: !(Maybe OnConflict)
  , iqReturning  :: !(Maybe [PGCol])
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''InsertQuery)

data InsertTxConflictCtx
  = InsertTxConflictCtx
  { itcAction        :: !ConflictAction
  , itcConstraint    :: !(Maybe ConstraintName)
  , itcSetExpression :: !(Maybe Text)
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''InsertTxConflictCtx)

type UpdVals = ColumnValues Value

data UpdateQuery
  = UpdateQuery
  { uqTable     :: !QualifiedTable
  , uqWhere     :: !(BoolExp 'Postgres)
  , uqSet       :: !UpdVals
  , uqInc       :: !UpdVals
  , uqMul       :: !UpdVals
  , uqDefault   :: ![PGCol]
  , uqReturning :: !(Maybe [PGCol])
  } deriving (Show, Eq, Lift)

instance FromJSON UpdateQuery where
  parseJSON (Object o) =
    UpdateQuery
    <$> o .:  "table"
    <*> o .:  "where"
    <*> ((o .: "$set" <|> o .:? "values") .!= M.empty)
    <*> (o .:? "$inc" .!= M.empty)
    <*> (o .:? "$mul" .!= M.empty)
    <*> o .:? "$default" .!= []
    <*> o .:? "returning"
  parseJSON _ =
    fail "Expecting an object for update query"

instance ToJSON UpdateQuery where
  toJSON (UpdateQuery tn wc setE incE mulE defE ret) =
    object [ "table" .= tn
           , "where" .= wc
           , "$set" .= setE
           , "$inc" .= incE
           , "$mul" .= mulE
           , "$default" .= defE
           , "returning" .= ret
           ]

data DeleteQuery
  = DeleteQuery
  { doTable     :: !QualifiedTable
  , doWhere     :: !(BoolExp 'Postgres)  -- where clause
  , doReturning :: !(Maybe [PGCol]) -- columns returning
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''DeleteQuery)

data CountQuery
  = CountQuery
  { cqTable    :: !QualifiedTable
  , cqDistinct :: !(Maybe [PGCol])
  , cqWhere    :: !(Maybe (BoolExp 'Postgres))
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''CountQuery)

data QueryT
  = QTInsert !InsertQuery
  | QTSelect !SelectQueryT
  | QTUpdate !UpdateQuery
  | QTDelete !DeleteQuery
  | QTCount  !CountQuery
  | QTBulk   ![QueryT]
  deriving (Show, Eq, Lift)

$(deriveJSON
  defaultOptions { constructorTagModifier = snakeCase . drop 2
                 , sumEncoding = TaggedObject "type" "args"
                 }
  ''QueryT)
