module Hasura.RQL.Types.DML
       ( BoolExp(..)
       , ColExp(..)
       , DMLQuery(..)
       , OrderType(..)
       , NullsOrder(..)

       , OrderByExp(..)
       , OrderByItemG(..)
       , OrderByItem
       , OrderByCol(..)

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

import qualified Hasura.SQL.DML             as S

import           Hasura.Incremental         (Cacheable)
import           Hasura.Prelude
import           Hasura.RQL.Types.BoolExp
import           Hasura.RQL.Types.Common
import           Hasura.SQL.Types

import           Control.Lens.TH
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import qualified Data.Attoparsec.Text       as Atto
import qualified Data.Attoparsec.Text       as AT
import qualified Data.Attoparsec.Types      as AttoT
import qualified Data.HashMap.Strict        as M
import qualified Data.Text                  as T
import           Hasura.RQL.Instances       ()
import           Instances.TH.Lift          ()
import           Language.Haskell.TH.Syntax (Lift)

data ColExp
  = ColExp
  { ceCol :: !FieldName
  , ceVal :: !Value
  } deriving (Show, Eq, Lift, Data, Generic)
instance NFData ColExp
instance Cacheable ColExp

newtype BoolExp
  = BoolExp { unBoolExp :: GBoolExp ColExp }
  deriving (Show, Eq, Lift, Generic, NFData, Cacheable)

$(makeWrapped ''BoolExp)

instance ToJSON BoolExp where
  toJSON (BoolExp gBoolExp) =
    gBoolExpToJSON f gBoolExp
    where
      f (ColExp k v) =
        (getFieldNameTxt k,  v)

instance FromJSON BoolExp where
  parseJSON =
    fmap BoolExp . parseGBoolExp f
    where
      f (k, v) = ColExp (FieldName k) <$> parseJSON v

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

newtype OrderType
  = OrderType { unOrderType :: S.OrderType }
  deriving (Show, Eq, Lift, Generic)

instance FromJSON OrderType where
  parseJSON =
    fmap OrderType . f
    where f = $(mkParseJSON
                defaultOptions{constructorTagModifier = snakeCase . drop 2}
                ''S.OrderType)

newtype NullsOrder
  = NullsOrder { unNullsOrder :: S.NullsOrder }
  deriving (Show, Eq, Lift, Generic)

instance FromJSON NullsOrder where
  parseJSON =
    fmap NullsOrder . f
    where f = $(mkParseJSON
                defaultOptions{constructorTagModifier = snakeCase . drop 1}
                ''S.NullsOrder)

instance ToJSON OrderType where
  toJSON =
    f . unOrderType
    where f = $(mkToJSON
                defaultOptions{constructorTagModifier = snakeCase . drop 2}
                ''S.OrderType)

instance ToJSON NullsOrder where
  toJSON =
    f . unNullsOrder
    where f = $(mkToJSON
                defaultOptions{constructorTagModifier = snakeCase . drop 1}
                ''S.NullsOrder)

data OrderByCol
  = OCPG !FieldName
  | OCRel !FieldName !OrderByCol
  deriving (Show, Eq, Lift)

orderByColToTxt :: OrderByCol -> Text
orderByColToTxt = \case
  OCPG pgCol -> getFieldNameTxt pgCol
  OCRel rel obCol -> getFieldNameTxt rel <> "." <> orderByColToTxt obCol

instance ToJSON OrderByCol where
  toJSON = toJSON . orderByColToTxt

orderByColFromToks
  :: (MonadFail m)
  => [T.Text] -> m OrderByCol
orderByColFromToks toks = do
  when (any T.null toks) $ fail "col/rel cannot be empty"
  case toks of
    []   -> fail "failed to parse an OrderByCol: found empty cols"
    x:xs -> return $ go (FieldName x) xs
  where
    go fld = \case
      []   -> OCPG fld
      x:xs -> OCRel fld $ go (FieldName x) xs

orderByColFromTxt
  :: (MonadFail m)
  => T.Text -> m OrderByCol
orderByColFromTxt =
  orderByColFromToks . T.split (=='.')

instance FromJSON OrderByCol where
  parseJSON = \case
    (String t) -> orderByColFromToks $ T.split (=='.') t
    v          -> parseJSON v >>= orderByColFromToks

data OrderByItemG a
  = OrderByItemG
  { obiType   :: !(Maybe OrderType)
  , obiColumn :: !a
  , obiNulls  :: !(Maybe NullsOrder)
  } deriving (Show, Eq, Lift, Functor, Foldable, Traversable)

type OrderByItem = OrderByItemG OrderByCol

$(deriveToJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''OrderByItemG)

-- Can either be string / object
instance FromJSON OrderByItem where
  parseJSON (String t) =
    case Atto.parseOnly orderByParser t of
    Right r -> return r
    Left _  ->
      fail "string format for 'order_by' entry : {+/-}column Eg : +posted"

  parseJSON (Object o) =
    OrderByItemG
    <$> o .:? "type"
    <*> o .:  "column"
    <*> o .:? "nulls"
  parseJSON _ = fail "expecting an object or string for order by"

newtype OrderByExp
  = OrderByExp { getOrderByItems :: [OrderByItem] }
  deriving (Show, Eq, ToJSON, Lift)

instance FromJSON OrderByExp where
  parseJSON v@(String _) =
    OrderByExp . (:[]) <$> parseJSON v
  parseJSON v@(Array _) =
    OrderByExp <$> parseJSON v
  parseJSON v@(Object _) =
    OrderByExp . (:[]) <$> parseJSON v
  parseJSON _ =
    fail "Expecting : array/string/object"

orderByParser :: AttoT.Parser T.Text OrderByItem
orderByParser =
  OrderByItemG <$> otP <*> colP <*> return Nothing
  where
    otP  = ("+" *> return (Just $ OrderType S.OTAsc))
           <|> ("-" *> return (Just $ OrderType S.OTDesc))
           <|> return Nothing
    colP = Atto.takeText >>= orderByColFromTxt

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

wcToText :: Wildcard -> T.Text
wcToText Star         = "*"
wcToText (StarDot wc) = "*." <> wcToText wc

parseWildcard :: AT.Parser Wildcard
parseWildcard =
  fromList <$> ((starParser `AT.sepBy1` AT.char '.') <* AT.endOfInput)
  where
    starParser = AT.char '*' *> pure Star
    fromList   = foldr1 (\_ x -> StarDot x)

-- Columns in RQL
data SelCol
  = SCStar !Wildcard
  | SCExtSimple !PGCol
  | SCExtRel !RelName !(Maybe RelName) !SelectQ
  deriving (Show, Eq, Lift)

instance FromJSON SelCol where
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

instance ToJSON SelCol where
  toJSON (SCStar wc) = String $ wcToText wc
  toJSON (SCExtSimple s) = toJSON s
  toJSON (SCExtRel rn mrn selq) =
    object $ [ "name" .= rn
             , "alias" .= mrn
             ] ++ selectGToPairs selq

type SelectQ = SelectG SelCol BoolExp Int
type SelectQT = SelectG SelCol BoolExp Value

type SelectQuery = DMLQuery SelectQ
type SelectQueryT = DMLQuery SelectQT

instance ToJSON SelectQuery where
  toJSON (DMLQuery qt selQ) =
    object $ "table" .= qt : selectGToPairs selQ

instance ToJSON SelectQueryT where
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
  , itcSetExpression :: !(Maybe T.Text)
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''InsertTxConflictCtx)

type UpdVals = ColumnValues Value

data UpdateQuery
  = UpdateQuery
  { uqTable     :: !QualifiedTable
  , uqWhere     :: !BoolExp
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
  , doWhere     :: !BoolExp  -- where clause
  , doReturning :: !(Maybe [PGCol]) -- columns returning
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''DeleteQuery)

data CountQuery
  = CountQuery
  { cqTable    :: !QualifiedTable
  , cqDistinct :: !(Maybe [PGCol])
  , cqWhere    :: !(Maybe BoolExp)
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''CountQuery)

data QueryT
  = QTInsert !InsertQuery
  | QTSelect !SelectQueryT
  | QTUpdate !UpdateQuery
  | QTDelete !DeleteQuery
  | QTCount !CountQuery
  | QTBulk ![QueryT]
  deriving (Show, Eq, Lift)

$(deriveJSON
  defaultOptions { constructorTagModifier = snakeCase . drop 2
                 , sumEncoding = TaggedObject "type" "args"
                 }
  ''QueryT)
