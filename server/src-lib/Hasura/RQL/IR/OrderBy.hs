module Hasura.RQL.IR.OrderBy
  ( OrderType(..)
  , NullsOrder(..)
  , OrderByExp(..)
  , OrderByItemG(..)
  , OrderByItem
  , OrderByCol(..)
  ) where

import           Hasura.Prelude

import qualified Data.Attoparsec.Text             as Atto
import qualified Data.Attoparsec.Types            as AttoT
import qualified Data.Text                        as T

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Instances.TH.Lift                ()
import           Language.Haskell.TH.Syntax       (Lift)

import qualified Hasura.Backends.Postgres.SQL.DML as S

import           Hasura.RQL.Instances             ()
import           Hasura.RQL.Types.Common


-- order type

newtype OrderType
  = OrderType { unOrderType :: S.OrderType }
  deriving (Show, Eq, Lift, Generic)
instance Hashable OrderType

instance FromJSON OrderType where
  parseJSON =
    fmap OrderType . f
    where f = $(mkParseJSON
                defaultOptions{constructorTagModifier = snakeCase . drop 2}
                ''S.OrderType)


-- nulls order

newtype NullsOrder
  = NullsOrder { unNullsOrder :: S.NullsOrder }
  deriving (Show, Eq, Lift, Generic)
instance Hashable NullsOrder

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


-- order by col

data OrderByCol
  = OCPG !FieldName
  | OCRel !FieldName !OrderByCol
  deriving (Show, Eq, Lift)

instance FromJSON OrderByCol where
  parseJSON = \case
    (String t) -> orderByColFromToks $ T.split (=='.') t
    v          -> parseJSON v >>= orderByColFromToks

instance ToJSON OrderByCol where
  toJSON = toJSON . orderByColToTxt

orderByColToTxt :: OrderByCol -> Text
orderByColToTxt = \case
  OCPG pgCol      -> getFieldNameTxt pgCol
  OCRel rel obCol -> getFieldNameTxt rel <> "." <> orderByColToTxt obCol

orderByColFromToks
  :: (MonadFail m)
  => [Text] -> m OrderByCol
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
  => Text -> m OrderByCol
orderByColFromTxt =
  orderByColFromToks . T.split (=='.')


-- order by item

data OrderByItemG a
  = OrderByItemG
  { obiType   :: !(Maybe OrderType)
  , obiColumn :: !a
  , obiNulls  :: !(Maybe NullsOrder)
  } deriving (Show, Eq, Lift, Functor, Foldable, Traversable, Generic)
instance (Hashable a) => Hashable (OrderByItemG a)

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

orderByParser :: AttoT.Parser Text OrderByItem
orderByParser =
  OrderByItemG <$> otP <*> colP <*> return Nothing
  where
    otP  = ("+" *> return (Just $ OrderType S.OTAsc))
           <|> ("-" *> return (Just $ OrderType S.OTDesc))
           <|> return Nothing
    colP = Atto.takeText >>= orderByColFromTxt
