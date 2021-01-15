module Hasura.RQL.IR.OrderBy
  ( OrderByCol(..)
  , OrderByItemG(..)
  , OrderByItem
  -- used by RQL.DML.Types
  , orderByColFromTxt
  ) where

import           Hasura.Prelude

import qualified Data.Text                  as T

import           Data.Aeson
import           Data.Aeson.Casing

import           Hasura.RQL.Instances       ()
import           Hasura.RQL.Types.Common
import           Hasura.SQL.Backend


-- order by col

data OrderByCol
  = OCPG !FieldName
  | OCRel !FieldName !OrderByCol
  deriving (Show, Eq)

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

data OrderByItemG (b :: BackendType) a
  = OrderByItemG
  { obiType   :: !(Maybe (BasicOrderType b))
  , obiColumn :: !a
  , obiNulls  :: !(Maybe (NullsOrderType b))
  } deriving (Functor, Foldable, Traversable, Generic)
deriving instance (Backend b, Show a) => Show (OrderByItemG b a)
deriving instance (Backend b, Eq a)   => Eq   (OrderByItemG b a)
instance (Backend b, Hashable a) => Hashable (OrderByItemG b a)

type OrderByItem b = OrderByItemG b OrderByCol

instance (Backend b, FromJSON a) => FromJSON (OrderByItemG b a) where
  parseJSON = genericParseJSON (aesonPrefix snakeCase){omitNothingFields=True}

instance (Backend b, ToJSON a) => ToJSON (OrderByItemG b a) where
  toJSON = genericToJSON (aesonPrefix snakeCase){omitNothingFields=True}
