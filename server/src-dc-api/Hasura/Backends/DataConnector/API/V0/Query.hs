{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

--
module Hasura.Backends.DataConnector.API.V0.Query
  ( Query (..),
    Field (..),
    RelField (..),
    ForeignKey (..),
    PrimaryKey (..),
  )
where

--------------------------------------------------------------------------------

import Autodocodec.Extended
import Autodocodec.OpenAPI ()
import Control.Lens.TH (makePrisms)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Data (Data)
import Data.HashMap.Strict qualified as M
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hasura.Backends.DataConnector.API.V0.Column qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.Expression qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.OrderBy qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.Table qualified as API.V0
import Prelude

--------------------------------------------------------------------------------

-- | A serializable request to retrieve strutured data from some
-- source.
data Query = Query
  { fields :: M.HashMap Text Field,
    from :: API.V0.TableName,
    limit :: Maybe Int,
    offset :: Maybe Int,
    where_ :: Maybe API.V0.Expression,
    orderBy :: Maybe (NonEmpty API.V0.OrderBy)
  }
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec Query

instance HasCodec Query where
  codec =
    -- named "query" $
    object "Query" $
      Query
        <$> requiredField "fields" "Fields of the query" .= fields
        <*> requiredField "from" "Source table" .= from
        <*> optionalFieldOrNull "limit" "Optionally limit to N results" .= limit
        <*> optionalFieldOrNull "offset" "Optionally offset from the Nth result" .= offset
        <*> optionalFieldOrNull "where" "Optionally constrain the results to satisfy some predicate" .= where_
        <*> optionalFieldOrNull "order_by" "Optionally order the results by the value of one or more fields" .= orderBy

--------------------------------------------------------------------------------

data RelField = RelField
  { columnMapping :: M.HashMap PrimaryKey ForeignKey,
    query :: Query
  }
  deriving stock (Eq, Ord, Show, Generic, Data)

instance HasObjectCodec RelField where
  objectCodec =
    RelField
      <$> requiredField "column_mapping" "Mapping from local fields to remote fields" .= columnMapping
      <*> requiredField "query" "Relationship query" .= query

--------------------------------------------------------------------------------

newtype PrimaryKey = PrimaryKey {unPrimaryKey :: API.V0.ColumnName}
  deriving stock (Data, Generic)
  deriving newtype (Eq, Hashable, Ord, Show, ToJSONKey, FromJSONKey)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec PrimaryKey

instance HasCodec PrimaryKey where
  codec = dimapCodec PrimaryKey unPrimaryKey codec

--------------------------------------------------------------------------------

newtype ForeignKey = ForeignKey {unForeignKey :: API.V0.ColumnName}
  deriving stock (Data, Generic)
  deriving newtype (Eq, Hashable, Ord, Show)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec ForeignKey

instance HasCodec ForeignKey where
  codec = dimapCodec ForeignKey unForeignKey codec

--------------------------------------------------------------------------------

-- | A serializable field targeted by a 'Query'.
data Field
  = ColumnField (ValueWrapper "column" API.V0.ColumnName)
  | RelationshipField RelField
  deriving stock (Eq, Ord, Show, Generic, Data)

$(makePrisms ''Field)

instance HasCodec Field where
  codec =
    named "Field" $
      sumTypeCodec
        [ TypeAlternative "ColumnField" "column" _ColumnField,
          TypeAlternative "RelationshipField" "relationship" _RelationshipField
        ]

deriving via Autodocodec Field instance FromJSON Field

deriving via Autodocodec Field instance ToJSON Field

deriving via Autodocodec Field instance ToSchema Field
