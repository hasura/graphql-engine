-- | This module has the various metadata we want to attach to the
-- generated/executed query
module Hasura.QueryTags
  ( Attribute,
    LivequeryMetadata (LivequeryMetadata),
    MutationMetadata (MutationMetadata),
    QueryMetadata (QueryMetadata),
    QueryTags (QTLiveQuery, QTMutation, QTQuery),
    QueryTagsAttributes (_unQueryTagsAttributes),
    QueryTagsComment (..),
    emptyQueryTagsComment,
    encodeQueryTags,

    -- * Exposed for testing
    emptyQueryTagsAttributes,
  )
where

import Data.Text.Extended
import Hasura.GraphQL.Namespace (RootFieldAlias)
import Hasura.GraphQL.ParameterizedQueryHash
import Hasura.Prelude
import Hasura.Server.Types (RequestId (..))
import Language.GraphQL.Draft.Syntax qualified as GQL

data QueryTags
  = QTQuery !QueryMetadata
  | QTMutation !MutationMetadata
  | QTLiveQuery !LivequeryMetadata
  deriving (Show)

-- | query-tags as SQL comment which is appended to the prepared SQL statement
newtype QueryTagsComment = QueryTagsComment {_unQueryTagsComment :: Text} deriving (Show, Eq)

type Attribute = (Text, Text)

newtype QueryTagsAttributes = QueryTagsAttributes {_unQueryTagsAttributes :: [Attribute]} deriving (Show, Eq)

emptyQueryTagsAttributes :: QueryTagsAttributes
emptyQueryTagsAttributes = QueryTagsAttributes mempty

emptyQueryTagsComment :: QueryTagsComment
emptyQueryTagsComment = QueryTagsComment mempty

data QueryMetadata = QueryMetadata
  { qmRequestId :: !RequestId,
    qmOperationName :: !(Maybe GQL.Name),
    qmFieldName :: !RootFieldAlias,
    qmParameterizedQueryHash :: !ParameterizedQueryHash
  }
  deriving (Show)

data MutationMetadata = MutationMetadata
  { mmRequestId :: !RequestId,
    mmOperationName :: !(Maybe GQL.Name),
    mmFieldName :: !RootFieldAlias,
    mmParameterizedQueryHash :: !ParameterizedQueryHash
  }
  deriving (Show)

data LivequeryMetadata = LivequeryMetadata
  { lqmFieldName :: !RootFieldAlias,
    lqmParameterizedQueryHash :: !ParameterizedQueryHash
  }
  deriving (Show)

encodeQueryTags :: QueryTags -> QueryTagsAttributes
encodeQueryTags = \case
  QTQuery queryMetadata -> QueryTagsAttributes $ encodeQueryMetadata queryMetadata
  QTMutation mutationMetadata -> QueryTagsAttributes $ encodeMutationMetadata mutationMetadata
  QTLiveQuery livequeryMetadata -> QueryTagsAttributes $ encodeLivequeryMetadata livequeryMetadata
  where
    -- TODO: how do we want to encode RootFieldAlias?
    -- Currently uses ToTxt instance, which produces "namespace.fieldname"
    encodeQueryMetadata QueryMetadata {..} =
      [ ("request_id", unRequestId qmRequestId),
        ("field_name", toTxt qmFieldName),
        ("parameterized_query_hash", bsToTxt $ unParamQueryHash qmParameterizedQueryHash)
      ]
        <> operationNameAttributes qmOperationName

    encodeMutationMetadata MutationMetadata {..} =
      [ ("request_id", unRequestId mmRequestId),
        ("field_name", toTxt mmFieldName),
        ("parameterized_query_hash", bsToTxt $ unParamQueryHash mmParameterizedQueryHash)
      ]
        <> operationNameAttributes mmOperationName

    encodeLivequeryMetadata LivequeryMetadata {..} =
      [ ("field_name", toTxt lqmFieldName),
        ("parameterized_query_hash", bsToTxt $ unParamQueryHash lqmParameterizedQueryHash)
      ]

operationNameAttributes :: Maybe GQL.Name -> [(Text, Text)]
operationNameAttributes = maybe [] (\opName -> [("operation_name", GQL.unName opName)])
