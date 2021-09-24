-- | This module has the various metadata we want to attach to the
-- generated/executed query
module Hasura.QueryTags where

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

emptyQueryTagsComment :: QueryTagsComment
emptyQueryTagsComment = QueryTagsComment mempty

data QueryMetadata = QueryMetadata
  { qmRequestId :: !RequestId,
    qmOperationName :: !(Maybe GQL.Name),
    qmFieldName :: !GQL.Name,
    qmParameterizedQueryHash :: !ParameterizedQueryHash
  }
  deriving (Show)

data MutationMetadata = MutationMetadata
  { mmRequestId :: !RequestId,
    mmOperationName :: !(Maybe GQL.Name),
    mmFieldName :: !GQL.Name,
    mmParameterizedQueryHash :: !ParameterizedQueryHash
  }
  deriving (Show)

data LivequeryMetadata = LivequeryMetadata
  { lqmFieldName :: !GQL.Name,
    lqmParameterizedQueryHash :: !ParameterizedQueryHash
  }
  deriving (Show)

encodeQueryTags :: QueryTags -> QueryTagsAttributes
encodeQueryTags = \case
  QTQuery queryMetadata -> QueryTagsAttributes $ encodeQueryMetadata queryMetadata
  QTMutation mutationMetadata -> QueryTagsAttributes $ encodeMutationMetadata mutationMetadata
  QTLiveQuery livequeryMetadata -> QueryTagsAttributes $ encodeLivequeryMetadata livequeryMetadata
  where
    encodeQueryMetadata QueryMetadata {..} =
      [ ("request_id", unRequestId qmRequestId),
        ("field_name", GQL.unName qmFieldName),
        ("parameterized_query_hash", bsToTxt $ unParamQueryHash qmParameterizedQueryHash)
      ]
        <> operationNameAttributes qmOperationName

    encodeMutationMetadata MutationMetadata {..} =
      [ ("request_id", unRequestId mmRequestId),
        ("field_name", GQL.unName mmFieldName),
        ("parameterized_query_hash", bsToTxt $ unParamQueryHash mmParameterizedQueryHash)
      ]
        <> operationNameAttributes mmOperationName

    encodeLivequeryMetadata LivequeryMetadata {..} =
      [ ("field_name", GQL.unName lqmFieldName),
        ("parameterized_query_hash", bsToTxt $ unParamQueryHash lqmParameterizedQueryHash)
      ]

operationNameAttributes :: Maybe GQL.Name -> [(Text, Text)]
operationNameAttributes = maybe [] (\opName -> [("operation_name", GQL.unName opName)])
