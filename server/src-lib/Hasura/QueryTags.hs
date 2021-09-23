-- | This module has the various metadata we want to attach to the
-- generated/executed query

module Hasura.QueryTags where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                   as Map

import qualified Hasura.RQL.Types.Common               as RQL
import qualified Hasura.RQL.Types.QueryTags            as RQL
import qualified Language.GraphQL.Draft.Syntax         as GQL

import           Hasura.GraphQL.ParameterizedQueryHash
import           Hasura.Server.Types                   (RequestId (..))

data QueryTags
  = QTQuery !QueryMetadata
  | QTMutation !MutationMetadata
  | QTLiveQuery !LivequeryMetadata
  deriving (Show)

-- | query-tags as SQL comment which is appended to the prepared SQL statement
newtype QueryTagsComment = QueryTagsComment {_unQueryTagsComment :: Text}

data QueryMetadata
  = QueryMetadata
  { qmRequestId              :: !RequestId
  , qmOperationName          :: !(Maybe GQL.Name)
  , qmFieldName              :: !GQL.Name
  , qmParameterizedQueryHash :: !ParameterizedQueryHash
  } deriving (Show)

data MutationMetadata
  = MutationMetadata
  { mmRequestId              :: !RequestId
  , mmOperationName          :: !(Maybe GQL.Name)
  , mmFieldName              :: !GQL.Name
  , mmParameterizedQueryHash :: !ParameterizedQueryHash
  } deriving (Show)

data LivequeryMetadata
  = LivequeryMetadata
  { lqmFieldName              :: !GQL.Name
  , lqmParameterizedQueryHash :: !ParameterizedQueryHash
  } deriving (Show)

encodeQueryTags :: QueryTags -> [(Text, Text)]
encodeQueryTags = \case
    QTQuery    queryMetadata      -> encodeQueryMetadata queryMetadata
    QTMutation mutationMetadata   -> encodeMutationMetadata mutationMetadata
    QTLiveQuery livequeryMetadata -> encodeLivequeryMetadata livequeryMetadata

  where
    encodeQueryMetadata QueryMetadata{..} =
      [ ("request_id", unRequestId qmRequestId)
      , ("field_name", GQL.unName qmFieldName)
      , ("parameterized_query_hash", bsToTxt $ unParamQueryHash qmParameterizedQueryHash)
      ] <> operationNameAttributes qmOperationName

    encodeMutationMetadata MutationMetadata{..} =
      [ ("request_id", unRequestId mmRequestId)
      , ("field_name", GQL.unName mmFieldName)
      , ("parameterized_query_hash", bsToTxt $ unParamQueryHash mmParameterizedQueryHash)
      ] <> operationNameAttributes mmOperationName

    encodeLivequeryMetadata LivequeryMetadata{..} =
      [ ("field_name", GQL.unName lqmFieldName)
      , ("parameterized_query_hash", bsToTxt $ unParamQueryHash lqmParameterizedQueryHash)
      ]

encodeOptionalQueryTags :: Maybe QueryTags -> [(Text, Text)]
encodeOptionalQueryTags = \case
  Just qt -> encodeQueryTags qt
  Nothing -> []

operationNameAttributes :: Maybe GQL.Name -> [(Text, Text)]
operationNameAttributes = maybe [] (\opName -> [("operation_name", GQL.unName opName)])

{-| Extract the Query Tags configuration for the source

Default config for Query tags:
  * Default Query Tag Format is Standard
  * Is enabled for all sources

If no configuration is set in the metadata for the 'Query Tags' we use the the Default Config as
mentioned above.
-}
getQueryTagsSourceConfig :: RQL.QueryTagsConfig -> RQL.SourceName -> RQL.QueryTagsSourceConfig
getQueryTagsSourceConfig qtConfig sourceName =

  case Map.lookup sourceNameText (RQL._qtmcPerSourceConfiguration qtConfig) of
    -- If the 'Query Tags' is not configured for source, Use the 'default config' set in metadata.
    Nothing -> RQL.QueryTagsSourceConfig defaultQueryTagsFormat False
    -- If 'Query Tags' is configured for the source but the 'format' is not set then use the 'default_format'
    Just (RQL.QueryTagsPerSourceConfig Nothing disabled) ->
        RQL.QueryTagsSourceConfig defaultQueryTagsFormat disabled
    Just (RQL.QueryTagsPerSourceConfig (Just format) disabled) ->
        RQL.QueryTagsSourceConfig format disabled

  where
    sourceNameText = RQL.sourceNameToText sourceName
    defaultQueryTagsFormat = (RQL._qtmcDefaultFormat qtConfig)
