{-# LANGUAGE TemplateHaskell #-}

module Hasura.RQL.IR.Action
  ( ActionFieldG (..),
    ActionFieldsG,
    ActionFields,
    ActionRemoteRelationshipSelect (..),
    _ACFExpression,
    _ACFNestedObject,
    _ACFRemote,
    _ACFScalar,
    AnnActionExecution (..),
    aaeName,
    aaeOutputType,
    aaeFields,
    aaePayload,
    aaeOutputFields,
    aaeWebhook,
    aaeHeaders,
    aaeForwardClientHeaders,
    aaeTimeOut,
    aaeRequestTransform,
    aaeResponseTransform,
    AnnActionMutationAsync (..),
    AsyncActionQueryFieldG (..),
    _AsyncTypename,
    _AsyncOutput,
    _AsyncId,
    _AsyncCreatedAt,
    _AsyncErrors,
    AnnActionAsyncQuery (..),
    aaaqName,
    aaaqActionId,
    aaaqOutputType,
    aaaqFields,
    aaaqDefinitionList,
    aaaqStringifyNum,
    aaaqForwardClientHeaders,
    aaaqSource,
    ActionSourceInfo (..),
    ActionOutputFields,
    getActionOutputFields,
  )
where

import Control.Lens (makeLenses, makePrisms)
import Data.Aeson qualified as J
import Data.HashMap.Strict qualified as HashMap
import Data.Kind (Type)
import Hasura.Prelude
import Hasura.RQL.DDL.Webhook.Transform (MetadataResponseTransform, RequestTransform)
import Hasura.RQL.Types.Action qualified as RQL
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common (EnvRecord, FieldName, Fields, ResolvedWebhook, SourceName, Timeout)
import Hasura.RQL.Types.CustomTypes
  ( AnnotatedObjectType (..),
    AnnotatedOutputType (..),
    GraphQLType (..),
    ObjectFieldDefinition (..),
    ObjectFieldName (..),
  )
import Hasura.RQL.Types.Headers
import Hasura.RQL.Types.Schema.Options (StringifyNumbers)
import Language.GraphQL.Draft.Syntax qualified as G

-- | Internal representation for a selection of fields on the result of an action.
-- Type parameter r will be either
-- r ~ (RemoteRelationshipField UnpreparedValue) when the AST is emitted by the parser.
-- r ~ Void when an execution tree is constructed so that a backend is
-- absolved of dealing with remote relationships.
data ActionFieldG (r :: Type)
  = -- | Scalar value. G.Name is the original field name from the object type.
    ACFScalar G.Name
  | -- | Remote relationship
    ACFRemote (ActionRemoteRelationshipSelect r)
  | -- | Constant text value (used for __typename fields)
    ACFExpression Text
  | -- | Nested object. G.Name is the original field name from the object type.
    ACFNestedObject G.Name (ActionFieldsG r)
  deriving (Eq, Show, Functor, Foldable, Traversable)

type ActionFieldsG r = Fields (ActionFieldG r)

type ActionFields = ActionFieldsG Void

data ActionRemoteRelationshipSelect r = ActionRemoteRelationshipSelect
  { -- | The fields on the table that are required for the join condition
    -- of the remote relationship
    _arrsLHSJoinFields :: HashMap FieldName G.Name,
    -- | The field that captures the relationship
    -- r ~ (RemoteRelationshipField UnpreparedValue) when the AST is emitted by the parser.
    -- r ~ Void when an execution tree is constructed so that a backend is
    -- absolved of dealing with remote relationships.
    _arrsRelationship :: r
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

$(makePrisms ''ActionFieldG)

data AnnActionExecution (r :: Type) = AnnActionExecution
  { _aaeName :: RQL.ActionName,
    -- | output type
    _aaeOutputType :: GraphQLType,
    -- | output selection
    _aaeFields :: (ActionFieldsG r),
    -- | jsonified input arguments
    _aaePayload :: J.Value,
    -- | to validate the response fields from webhook
    _aaeOutputFields :: ActionOutputFields,
    _aaeWebhook :: EnvRecord ResolvedWebhook,
    _aaeHeaders :: [HeaderConf],
    _aaeForwardClientHeaders :: Bool,
    _aaeTimeOut :: Timeout,
    _aaeRequestTransform :: Maybe RequestTransform,
    _aaeResponseTransform :: Maybe MetadataResponseTransform
  }
  deriving stock (Functor, Foldable, Traversable)

type ActionOutputFields = HashMap.HashMap G.Name G.GType

getActionOutputFields :: AnnotatedOutputType -> ActionOutputFields
getActionOutputFields inp = case inp of
  AOTObject aot -> HashMap.fromList do
    ObjectFieldDefinition {..} <- toList $ _aotFields aot
    pure (unObjectFieldName _ofdName, fst _ofdType)
  AOTScalar _ -> HashMap.empty

data AnnActionMutationAsync = AnnActionMutationAsync
  { _aamaName :: RQL.ActionName,
    _aamaForwardClientHeaders :: Bool,
    -- | jsonified input arguments
    _aamaPayload :: J.Value
  }
  deriving (Show, Eq)

data AsyncActionQueryFieldG (r :: Type)
  = AsyncTypename Text
  | AsyncOutput (ActionFieldsG r)
  | AsyncId
  | AsyncCreatedAt
  | AsyncErrors
  deriving stock (Functor, Foldable, Traversable)

type AsyncActionQueryFieldsG r = Fields (AsyncActionQueryFieldG r)

data AnnActionAsyncQuery (b :: BackendType) (r :: Type) = AnnActionAsyncQuery
  { _aaaqName :: RQL.ActionName,
    _aaaqActionId :: RQL.ActionId,
    _aaaqOutputType :: GraphQLType,
    _aaaqFields :: AsyncActionQueryFieldsG r,
    _aaaqDefinitionList :: [(Column b, ScalarType b)],
    _aaaqStringifyNum :: StringifyNumbers,
    _aaaqForwardClientHeaders :: Bool,
    _aaaqSource :: ActionSourceInfo b
  }
  deriving stock (Functor, Foldable, Traversable)

data ActionSourceInfo b
  = -- | No relationships defined on the action output object
    ASINoSource
  | -- | All relationships refer to tables in one source
    ASISource SourceName (SourceConfig b)

$(makeLenses ''AnnActionAsyncQuery)
$(makeLenses ''AnnActionExecution)
$(makePrisms ''AsyncActionQueryFieldG)
