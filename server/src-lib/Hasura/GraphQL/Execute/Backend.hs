module Hasura.GraphQL.Execute.Backend where

import           Hasura.Prelude

import qualified Data.Aeson                              as J
import qualified Data.Aeson.Casing                       as J
import qualified Data.Aeson.Ordered                      as JO
import qualified Data.ByteString                         as B
import qualified Data.HashMap.Strict                     as Map
import qualified Data.List.NonEmpty                      as NE
import qualified Language.GraphQL.Draft.Syntax           as G

import           Control.Monad.Trans.Control             (MonadBaseControl)
import           Data.Kind                               (Type)
import           Data.Text.Extended
import           Data.Text.NonEmpty                      (mkNonEmptyTextUnsafe)

import qualified Hasura.GraphQL.Transport.HTTP.Protocol  as GH
import qualified Hasura.Logging                          as L

import           Hasura.Base.Error
import           Hasura.EncJSON
import           Hasura.GraphQL.Execute.LiveQuery.Plan
import           Hasura.GraphQL.Execute.RemoteJoin.Types
import           Hasura.GraphQL.Logging                  (MonadQueryLog)
import           Hasura.GraphQL.Parser                   hiding (Type)
import           Hasura.RQL.IR
import           Hasura.RQL.Types.Backend
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.RemoteSchema
import           Hasura.SQL.Backend
import           Hasura.Server.Types                     (RequestId)
import           Hasura.Server.Version                   (HasVersion)
import           Hasura.Session
import           Hasura.Tracing


-- | This typeclass enacapsulates how a given backend fetches the data for a
-- root field
class ( Backend b
      , ToTxt (MultiplexedQuery b)
      ) => BackendExecute (b :: BackendType) where
  type MultiplexedQuery b :: Type

  executeQueryField
    :: forall m
     . ( MonadIO m
       , MonadError QErr m
       , MonadQueryLog m
       , MonadTrace m
       )
    => RequestId
    -> L.Logger L.Hasura
    -> UserInfo
    -> SourceName
    -> SourceConfig b
    -> QueryDB b (Const Void) (UnpreparedValue b)
    -> m EncJSON
  executeMutationField
    :: forall m
     . ( MonadIO m
       , MonadError QErr m
       , HasVersion
       , MonadQueryLog m
       , MonadTrace m
       )
    => RequestId
    -> L.Logger L.Hasura
    -> UserInfo
    -> Bool
    -> SourceName
    -> SourceConfig b
    -> MutationDB b (Const Void) (UnpreparedValue b)
    -> m EncJSON
  makeLiveQueryPlan
    :: forall m
     . ( MonadError QErr m
       , MonadIO m
       )
    => UserInfo
    -> SourceName
    -> SourceConfig b
    -> InsOrdHashMap G.Name (QueryDB b (Const Void) (UnpreparedValue b))
    -> m (LiveQueryPlan b (MultiplexedQuery b))
  executeMultiplexedQuery
    :: forall m
     . ( MonadIO m
       )
    => SourceConfig b
    -> MultiplexedQuery b
    -> [(CohortId, CohortVariables)]
    -- ^ WARNING: Postgres-specific, ignored by other backends
    -> m (DiffTime, Either QErr [(CohortId, B.ByteString)])
  explainQueryField
    :: forall m
     . ( MonadError QErr m
       , MonadIO m
       )
    => G.Name
    -> UserInfo
    -> SourceName
    -> SourceConfig b
    -> QueryDB b (Const Void) (UnpreparedValue b)
    -> m EncJSON
  explainLiveQuery
    :: ( MonadError QErr m
      , MonadIO m
      , MonadBaseControl IO m
      )
    => LiveQueryPlan b (MultiplexedQuery b)
    -> m LiveQueryPlanExplanation

  -- | Execute a remote relationship to this source
  executeRemoteRelationship
    :: forall m
     . ( MonadIO m
       , MonadError QErr m
       , MonadQueryLog m
       , MonadTrace m
       )
    => RequestId
    -> L.Logger L.Hasura
    -> UserInfo
    -> SourceName
    -> SourceConfig b
    -> NE.NonEmpty J.Object
    -- ^ List of json objects, each of which becomes a row of the table
    -> Map.HashMap FieldName (Column b, ScalarType b)
    -- ^ The above objects have this schema
    -> FieldName
    -- ^ This is a field name from the lhs that *has* to be selected in the
    -- response along with the relationship
    -> (FieldName, SourceRelationshipSelection b (Const Void) UnpreparedValue)
    -> m EncJSON

-- | This is a helper function to convert a remote source's relationship to a
-- normal relationship to a temporary table. This function can be used to
-- implement executeRemoteRelationship function in databases which support
-- constructing a temporary table for a list of json objects.
convertRemoteSourceRelationship
  :: Map.HashMap (Column b) (Column b)
  -- ^ Join columns for the relationship
  -> SelectFromG b (UnpreparedValue b)
  -- ^ The LHS of the join, this is the expression which selects from json
  -- objects
  -> ColumnInfo b
  -- ^ This is the __argument__ id column, that needs to be added to the response
  -- This is used by by the remote joins processing logic to convert the
  -- response from upstream to join indices
  -> (FieldName, SourceRelationshipSelection b (Const Void) UnpreparedValue)
  -- ^ The relationship column and its name (how it should be selected in the
  -- response)
  -> QueryDB b (Const Void) (UnpreparedValue b)
convertRemoteSourceRelationship columnMapping selectFrom argumentIdColumn
  (relationshipName, relationship) =
  QDBMultipleRows simpleSelect
  where
    -- TODO: FieldName should have also been a wrapper around NonEmptyText
    relName = RelName $ mkNonEmptyTextUnsafe $ getFieldNameTxt relationshipName

    relationshipField = case relationship of
      SourceRelationshipObject s ->
        AFObjectRelation $ AnnRelationSelectG relName columnMapping s
      SourceRelationshipArray s ->
        AFArrayRelation $ ASSimple $ AnnRelationSelectG relName columnMapping s
      SourceRelationshipArrayAggregate s ->
        AFArrayRelation $ ASAggregate $ AnnRelationSelectG relName columnMapping s

    argumentIdField =
      ( FieldName "__argument__id"
      , AFColumn $
        AnnColumnField { _acfInfo = argumentIdColumn
                       , _acfAsText = False
                       , _acfOp = Nothing
                       , _acfCaseBoolExpression = Nothing
                       }
      )

    simpleSelect =
      AnnSelectG { _asnFields = [argumentIdField, (relationshipName, relationshipField)]
                 , _asnFrom = selectFrom
                 , _asnPerm = TablePerm annBoolExpTrue Nothing
                 , _asnArgs = noSelectArgs
                 , _asnStrfyNum = False
                 }

-- | The result of an explain query: for a given root field (denoted by its
-- name): the generated SQL query, and the detailed explanation obtained from
-- the database (if any). We mostly use this type as an intermediary step, and
-- immediately tranform any value we obtain into an equivalent JSON
-- representation.
data ExplainPlan
  = ExplainPlan
  { _fpField :: !G.Name
  , _fpSql   :: !(Maybe Text)
  , _fpPlan  :: !(Maybe [Text])
  } deriving (Show, Eq, Generic)

instance J.ToJSON ExplainPlan where
  toJSON = J.genericToJSON $ J.aesonPrefix J.camelCase

type QueryFieldExecution
  = RootField
      (QueryDBOnlyField UnpreparedValue, Maybe RemoteJoins)
      (RemoteSchemaInfo, GH.GQLReqOutgoing)
      (QueryActionOnlyField UnpreparedValue, Maybe RemoteJoins)
      JO.Value

type MutationFieldExecution
  = RootField
      (MutationDBOnlyField UnpreparedValue, Maybe RemoteJoins, SQLGenCtx)
      (RemoteSchemaInfo, GH.GQLReqOutgoing)
      (MutationActionOnlyField UnpreparedValue, Maybe RemoteJoins)
      JO.Value

type ExecutionPlan root = InsOrdHashMap G.Name root
type QueryExecutionPlan = ExecutionPlan QueryFieldExecution
type MutationExecutionPlan = ExecutionPlan MutationFieldExecution
