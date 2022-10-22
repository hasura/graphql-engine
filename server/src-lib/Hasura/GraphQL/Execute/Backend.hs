module Hasura.GraphQL.Execute.Backend where

import           Hasura.Prelude

import qualified Data.Aeson                              as J
import qualified Data.Aeson.Casing                       as J
import qualified Data.Aeson.Ordered                      as JO
import qualified Language.GraphQL.Draft.Syntax           as G
import qualified Network.HTTP.Types                      as HTTP

import           Control.Monad.Trans.Control             (MonadBaseControl)
import           Data.Kind                               (Type)
import           Data.Tagged
import           Data.Text.Extended
import           Data.Text.NonEmpty                      (mkNonEmptyTextUnsafe)

import qualified Hasura.GraphQL.Transport.HTTP.Protocol  as GH
import qualified Hasura.SQL.AnyBackend                   as AB

import           Hasura.Base.Error
import           Hasura.EncJSON
import           Hasura.GraphQL.Execute.Action.Types     (ActionExecutionPlan)
import           Hasura.GraphQL.Execute.LiveQuery.Plan
import           Hasura.GraphQL.Execute.RemoteJoin.Types
import           Hasura.GraphQL.Parser                   hiding (Type)
import           Hasura.QueryTags
import           Hasura.RQL.IR
import           Hasura.RQL.Types.Action
import           Hasura.RQL.Types.Backend
import           Hasura.RQL.Types.Column                 (ColumnType, fromCol)
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.QueryTags              (QueryTagsConfig)
import           Hasura.RQL.Types.RemoteSchema
import           Hasura.SQL.Backend
import           Hasura.Server.Version                   (HasVersion)
import           Hasura.Session

import           Hasura.Metadata.Class
import           Hasura.Tracing                          (TraceT)

import qualified Database.PG.Query                       as Q
import           Hasura.RQL.DDL.Schema.Cache             (CacheRWT)
import           Hasura.RQL.Types.Run                    (RunT (..))
import           Hasura.RQL.Types.SchemaCache.Build      (MetadataT (..))


-- | This typeclass enacapsulates how a given backend translates a root field into an execution
-- plan. For now, each root field maps to one execution step, but in the future, when we have
-- a client-side dataloader, each root field might translate into a multi-step plan.
class ( Backend b
      , ToTxt (MultiplexedQuery b)
      , Monad (ExecutionMonad b)
      ) => BackendExecute (b :: BackendType) where
  -- generated query information
  type PreparedQuery    b :: Type
  type MultiplexedQuery b :: Type
  type ExecutionMonad   b :: Type -> Type

  -- execution plan generation
  mkDBQueryPlan
    :: forall m
     . ( MonadError QErr m
       , MonadQueryTags m
       , MonadReader QueryTagsComment m
       )
    => UserInfo
    -> SourceName
    -> SourceConfig b
    -> QueryDB b (Const Void) (UnpreparedValue b)
    -> m (DBStepInfo b)
  mkDBMutationPlan
    :: forall m
     . ( MonadError QErr m
       , HasVersion
       , MonadQueryTags m
       , MonadReader QueryTagsComment m
       )
    => UserInfo
    -> Bool
    -> SourceName
    -> SourceConfig b
    -> MutationDB b (Const Void) (UnpreparedValue b)
    -> m (DBStepInfo b)
  mkDBSubscriptionPlan
    :: forall m
     . ( MonadError QErr m
       , MonadIO m
       , MonadReader QueryTagsComment m
       )
    => UserInfo
    -> SourceName
    -> SourceConfig b
    -> InsOrdHashMap G.Name (QueryDB b (Const Void) (UnpreparedValue b))
    -> m (LiveQueryPlan b (MultiplexedQuery b))
  mkDBQueryExplain
    :: forall m
     . ( MonadError QErr m
       )
    => G.Name
    -> UserInfo
    -> SourceName
    -> SourceConfig b
    -> QueryDB b (Const Void) (UnpreparedValue b)
    -> m (AB.AnyBackend DBStepInfo)
  mkLiveQueryExplain
    :: ( MonadError QErr m
      , MonadIO m
      , MonadBaseControl IO m
      )
    => LiveQueryPlan b (MultiplexedQuery b)
    -> m LiveQueryPlanExplanation

  mkDBRemoteRelationshipPlan
    :: forall m
     . ( MonadError QErr m
       , MonadQueryTags m
       )
    => UserInfo
    -> SourceName
    -> SourceConfig b
    -> NonEmpty J.Object
    -- ^ List of json objects, each of which becomes a row of the table.
    -> HashMap FieldName (Column b, ScalarType b)
    -- ^ The above objects have this schema.
    -> FieldName
    -- ^ This is a field name from the lhs that *has* to be selected in the
    -- response along with the relationship.
    -> (FieldName, SourceRelationshipSelection b (Const Void) UnpreparedValue)
    -> m (DBStepInfo b)

-- | This is a helper function to convert a remote source's relationship to a
-- normal relationship to a temporary table. This function can be used to
-- implement executeRemoteRelationship function in databases which support
-- constructing a temporary table for a list of json objects.
convertRemoteSourceRelationship
  :: forall b. (Backend b)
  => HashMap (Column b) (Column b)
  -- ^ Join columns for the relationship
  -> SelectFromG b (UnpreparedValue b)
  -- ^ The LHS of the join, this is the expression which selects from json
  -- objects
  -> Column b
  -- ^ This is the __argument__ id column, that needs to be added to the response
  -- This is used by by the remote joins processing logic to convert the
  -- response from upstream to join indices
  -> ColumnType b
  -- ^ This is the type of the __argument__ id column
  -> (FieldName, SourceRelationshipSelection b (Const Void) UnpreparedValue)
  -- ^ The relationship column and its name (how it should be selected in the
  -- response)
  -> QueryDB b (Const Void) (UnpreparedValue b)
convertRemoteSourceRelationship columnMapping selectFrom argumentIdColumn argumentIdColumnType
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
      ( fromCol @b argumentIdColumn
      , AFColumn $
        AnnColumnField { _acfColumn = argumentIdColumn
                       , _acfType = argumentIdColumnType
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

data DBStepInfo b = DBStepInfo
  { dbsiSourceName    :: SourceName
  , dbsiSourceConfig  :: SourceConfig b
  , dbsiPreparedQuery :: Maybe (PreparedQuery b)
  , dbsiAction        :: ExecutionMonad b EncJSON
  }


-- | The result of an explain query: for a given root field (denoted by its name): the generated SQL
-- query, and the detailed explanation obtained from the database (if any). We mostly use this type
-- as an intermediary step, and immediately tranform any value we obtain into an equivalent JSON
-- representation.
data ExplainPlan
  = ExplainPlan
  { _fpField :: !G.Name
  , _fpSql   :: !(Maybe Text)
  , _fpPlan  :: !(Maybe [Text])
  } deriving (Show, Eq, Generic)

instance J.ToJSON ExplainPlan where
  toJSON = J.genericToJSON $ J.aesonPrefix J.camelCase


-- | One execution step to processing a GraphQL query (e.g. one root field).
data ExecutionStep where
  -- | A query to execute against the database
  ExecStepDB
    :: HTTP.ResponseHeaders
    -> AB.AnyBackend DBStepInfo
    -> Maybe RemoteJoins
    -> ExecutionStep
  -- | Execute an action
  ExecStepAction
    :: ActionExecutionPlan
    -> ActionsInfo
    -> Maybe RemoteJoins
    -> ExecutionStep
  -- | A graphql query to execute against a remote schema
  ExecStepRemote
    :: !RemoteSchemaInfo
    -> !RemoteResultCustomizer
    -> !GH.GQLReqOutgoing
    -> ExecutionStep
  -- | Output a plain JSON object
  ExecStepRaw
    :: JO.Value
    -> ExecutionStep


-- | The series of steps that need to be executed for a given query. For now, those steps are all
-- independent. In the future, when we implement a client-side dataloader and generalized joins,
-- this will need to be changed into an annotated tree.
type ExecutionPlan = InsOrdHashMap G.Name ExecutionStep

class (Monad m) => MonadQueryTags m where
  -- | Creates Query Tags. These are appended to the Generated SQL.
  -- Helps users to use native database monitoring tools to get some 'application-context'.
  createQueryTags
    :: QueryTagsAttributes  -> Maybe QueryTagsConfig -> Tagged m QueryTagsComment

instance (MonadQueryTags m) => MonadQueryTags (ReaderT r m) where
  createQueryTags qtSourceConfig attr = retag (createQueryTags @m qtSourceConfig attr) :: Tagged (ReaderT r m) QueryTagsComment

instance (MonadQueryTags m) => MonadQueryTags (ExceptT e m) where
  createQueryTags qtSourceConfig attr = retag (createQueryTags @m qtSourceConfig attr) :: Tagged (ExceptT e m) QueryTagsComment

instance (MonadQueryTags m) => MonadQueryTags (TraceT m) where
  createQueryTags qtSourceConfig attr = retag (createQueryTags @m qtSourceConfig attr) :: Tagged (TraceT m) QueryTagsComment

instance (MonadQueryTags m) => MonadQueryTags (MetadataStorageT m) where
  createQueryTags qtSourceConfig attr = retag (createQueryTags @m qtSourceConfig attr) :: Tagged (MetadataStorageT m) QueryTagsComment

instance (MonadQueryTags m) => MonadQueryTags (Q.TxET QErr m) where
  createQueryTags qtSourceConfig attr = retag (createQueryTags @m qtSourceConfig attr) :: Tagged (Q.TxET QErr m) QueryTagsComment

instance (MonadQueryTags m) => MonadQueryTags (MetadataT m) where
  createQueryTags qtSourceConfig attr = retag (createQueryTags @m qtSourceConfig attr) :: Tagged (MetadataT m) QueryTagsComment

instance (MonadQueryTags m) => MonadQueryTags (CacheRWT m) where
  createQueryTags qtSourceConfig attr = retag (createQueryTags @m qtSourceConfig attr) :: Tagged (CacheRWT m) QueryTagsComment

instance (MonadQueryTags m) => MonadQueryTags (RunT m) where
  createQueryTags qtSourceConfig attr = retag (createQueryTags @m qtSourceConfig attr) :: Tagged (RunT m) QueryTagsComment
