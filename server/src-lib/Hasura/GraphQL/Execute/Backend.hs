module Hasura.GraphQL.Execute.Backend where

import           Hasura.Prelude

import qualified Data.Aeson                              as J
import qualified Data.Aeson.Casing                       as J
import qualified Data.Aeson.Ordered                      as JO
import qualified Language.GraphQL.Draft.Syntax           as G
import qualified Network.HTTP.Types                      as HTTP

import           Control.Monad.Trans.Control             (MonadBaseControl)
import           Data.Kind                               (Type)
import           Data.SqlCommenter
import           Data.Tagged
import           Data.Text.Extended

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
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.QueryTags              (QueryTagsSourceConfig)
import           Hasura.RQL.Types.RemoteSchema
import           Hasura.SQL.Backend
import           Hasura.Server.Version                   (HasVersion)
import           Hasura.Session

import           Hasura.Metadata.Class
import           Hasura.Tracing                          (TraceT)

import           Hasura.Backends.Postgres.Connection     (LazyTxT)
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
       )
    => UserInfo
    -> SourceName
    -> SourceConfig b
    -> QueryDB b (Const Void) (UnpreparedValue b)
    -> QueryTagsComment
    -> m (DBStepInfo b)
  mkDBMutationPlan
    :: forall m
     . ( MonadError QErr m
       , HasVersion
       , MonadQueryTags m
       )
    => UserInfo
    -> Bool
    -> SourceName
    -> SourceConfig b
    -> MutationDB b (Const Void) (UnpreparedValue b)
    -> QueryTagsComment
    -> m (DBStepInfo b)
  mkDBSubscriptionPlan
    :: forall m
     . ( MonadError QErr m
       , MonadIO m
       )
    => UserInfo
    -> SourceName
    -> SourceConfig b
    -> InsOrdHashMap G.Name (QueryDB b (Const Void) (UnpreparedValue b))
    -> QueryTagsComment
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
    :: (Maybe QueryTagsSourceConfig) -> [Attribute] -> Tagged m Text

instance (MonadQueryTags m) => MonadQueryTags (ReaderT r m) where
  createQueryTags qtSourceConfig attr = retag (createQueryTags @m qtSourceConfig attr) :: Tagged (ReaderT r m) Text

instance (MonadQueryTags m) => MonadQueryTags (ExceptT e m) where
  createQueryTags qtSourceConfig attr = retag (createQueryTags @m qtSourceConfig attr) :: Tagged (ExceptT e m) Text

instance (MonadQueryTags m) => MonadQueryTags (TraceT m) where
  createQueryTags qtSourceConfig attr = retag (createQueryTags @m qtSourceConfig attr) :: Tagged (TraceT m) Text

instance (MonadQueryTags m) => MonadQueryTags (MetadataStorageT m) where
  createQueryTags qtSourceConfig attr = retag (createQueryTags @m qtSourceConfig attr) :: Tagged (MetadataStorageT m) Text

instance (MonadQueryTags m) => MonadQueryTags (LazyTxT QErr m) where
  createQueryTags qtSourceConfig attr = retag (createQueryTags @m qtSourceConfig attr) :: Tagged (LazyTxT QErr m) Text

instance (MonadQueryTags m) => MonadQueryTags (MetadataT m) where
  createQueryTags qtSourceConfig attr = retag (createQueryTags @m qtSourceConfig attr) :: Tagged (MetadataT m) Text

instance (MonadQueryTags m) => MonadQueryTags (CacheRWT m) where
  createQueryTags qtSourceConfig attr = retag (createQueryTags @m qtSourceConfig attr) :: Tagged (CacheRWT m) Text

instance (MonadQueryTags m) => MonadQueryTags (RunT m) where
  createQueryTags qtSourceConfig attr = retag (createQueryTags @m qtSourceConfig attr) :: Tagged (RunT m) Text
