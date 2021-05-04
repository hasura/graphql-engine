module Hasura.GraphQL.Execute.Backend where

import           Hasura.Prelude

import qualified Data.Aeson                             as J
import qualified Data.Aeson.Casing                      as J
import qualified Data.Environment                       as Env
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Types                     as HTTP

import           Control.Monad.Trans.Control            (MonadBaseControl)
import           Data.Kind                              (Type)
import           Data.Text.Extended

import qualified Hasura.GraphQL.Transport.HTTP.Protocol as GH
import qualified Hasura.RQL.IR.RemoteJoin               as IR
import qualified Hasura.SQL.AnyBackend                  as AB

import           Hasura.EncJSON
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Execute.Action.Types    (ActionExecutionPlan)
import           Hasura.GraphQL.Execute.LiveQuery.Plan
import           Hasura.GraphQL.Parser                  hiding (Type)
import           Hasura.RQL.IR.RemoteJoin
import           Hasura.RQL.Types.Action
import           Hasura.RQL.Types.Backend
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.Error
import           Hasura.RQL.Types.RemoteSchema
import           Hasura.SQL.Backend
import           Hasura.Server.Version                  (HasVersion)
import           Hasura.Session


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
  getRemoteJoins :: PreparedQuery b -> [RemoteJoin b]

  -- execution plan generation
  mkDBQueryPlan
    :: forall m
     . ( MonadError QErr m
       , HasVersion
       )
    => Env.Environment
    -> HTTP.Manager
    -> [HTTP.Header]
    -> UserInfo
    -> [G.Directive G.Name]
    -> SourceName
    -> SourceConfig b
    -> QueryDB b (UnpreparedValue b)
    -> m ExecutionStep
  mkDBMutationPlan
    :: forall m
     . ( MonadError QErr m
       , HasVersion
       )
    => Env.Environment
    -> HTTP.Manager
    -> [HTTP.Header]
    -> UserInfo
    -> Bool
    -> SourceName
    -> SourceConfig b
    -> MutationDB b (UnpreparedValue b)
    -> m ExecutionStep
  mkDBSubscriptionPlan
    :: forall m
     . ( MonadError QErr m
       , MonadIO m
       )
    => UserInfo
    -> SourceName
    -> SourceConfig b
    -> InsOrdHashMap G.Name (QueryDB b (UnpreparedValue b))
    -> m (LiveQueryPlan b (MultiplexedQuery b))
  mkDBQueryExplain
    :: forall m
     . ( MonadError QErr m
       )
    => G.Name
    -> UserInfo
    -> SourceName
    -> SourceConfig b
    -> QueryDB b (UnpreparedValue b)
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
  ExecStepDB
    :: HTTP.ResponseHeaders
    -> AB.AnyBackend DBStepInfo
    -> ExecutionStep
  -- ^ A query to execute against the database
  ExecStepAction
    :: (ActionExecutionPlan, ActionsInfo)
    -> ExecutionStep
  -- ^ Execute an action
  ExecStepRemote
    :: !RemoteSchemaInfo
    -> !GH.GQLReqOutgoing
    -> ExecutionStep
  -- ^ A graphql query to execute against a remote schema
  ExecStepRaw
    :: J.Value
    -> ExecutionStep
  -- ^ Output a plain JSON object


-- | The series of steps that need to be executed for a given query. For now, those steps are all
-- independent. In the future, when we implement a client-side dataloader and generalized joins,
-- this will need to be changed into an annotated tree.
type ExecutionPlan = InsOrdHashMap G.Name ExecutionStep

getRemoteSchemaInfo
    :: forall b
     . BackendExecute b
    => DBStepInfo b
    -> [RemoteSchemaInfo]
getRemoteSchemaInfo (DBStepInfo _ _ genSql _) =
    IR._rjRemoteSchema <$> maybe [] (getRemoteJoins @b) genSql
