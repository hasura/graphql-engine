module Hasura.GraphQL.Execute.Backend where

import           Hasura.Prelude

import qualified Data.Aeson                              as J
import qualified Data.Aeson.Casing                       as J
import qualified Data.Aeson.Ordered                      as JO
import qualified Data.ByteString                         as B
import qualified Language.GraphQL.Draft.Syntax           as G

import           Control.Monad.Trans.Control             (MonadBaseControl)
import           Data.Kind                               (Type)
import           Data.Text.Extended

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
    -> QueryDB b (UnpreparedValue b)
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
    -> MutationDB b (UnpreparedValue b)
    -> m EncJSON
  makeLiveQueryPlan
    :: forall m
     . ( MonadError QErr m
       , MonadIO m
       )
    => UserInfo
    -> SourceName
    -> SourceConfig b
    -> InsOrdHashMap G.Name (QueryDB b (UnpreparedValue b))
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
    -> QueryDB b (UnpreparedValue b)
    -> m EncJSON
  explainLiveQuery
    :: ( MonadError QErr m
      , MonadIO m
      , MonadBaseControl IO m
      )
    => LiveQueryPlan b (MultiplexedQuery b)
    -> m LiveQueryPlanExplanation

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
      (QueryDBRootField UnpreparedValue, Maybe RemoteJoins)
      (RemoteSchemaInfo, GH.GQLReqOutgoing)
      (QueryActionRootField UnpreparedValue, Maybe RemoteJoins)
      JO.Value

type MutationFieldExecution
  = RootField
      (MutationDBRootField UnpreparedValue, Maybe RemoteJoins, SQLGenCtx)
      (RemoteSchemaInfo, GH.GQLReqOutgoing)
      (MutationActionRootField UnpreparedValue, Maybe RemoteJoins)
      JO.Value

type ExecutionPlan root = InsOrdHashMap G.Name root
type QueryExecutionPlan = ExecutionPlan QueryFieldExecution
type MutationExecutionPlan = ExecutionPlan MutationFieldExecution
