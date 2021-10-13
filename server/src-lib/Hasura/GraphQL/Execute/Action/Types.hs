module Hasura.GraphQL.Execute.Action.Types where

import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson qualified as J
import Data.Aeson.Casing qualified as J
import Data.Aeson.TH qualified as J
import Data.HashMap.Strict qualified as Map
import Data.Int (Int64)
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.GraphQL.Parser
import Hasura.GraphQL.Transport.HTTP.Protocol
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.RQL.DDL.Headers
import Hasura.RQL.IR.Select qualified as RS
import Hasura.RQL.Types
import Hasura.Session
import Hasura.Tracing qualified as Tracing
import Language.GraphQL.Draft.Syntax qualified as G
import Network.HTTP.Client.Transformable qualified as HTTP

newtype ActionExecution = ActionExecution
  { unActionExecution ::
      forall m.
      (MonadIO m, MonadBaseControl IO m, MonadError QErr m, Tracing.MonadTrace m) =>
      m (EncJSON, HTTP.ResponseHeaders)
  }

data AsyncActionQuerySourceExecution v = AsyncActionQuerySourceExecution
  { _aaqseSource :: !SourceName,
    _aaqseJsonAggSelect :: !JsonAggSelect,
    _aaqseSelectBuilder :: !(ActionLogResponse -> RS.AnnSimpleSelectG ('Postgres 'Vanilla) (Const Void) v)
  }

data AsyncActionQueryExecution v
  = -- | Async actions associated with no relationships.
    AAQENoRelationships !(ActionLogResponse -> Either QErr EncJSON)
  | -- | Async actions with relationships defined to Postgres
    -- (as of now, we may have support for other backends as well in further iterations) tables.
    AAQEOnSourceDB !(SourceConfig ('Postgres 'Vanilla)) !(AsyncActionQuerySourceExecution v)

-- | A plan to execute async action query
data AsyncActionQueryExecutionPlan = AsyncActionQueryExecutionPlan
  { _aaqepId :: !ActionId,
    _aaqepExecution :: !(AsyncActionQueryExecution (UnpreparedValue ('Postgres 'Vanilla)))
  }

-- A plan to execute any action
data ActionExecutionPlan
  = AEPSync !ActionExecution
  | AEPAsyncQuery !AsyncActionQueryExecutionPlan
  | AEPAsyncMutation !ActionId

newtype ActionContext = ActionContext {_acName :: ActionName}
  deriving (Show, Eq)

$(J.deriveJSON (J.aesonDrop 3 J.snakeCase) ''ActionContext)

-- _awpRequestQuery is Nothing is case of Asynchronous actions
data ActionWebhookPayload = ActionWebhookPayload
  { _awpAction :: !ActionContext,
    _awpSessionVariables :: !SessionVariables,
    _awpInput :: !J.Value,
    _awpRequestQuery :: !(Maybe GQLQueryText)
  }
  deriving (Show, Eq)

$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''ActionWebhookPayload)

data ActionWebhookErrorResponse = ActionWebhookErrorResponse
  { _awerMessage :: !Text,
    _awerCode :: !(Maybe Text),
    _awerExtensions :: !(Maybe J.Value)
  }
  deriving (Show, Eq)

$(J.deriveJSON (J.aesonDrop 5 J.snakeCase) ''ActionWebhookErrorResponse)

data ActionWebhookResponse
  = AWRArray ![Map.HashMap G.Name J.Value]
  | AWRObject !(Map.HashMap G.Name J.Value)
  | AWRNull
  deriving (Show, Eq)

instance J.FromJSON ActionWebhookResponse where
  parseJSON v = case v of
    J.Array {} -> AWRArray <$> J.parseJSON v
    J.Object {} -> AWRObject <$> J.parseJSON v
    J.Null {} -> pure AWRNull
    _ -> fail "expecting null, object or array of objects for action webhook response"

instance J.ToJSON ActionWebhookResponse where
  toJSON (AWRArray objects) = J.toJSON objects
  toJSON (AWRObject obj) = J.toJSON obj
  toJSON (AWRNull) = J.Null

data ActionRequestInfo = ActionRequestInfo
  { _areqiUrl :: !Text,
    _areqiBody :: !J.Value,
    _areqiHeaders :: ![HeaderConf],
    _areqiTransformedRequest :: !(Maybe HTTP.Request)
  }
  deriving (Show)

$(J.deriveToJSON (J.aesonDrop 6 J.snakeCase) ''ActionRequestInfo)

data ActionResponseInfo = ActionResponseInfo
  { _aresiStatus :: !Int,
    _aresiBody :: !J.Value,
    _aresiHeaders :: ![HeaderConf]
  }
  deriving (Show, Eq)

$(J.deriveToJSON (J.aesonDrop 6 J.snakeCase) ''ActionResponseInfo)

data ActionInternalError = ActionInternalError
  { _aieError :: !J.Value,
    _aieRequest :: !ActionRequestInfo,
    _aieResponse :: !(Maybe ActionResponseInfo)
  }
  deriving (Show)

$(J.deriveToJSON (J.aesonDrop 4 J.snakeCase) ''ActionInternalError)

-- * Action handler logging related

data ActionHandlerLog = ActionHandlerLog
  { _ahlRequest :: !HTTP.Request,
    _ahlRequestTrans :: !(Maybe HTTP.Request),
    _ahlRequestSize :: !Int64,
    _ahlTransformedRequestSize :: !(Maybe Int64),
    _ahlResponseSize :: !Int64,
    _ahlActionName :: !ActionName
  }
  deriving (Show)

$(J.deriveToJSON (J.aesonDrop 4 J.snakeCase) {J.omitNothingFields = True} ''ActionHandlerLog)

instance L.ToEngineLog ActionHandlerLog L.Hasura where
  toEngineLog ahl = (L.LevelInfo, L.ELTActionHandler, J.toJSON ahl)
