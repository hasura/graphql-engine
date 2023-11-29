module Hasura.GraphQL.Execute.Action.Types
  ( ActionContext (..),
    ActionExecution (..),
    ActionExecutionPlan (..),
    ActionHandlerLog (..),
    ActionInternalError (..),
    ActionRequestInfo (..),
    ActionResponseInfo (..),
    ActionWebhookErrorResponse (..),
    ActionWebhookPayload (..),
    ActionWebhookResponse,
    AsyncActionQueryExecution (..),
    AsyncActionQueryExecutionPlan (..),
    AsyncActionQuerySourceExecution (..),
  )
where

import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson qualified as J
import Data.Aeson.Casing qualified as J
import Data.Int (Int64)
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.GraphQL.Transport.HTTP.Protocol
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.RQL.IR.Select qualified as RS
import Hasura.RQL.IR.Value
import Hasura.RQL.Types.Action
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Headers (HeaderConf)
import Hasura.Session
import Hasura.Tracing qualified as Tracing
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
    _aaqseSelectBuilder :: !(ActionLogResponse -> RS.AnnSimpleSelectG ('Postgres 'Vanilla) Void v)
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
  deriving (Show, Eq, Generic)

instance J.FromJSON ActionContext where
  parseJSON = J.genericParseJSON (J.aesonDrop 3 J.snakeCase)

instance J.ToJSON ActionContext where
  toJSON = J.genericToJSON (J.aesonDrop 3 J.snakeCase)
  toEncoding = J.genericToEncoding (J.aesonDrop 3 J.snakeCase)

-- _awpRequestQuery is Nothing is case of Asynchronous actions
data ActionWebhookPayload = ActionWebhookPayload
  { _awpAction :: !ActionContext,
    _awpSessionVariables :: !SessionVariables,
    _awpInput :: !J.Value,
    _awpRequestQuery :: !(Maybe GQLQueryText)
  }
  deriving (Show, Eq, Generic)

instance J.FromJSON ActionWebhookPayload where
  parseJSON = J.genericParseJSON (J.aesonDrop 4 J.snakeCase)

instance J.ToJSON ActionWebhookPayload where
  toJSON = J.genericToJSON (J.aesonDrop 4 J.snakeCase)
  toEncoding = J.genericToEncoding (J.aesonDrop 4 J.snakeCase)

data ActionWebhookErrorResponse = ActionWebhookErrorResponse
  { _awerMessage :: !Text,
    _awerCode :: !(Maybe Text),
    _awerExtensions :: !(Maybe J.Value)
  }
  deriving (Show, Eq, Generic)

instance J.FromJSON ActionWebhookErrorResponse where
  parseJSON = J.genericParseJSON (J.aesonDrop 5 J.snakeCase)

instance J.ToJSON ActionWebhookErrorResponse where
  toJSON = J.genericToJSON (J.aesonDrop 5 J.snakeCase)
  toEncoding = J.genericToEncoding (J.aesonDrop 5 J.snakeCase)

type ActionWebhookResponse = J.Value

data ActionRequestInfo = ActionRequestInfo
  { _areqiUrl :: !Text,
    _areqiBody :: !J.Value,
    _areqiHeaders :: ![HeaderConf],
    _areqiTransformedRequest :: !(Maybe HTTP.Request)
  }
  deriving (Show, Generic)

instance J.ToJSON ActionRequestInfo where
  toJSON = J.genericToJSON (J.aesonDrop 6 J.snakeCase)
  toEncoding = J.genericToEncoding (J.aesonDrop 6 J.snakeCase)

data ActionResponseInfo = ActionResponseInfo
  { _aresiStatus :: !Int,
    _aresiBody :: !J.Value,
    _aresiHeaders :: ![HeaderConf]
  }
  deriving (Show, Eq, Generic)

instance J.FromJSON ActionResponseInfo where
  parseJSON = J.genericParseJSON (J.aesonDrop 6 J.snakeCase)

instance J.ToJSON ActionResponseInfo where
  toJSON = J.genericToJSON (J.aesonDrop 6 J.snakeCase)
  toEncoding = J.genericToEncoding (J.aesonDrop 6 J.snakeCase)

data ActionInternalError = ActionInternalError
  { _aieError :: !J.Value,
    _aieRequest :: !ActionRequestInfo,
    _aieResponse :: !(Maybe ActionResponseInfo)
  }
  deriving (Show, Generic)

instance J.ToJSON ActionInternalError where
  toJSON = J.genericToJSON (J.aesonDrop 4 J.snakeCase)
  toEncoding = J.genericToEncoding (J.aesonDrop 4 J.snakeCase)

-- * Action handler logging related

data ActionHandlerLog = ActionHandlerLog
  { _ahlRequest :: !HTTP.Request,
    _ahlRequestTrans :: !(Maybe HTTP.Request),
    _ahlRequestSize :: !Int64,
    _ahlTransformedRequestSize :: !(Maybe Int64),
    _ahlResponseSize :: !Int64,
    _ahlActionName :: !ActionName
  }
  deriving (Show, Generic)

instance J.ToJSON ActionHandlerLog where
  toJSON = J.genericToJSON (J.aesonDrop 4 J.snakeCase) {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding (J.aesonDrop 4 J.snakeCase) {J.omitNothingFields = True}

instance L.ToEngineLog ActionHandlerLog L.Hasura where
  toEngineLog ahl = (L.LevelInfo, L.ELTActionHandler, J.toJSON ahl)
