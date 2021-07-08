module Hasura.GraphQL.Execute.Action.Types where

import           Hasura.Prelude

import qualified Data.Aeson                    as J
import qualified Data.Aeson.Casing             as J
import qualified Data.Aeson.TH                 as J
import qualified Data.HashMap.Strict           as Map
import qualified Language.GraphQL.Draft.Syntax as G
import qualified Network.HTTP.Types            as HTTP

import           Control.Monad.Trans.Control   (MonadBaseControl)
import           Data.Int                      (Int64)

import qualified Hasura.Logging                as L
import qualified Hasura.RQL.IR.Select          as RS
import qualified Hasura.Tracing                as Tracing

import           Hasura.Base.Error
import           Hasura.EncJSON
import           Hasura.GraphQL.Parser
import           Hasura.RQL.DDL.Headers
import           Hasura.RQL.Types
import           Hasura.Session


newtype ActionExecution =
  ActionExecution {
    unActionExecution
      :: forall m
       . (MonadIO m, MonadBaseControl IO m, MonadError QErr m, Tracing.MonadTrace m) => m (EncJSON, HTTP.ResponseHeaders)
  }

data AsyncActionQuerySourceExecution v
  = AsyncActionQuerySourceExecution
  { _aaqseSource        :: !SourceName
  , _aaqseJsonAggSelect :: !JsonAggSelect
  , _aaqseSelectBuilder :: !(ActionLogResponse -> RS.AnnSimpleSelectG ('Postgres 'Vanilla) (Const Void) v)
  }

data AsyncActionQueryExecution v
  = AAQENoRelationships !(ActionLogResponse -> Either QErr EncJSON)
  -- ^ Async actions associated with no relationships.
  | AAQEOnSourceDB !(SourceConfig ('Postgres 'Vanilla)) !(AsyncActionQuerySourceExecution v)
  -- ^ Async actions with relationships defined to Postgres
  -- (as of now, we may have support for other backends as well in further iterations) tables.

-- | A plan to execute async action query
data AsyncActionQueryExecutionPlan
  = AsyncActionQueryExecutionPlan
  { _aaqepId        :: !ActionId
  , _aaqepExecution :: !(AsyncActionQueryExecution (UnpreparedValue ('Postgres 'Vanilla)))
  }

-- A plan to execute any action
data ActionExecutionPlan
  = AEPSync !ActionExecution
  | AEPAsyncQuery !AsyncActionQueryExecutionPlan
  | AEPAsyncMutation !ActionId

newtype ActionContext
  = ActionContext {_acName :: ActionName}
  deriving (Show, Eq)
$(J.deriveJSON (J.aesonDrop 3 J.snakeCase) ''ActionContext)

data ActionWebhookPayload
  = ActionWebhookPayload
  { _awpAction           :: !ActionContext
  , _awpSessionVariables :: !SessionVariables
  , _awpInput            :: !J.Value
  } deriving (Show, Eq)
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''ActionWebhookPayload)

data ActionWebhookErrorResponse
  = ActionWebhookErrorResponse
  { _awerMessage :: !Text
  , _awerCode    :: !(Maybe Text)
  } deriving (Show, Eq)
$(J.deriveJSON (J.aesonDrop 5 J.snakeCase) ''ActionWebhookErrorResponse)

data ActionWebhookResponse
  = AWRArray ![Map.HashMap G.Name J.Value]
  | AWRObject !(Map.HashMap G.Name J.Value)
  deriving (Show, Eq)

instance J.FromJSON ActionWebhookResponse where
  parseJSON v = case v of
    J.Array{}  -> AWRArray <$> J.parseJSON v
    J.Object{} -> AWRObject <$> J.parseJSON v
    _          -> fail "expecting object or array of objects for action webhook response"

instance J.ToJSON ActionWebhookResponse where
  toJSON (AWRArray objects) = J.toJSON objects
  toJSON (AWRObject obj)    = J.toJSON obj

data ActionRequestInfo
  = ActionRequestInfo
  { _areqiUrl     :: !Text
  , _areqiBody    :: !J.Value
  , _areqiHeaders :: ![HeaderConf]
  } deriving (Show, Eq)
$(J.deriveToJSON (J.aesonDrop 6 J.snakeCase) ''ActionRequestInfo)

data ActionResponseInfo
  = ActionResponseInfo
  { _aresiStatus  :: !Int
  , _aresiBody    :: !J.Value
  , _aresiHeaders :: ![HeaderConf]
  } deriving (Show, Eq)
$(J.deriveToJSON (J.aesonDrop 6 J.snakeCase) ''ActionResponseInfo)

data ActionInternalError
  = ActionInternalError
  { _aieError    :: !J.Value
  , _aieRequest  :: !ActionRequestInfo
  , _aieResponse :: !(Maybe ActionResponseInfo)
  } deriving (Show, Eq)
$(J.deriveToJSON (J.aesonDrop 4 J.snakeCase) ''ActionInternalError)

-- * Action handler logging related
data ActionHandlerLog
  = ActionHandlerLog
  { _ahlRequestSize  :: !Int64
  , _ahlResponseSize :: !Int64
  , _ahlActionName   :: !ActionName
  } deriving (Show)
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase){J.omitNothingFields=True} ''ActionHandlerLog)

instance L.ToEngineLog ActionHandlerLog L.Hasura where
  toEngineLog ahl = (L.LevelInfo, L.ELTActionHandler, J.toJSON ahl)
