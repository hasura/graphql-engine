module Hasura.Backends.Postgres.Execute.ConnectionTemplate
  ( PrimaryTag (..),
    DefaultTag (..),
    ReadReplicasTag (..),
    ConnectionSetMemberTemplateContext (..),
    RequestContextHeaders (..),
    ConnectionSetTemplateContext,
    QueryContext (..),
    RequestContext (..),
    PostgresConnectionTemplateContext (..),
    PostgresResolvedConnectionTemplate (..),
    QueryOperationType (..),
    mkConnectionSetMemberTemplateContext,
    makeConnectionTemplateContext,
    makeRequestContext,
    runKritiEval,
  )
where

import Data.Aeson qualified as J
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.CaseInsensitive qualified as CI
import Data.HashMap.Strict qualified as HashMap
import Data.Text.Extended
import Hasura.Backends.Postgres.Connection.Settings
import Hasura.Prelude
import Hasura.Session (SessionVariables)
import Kriti.Eval qualified as Kriti
import Language.GraphQL.Draft.Syntax qualified as G
import Network.HTTP.Types qualified as HTTP

-- | This connection tag represents primary database connection
data PrimaryTag = PrimaryTag deriving (Eq, Show, Generic)

instance Hashable PrimaryTag

instance NFData PrimaryTag

primaryTagValue :: J.Value
primaryTagValue = J.String "PRIMARY"

instance J.ToJSON PrimaryTag where
  toJSON PrimaryTag = primaryTagValue

instance J.FromJSON PrimaryTag where
  parseJSON v
    | v == primaryTagValue = pure PrimaryTag
    | otherwise = fail $ "expected value " <> show primaryTagValue

-- | This connection tag represents default behaviour of database connections.
--
--   For example, if read replica is set, then it will redirect GQL queries to
--   read replicas and mutations to the primary connection
data DefaultTag = DefaultTag deriving (Eq, Show, Generic)

instance Hashable DefaultTag

instance NFData DefaultTag

defaultTagValue :: J.Value
defaultTagValue = J.String "DEFAULT"

instance J.ToJSON DefaultTag where
  toJSON DefaultTag = defaultTagValue

instance J.FromJSON DefaultTag where
  parseJSON v
    | v == defaultTagValue = pure DefaultTag
    | otherwise = fail $ "expected value " <> show defaultTagValue

-- | This connection tag represents read replica database connection
data ReadReplicasTag = ReadReplicasTag deriving (Eq, Show, Generic)

instance Hashable ReadReplicasTag

instance NFData ReadReplicasTag

readReplicasTagValue :: J.Value
readReplicasTagValue = J.String "READ_REPLICAS"

instance J.ToJSON ReadReplicasTag where
  toJSON ReadReplicasTag = readReplicasTagValue

instance J.FromJSON ReadReplicasTag where
  parseJSON v
    | v == readReplicasTagValue = pure ReadReplicasTag
    | otherwise = fail $ "expected value " <> show readReplicasTagValue

-- | The connection_set template context type. Always encodes to "connection_set"
-- string value in template context.
data ConnectionSetTemplateContextType = ConnectionSetTemplateContextType
  deriving (Eq, Show, Generic)

instance Hashable ConnectionSetTemplateContextType

instance NFData ConnectionSetTemplateContextType

instance J.ToJSON ConnectionSetTemplateContextType where
  toJSON ConnectionSetTemplateContextType = J.String "connection_set"

instance J.FromJSON ConnectionSetTemplateContextType where
  parseJSON = J.withText "ConnectionSetTemplateContextType" \case
    "connection_set" -> pure ConnectionSetTemplateContextType
    t -> fail $ "unexpected type for connection set member " <> show t

-- | Data type for single member in connection_set for connection template context
data ConnectionSetMemberTemplateContext = ConnectionSetMemberTemplateContext
  { _cseType :: ConnectionSetTemplateContextType,
    _cseName :: PostgresConnectionSetMemberName
  }
  deriving (Eq, Show, Generic)

instance J.FromJSON ConnectionSetMemberTemplateContext where
  parseJSON = J.genericParseJSON hasuraJSON

instance J.ToJSON ConnectionSetMemberTemplateContext where
  toJSON = J.genericToJSON hasuraJSON
  toEncoding = J.genericToEncoding hasuraJSON

instance Hashable ConnectionSetMemberTemplateContext

instance NFData ConnectionSetMemberTemplateContext

mkConnectionSetMemberTemplateContext :: PostgresConnectionSetMemberName -> ConnectionSetMemberTemplateContext
mkConnectionSetMemberTemplateContext memberName =
  ConnectionSetMemberTemplateContext
    { _cseType = ConnectionSetTemplateContextType,
      _cseName = memberName
    }

-- | Outcome of the connection template resolution
data PostgresResolvedConnectionTemplate
  = PCTODefault DefaultTag
  | PCTOPrimary PrimaryTag
  | PCTOReadReplicas ReadReplicasTag
  | PCTOConnectionSet PostgresConnectionSetMemberName
  deriving (Eq, Show, Generic)

instance Hashable PostgresResolvedConnectionTemplate

instance NFData PostgresResolvedConnectionTemplate

instance J.FromJSON PostgresResolvedConnectionTemplate where
  parseJSON v =
    (PCTOPrimary <$> J.parseJSON v)
      <|> (PCTODefault <$> J.parseJSON v)
      <|> (PCTOReadReplicas <$> J.parseJSON v)
      <|> (PCTOConnectionSet . _cseName <$> J.parseJSON v)

instance J.ToJSON PostgresResolvedConnectionTemplate where
  toJSON resolvedConnTemplate =
    let (routingTo, value) = case resolvedConnTemplate of
          (PCTOPrimary _) -> ("primary", J.Null)
          (PCTODefault _) -> ("default", J.Null)
          (PCTOReadReplicas _) -> ("read_replicas", J.Null)
          (PCTOConnectionSet v) -> ("connection_set", J.toJSON v)
     in J.object ["routing_to" J..= (routingTo :: Text), "value" J..= value]

-- | Headers information for the connection template context
data RequestContextHeaders = RequestContextHeaders (HashMap Text Text)
  deriving (Show, Generic)

instance J.FromJSON RequestContextHeaders where
  parseJSON = J.genericParseJSON hasuraJSON

instance J.ToJSON RequestContextHeaders where
  toJSON = J.genericToJSON hasuraJSON
  toEncoding = J.genericToEncoding hasuraJSON

-- | Data type for connection_set for connection template context
newtype ConnectionSetTemplateContext = ConnectionSetTemplateContext {_getConnectionSet :: HashMap PostgresConnectionSetMemberName ConnectionSetMemberTemplateContext}
  deriving (Show, Semigroup, Monoid)

instance J.FromJSON ConnectionSetTemplateContext where
  parseJSON = J.withObject "ConnectionSetTemplateContext" \o -> do
    connections <-
      traverse
        ( \(k, v) -> do
            connSetMember <- J.parseJSON v
            connSetMemberName <- PostgresConnectionSetMemberName <$> J.parseJSON (J.toJSON k)
            pure (connSetMemberName, connSetMember)
        )
        (KM.toList o)
    pure $ ConnectionSetTemplateContext (HashMap.fromList connections)

instance J.ToJSON ConnectionSetTemplateContext where
  toJSON (ConnectionSetTemplateContext connections) =
    J.Object $ KM.fromHashMap $ HashMap.map (J.toJSON) (HashMap.mapKeys (K.fromText . toTxt) connections)

newtype QueryOperationType = QueryOperationType G.OperationType
  deriving (Show)

instance J.FromJSON QueryOperationType where
  parseJSON (J.String "query") = pure (QueryOperationType G.OperationTypeQuery)
  parseJSON (J.String "mutation") = pure (QueryOperationType G.OperationTypeMutation)
  parseJSON (J.String "subscription") = pure (QueryOperationType G.OperationTypeSubscription)
  parseJSON _ = fail "operation type can only be one of the following: query, mutation or subscription"

instance J.ToJSON QueryOperationType where
  toJSON (QueryOperationType operationType) =
    J.String $ case operationType of
      G.OperationTypeQuery -> "query"
      G.OperationTypeSubscription -> "subscription"
      G.OperationTypeMutation -> "mutation"

-- | Query information (operation name and operation type) for connection
--   template context
data QueryContext = QueryContext
  { _qcOperationName :: Maybe G.Name,
    _qcOperationType :: QueryOperationType
  }
  deriving (Show, Generic)

instance J.FromJSON QueryContext where
  parseJSON = J.genericParseJSON hasuraJSON {J.omitNothingFields = True}

instance J.ToJSON QueryContext where
  toJSON = J.genericToJSON hasuraJSON {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding hasuraJSON {J.omitNothingFields = True}

-- | Request information for connection template context
data RequestContext = RequestContext
  { _rcHeaders :: RequestContextHeaders,
    _rcSession :: SessionVariables,
    _rcQuery :: Maybe QueryContext
  }
  deriving (Show, Generic)

instance J.FromJSON RequestContext where
  parseJSON = J.withObject "RequestContext" $ \o -> do
    headers <- o J..:? "headers" J..!= (RequestContextHeaders mempty)
    sessionVars <- o J..:? "session" J..!= mempty
    queryContext <- o J..:? "query"
    pure (RequestContext headers sessionVars queryContext)

instance J.ToJSON RequestContext where
  toJSON = J.genericToJSON hasuraJSON {J.omitNothingFields = True}

-- | The complete connection template context used for resolving connection
--   template
data PostgresConnectionTemplateContext = PostgresConnectionTemplateContext
  { _pctcRequest :: RequestContext,
    _pctcPrimary :: PrimaryTag,
    _pctcReadReplicas :: ReadReplicasTag,
    _pctcDefault :: DefaultTag,
    _pctcConnectionSet :: ConnectionSetTemplateContext
  }
  deriving (Show, Generic)

instance J.FromJSON PostgresConnectionTemplateContext where
  parseJSON = J.genericParseJSON hasuraJSON

instance J.ToJSON PostgresConnectionTemplateContext where
  toJSON = J.genericToJSON hasuraJSON
  toEncoding = J.genericToEncoding hasuraJSON

-- | Construct template context
makeConnectionTemplateContext :: RequestContext -> [PostgresConnectionSetMemberName] -> PostgresConnectionTemplateContext
makeConnectionTemplateContext reqCtx connectionSetMembers =
  PostgresConnectionTemplateContext
    reqCtx
    PrimaryTag
    ReadReplicasTag
    DefaultTag
    connectionSet
  where
    connectionSet =
      ConnectionSetTemplateContext $ HashMap.fromList $ map (id &&& mkConnectionSetMemberTemplateContext) connectionSetMembers

-- | We should move this to Data.Aeson.Kriti.Functions
runKritiEval :: PostgresConnectionTemplateContext -> KritiTemplate -> Either Kriti.EvalError J.Value
runKritiEval ktcContext (KritiTemplate rawTemplate templateAST) = Kriti.runEval templateBS templateAST templateCtx
  where
    templateBS = txtToBs rawTemplate
    templateCtx = [("$", J.toJSON ktcContext)]

makeRequestContext :: Maybe QueryContext -> [HTTP.Header] -> SessionVariables -> RequestContext
makeRequestContext queryContext reqHeaders sessionVars =
  let reqHeaderHashMap = HashMap.fromList $ map (\(hdrName, hdrVal) -> (bsToTxt (CI.original hdrName), bsToTxt hdrVal)) reqHeaders
   in RequestContext (RequestContextHeaders reqHeaderHashMap) sessionVars queryContext
