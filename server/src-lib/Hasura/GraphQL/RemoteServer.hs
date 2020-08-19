module Hasura.GraphQL.RemoteServer
  ( fetchRemoteSchema
  , IntrospectionResult
  , execRemoteGQ'
  ) where

import           Control.Exception                      (try)
import           Control.Lens                           ((^.))
import           Control.Monad.Unique
import           Data.Aeson                             ((.:), (.:?))
import           Hasura.HTTP
import           Hasura.Prelude

import qualified Data.Aeson                             as J
import qualified Data.ByteString.Lazy                   as BL
import qualified Data.Environment                       as Env
import qualified Data.HashMap.Strict                    as Map
import qualified Data.Text                              as T
import qualified Hasura.Tracing                         as Tracing
import qualified Language.GraphQL.Draft.Parser          as G
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Language.Haskell.TH.Syntax             as TH
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Types                     as N
import qualified Network.Wreq                           as Wreq


import qualified Hasura.GraphQL.Parser.Monad            as P
import           Hasura.GraphQL.Schema.Remote
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.RQL.DDL.Headers                 (makeHeadersFromConf)
import           Hasura.RQL.Types
import           Hasura.Server.Utils
import           Hasura.Server.Version                  (HasVersion)
import           Hasura.Session

introspectionQuery :: GQLReqParsed
introspectionQuery =
  $(do
       let fp = "src-rsr/introspection.json"
       TH.qAddDependentFile fp
       eitherResult <- TH.runIO $ J.eitherDecodeFileStrict fp
       case eitherResult of
         Left e                  -> fail e
         Right (r::GQLReqParsed) -> TH.lift r
   )

fetchRemoteSchema
  :: forall m
   . (HasVersion, MonadIO m, MonadUnique m, MonadError QErr m)
  => Env.Environment
  -> HTTP.Manager
  -> RemoteSchemaName
  -> RemoteSchemaInfo
  -> m RemoteSchemaCtx
fetchRemoteSchema env manager schemaName schemaInfo@(RemoteSchemaInfo url headerConf _ timeout) = do
  headers <- makeHeadersFromConf env headerConf
  let hdrsWithDefaults = addDefaultHeaders headers

  initReqE <- liftIO $ try $ HTTP.parseRequest (show url)
  initReq <- either throwHttpErr pure initReqE
  let req = initReq
           { HTTP.method = "POST"
           , HTTP.requestHeaders = hdrsWithDefaults
           , HTTP.requestBody = HTTP.RequestBodyLBS $ J.encode introspectionQuery
           , HTTP.responseTimeout = HTTP.responseTimeoutMicro (timeout * 1000000)
           }
  res  <- liftIO $ try $ HTTP.httpLbs req manager
  resp <- either throwHttpErr return res

  let respData = resp ^. Wreq.responseBody
      statusCode = resp ^. Wreq.responseStatus . Wreq.statusCode
  when (statusCode /= 200) $ throwNon200 statusCode respData

  -- Parse the JSON into flat GraphQL type AST
  (FromIntrospection introspectRes) :: (FromIntrospection IntrospectionResult) <-
    either (remoteSchemaErr . T.pack) return $ J.eitherDecode respData

  -- Check that the parsed GraphQL type info is valid by running the schema generation
  (queryParsers, mutationParsers, subscriptionParsers) <-
    P.runSchemaT @m @(P.ParseT Identity) $ buildRemoteParser introspectRes schemaInfo

  -- The 'rawIntrospectionResult' contains the 'Bytestring' response of
  -- the introspection result of the remote server. We store this in the
  -- 'RemoteSchemaCtx' because we can use this when the 'introspect_remote_schema'
  -- is called by simple encoding the result to JSON.
  return $ RemoteSchemaCtx schemaName introspectRes schemaInfo respData $
    ParsedIntrospection queryParsers mutationParsers subscriptionParsers
  where
    remoteSchemaErr :: T.Text -> m a
    remoteSchemaErr = throw400 RemoteSchemaError

    throwHttpErr :: HTTP.HttpException -> m a
    throwHttpErr = throwWithInternal httpExceptMsg . httpExceptToJSON

    throwNon200 st = throwWithInternal (non200Msg st) . decodeNon200Resp

    throwWithInternal msg v =
      let err = err400 RemoteSchemaError $ T.pack msg
      in throwError err{qeInternal = Just $ J.toJSON v}

    httpExceptMsg =
      "HTTP exception occurred while sending the request to " <> show url

    non200Msg st = "introspection query to " <> show url
                   <> " has responded with " <> show st <> " status code"

    decodeNon200Resp bs = case J.eitherDecode bs of
      Right a -> J.object ["response" J..= (a :: J.Value)]
      Left _  -> J.object ["raw_body" J..= bsToTxt (BL.toStrict bs)]

-- | Parsing the introspection query result.  We use this newtype wrapper to
-- avoid orphan instances and parse JSON in the way that we need for GraphQL
-- introspection results.
newtype FromIntrospection a
  = FromIntrospection { fromIntrospection :: a }
  deriving (Show, Eq, Generic)

pErr :: (MonadFail m) => Text -> m a
pErr = fail . T.unpack

kindErr :: (MonadFail m) => Text -> Text -> m a
kindErr gKind eKind = pErr $ "Invalid `kind: " <> gKind <> "` in " <> eKind

instance J.FromJSON (FromIntrospection G.Description) where
  parseJSON = fmap (FromIntrospection . G.Description) . J.parseJSON

instance J.FromJSON (FromIntrospection G.ScalarTypeDefinition) where
  parseJSON = J.withObject "ScalarTypeDefinition" $ \o -> do
    kind <- o .:  "kind"
    name <- o .:  "name"
    desc <- o .:? "description"
    when (kind /= "SCALAR") $ kindErr kind "scalar"
    let desc' = fmap fromIntrospection desc
        r = G.ScalarTypeDefinition desc' name []
    return $ FromIntrospection r

instance J.FromJSON (FromIntrospection G.ObjectTypeDefinition) where
  parseJSON = J.withObject "ObjectTypeDefinition" $ \o -> do
    kind   <- o .:  "kind"
    name   <- o .:  "name"
    desc   <- o .:? "description"
    fields <- o .:? "fields"
    interfaces :: Maybe [FromIntrospection (G.InterfaceTypeDefinition [G.Name])] <- o .:? "interfaces"
    when (kind /= "OBJECT") $ kindErr kind "object"
    let implIfaces = map G._itdName $ maybe [] (fmap fromIntrospection) interfaces
        flds = maybe [] (fmap fromIntrospection) fields
        desc' = fmap fromIntrospection desc
        r = G.ObjectTypeDefinition desc' name implIfaces [] flds
    return $ FromIntrospection r

instance J.FromJSON (FromIntrospection G.FieldDefinition) where
  parseJSON = J.withObject "FieldDefinition" $ \o -> do
    name  <- o .:  "name"
    desc  <- o .:? "description"
    args  <- o .: "args"
    _type <- o .: "type"
    let desc' = fmap fromIntrospection desc
        r = G.FieldDefinition desc' name (fmap fromIntrospection args)
            (fromIntrospection _type) []
    return $ FromIntrospection r

instance J.FromJSON (FromIntrospection G.GType) where
  parseJSON = J.withObject "GType" $ \o -> do
    kind  <- o .: "kind"
    mName <- o .:? "name"
    mType <- o .:? "ofType"
    r <- case (kind, mName, mType) of
      ("NON_NULL", _, Just typ) -> return $ mkNotNull (fromIntrospection typ)
      ("NON_NULL", _, Nothing)  -> pErr "NON_NULL should have `ofType`"
      ("LIST", _, Just typ)     ->
        return $ G.TypeList (G.Nullability True) (fromIntrospection typ)
      ("LIST", _, Nothing)      -> pErr "LIST should have `ofType`"
      (_, Just name, _)         -> return $ G.TypeNamed (G.Nullability True) name
      _                         -> pErr $ "kind: " <> kind <> " should have name"
    return $ FromIntrospection r

    where
      mkNotNull typ = case typ of
        G.TypeList _ ty -> G.TypeList (G.Nullability False) ty
        G.TypeNamed _ n -> G.TypeNamed (G.Nullability False) n

instance J.FromJSON (FromIntrospection G.InputValueDefinition) where
  parseJSON = J.withObject "InputValueDefinition" $ \o -> do
    name  <- o .:  "name"
    desc  <- o .:? "description"
    _type <- o .: "type"
    defVal <- o .:? "defaultValue"
    let desc' = fmap fromIntrospection desc
    let defVal' = fmap fromIntrospection defVal
        r = G.InputValueDefinition desc' name (fromIntrospection _type) defVal'
    return $ FromIntrospection r

instance J.FromJSON (FromIntrospection (G.Value Void)) where
   parseJSON = J.withText "Value Void" $ \t ->
     let parseValueConst = G.runParser G.value
     in fmap FromIntrospection $ either (fail . T.unpack) return $ parseValueConst t

instance J.FromJSON (FromIntrospection (G.InterfaceTypeDefinition [G.Name])) where
  parseJSON = J.withObject "InterfaceTypeDefinition" $ \o -> do
    kind  <- o .: "kind"
    name  <- o .:  "name"
    desc  <- o .:? "description"
    fields <- o .:? "fields"
    possibleTypes :: Maybe [FromIntrospection G.ObjectTypeDefinition] <- o .:? "possibleTypes"
    let flds = maybe [] (fmap fromIntrospection) fields
        desc' = fmap fromIntrospection desc
        possTps = map G._otdName $ maybe [] (fmap fromIntrospection) possibleTypes
    when (kind /= "INTERFACE") $ kindErr kind "interface"
    -- TODO (non PDV) track which interfaces implement which other interfaces, after a
    -- GraphQL spec > Jun 2018 is released.
    let r = G.InterfaceTypeDefinition desc' name [] flds possTps
    return $ FromIntrospection r

instance J.FromJSON (FromIntrospection G.UnionTypeDefinition) where
  parseJSON = J.withObject "UnionTypeDefinition" $ \o -> do
    kind  <- o .: "kind"
    name  <- o .:  "name"
    desc  <- o .:? "description"
    possibleTypes <- o .: "possibleTypes"
    let possibleTypes' = map G._otdName $ fmap fromIntrospection possibleTypes
        desc' = fmap fromIntrospection desc
    when (kind /= "UNION") $ kindErr kind "union"
    let r = G.UnionTypeDefinition desc' name [] possibleTypes'
    return $ FromIntrospection r

instance J.FromJSON (FromIntrospection G.EnumTypeDefinition) where
  parseJSON = J.withObject "EnumTypeDefinition" $ \o -> do
    kind  <- o .: "kind"
    name  <- o .:  "name"
    desc  <- o .:? "description"
    vals  <- o .: "enumValues"
    when (kind /= "ENUM") $ kindErr kind "enum"
    let desc' = fmap fromIntrospection desc
    let r = G.EnumTypeDefinition desc' name [] (fmap fromIntrospection vals)
    return $ FromIntrospection r

instance J.FromJSON (FromIntrospection G.EnumValueDefinition) where
  parseJSON = J.withObject "EnumValueDefinition" $ \o -> do
    name  <- o .:  "name"
    desc  <- o .:? "description"
    let desc' = fmap fromIntrospection desc
    let r = G.EnumValueDefinition desc' name []
    return $ FromIntrospection r

instance J.FromJSON (FromIntrospection G.InputObjectTypeDefinition) where
  parseJSON = J.withObject "InputObjectTypeDefinition" $ \o -> do
    kind  <- o .: "kind"
    name  <- o .:  "name"
    desc  <- o .:? "description"
    mInputFields <- o .:? "inputFields"
    let inputFields = maybe [] (fmap fromIntrospection) mInputFields
    let desc' = fmap fromIntrospection desc
    when (kind /= "INPUT_OBJECT") $ kindErr kind "input_object"
    let r = G.InputObjectTypeDefinition desc' name [] inputFields
    return $ FromIntrospection r

instance J.FromJSON (FromIntrospection (G.TypeDefinition [G.Name])) where
  parseJSON = J.withObject "TypeDefinition" $ \o -> do
    kind :: Text <- o .: "kind"
    r <- case kind of
      "SCALAR" ->
        G.TypeDefinitionScalar . fromIntrospection <$> J.parseJSON (J.Object o)
      "OBJECT" ->
        G.TypeDefinitionObject . fromIntrospection <$> J.parseJSON (J.Object o)
      "INTERFACE" ->
        G.TypeDefinitionInterface . fromIntrospection <$> J.parseJSON (J.Object o)
      "UNION" ->
        G.TypeDefinitionUnion . fromIntrospection <$> J.parseJSON (J.Object o)
      "ENUM" ->
        G.TypeDefinitionEnum . fromIntrospection <$> J.parseJSON (J.Object o)
      "INPUT_OBJECT" ->
        G.TypeDefinitionInputObject . fromIntrospection <$> J.parseJSON (J.Object o)
      _ -> pErr $ "unknown kind: " <> kind
    return $ FromIntrospection r

instance J.FromJSON (FromIntrospection IntrospectionResult) where
  parseJSON = J.withObject "SchemaDocument" $ \o -> do
    _data <- o .: "data"
    schema <- _data .: "__schema"
    -- the list of types
    types <- schema .: "types"
    -- query root
    queryType <- schema .: "queryType"
    queryRoot <- queryType .: "name"
    -- mutation root
    mMutationType <- schema .:? "mutationType"
    mutationRoot <- case mMutationType of
      Nothing      -> return Nothing
      Just mutType -> do
        mutRoot <- mutType .: "name"
        return $ Just mutRoot
    -- subscription root
    mSubsType <- schema .:? "subscriptionType"
    subsRoot <- case mSubsType of
      Nothing      -> return Nothing
      Just subsType -> do
        subRoot <- subsType .: "name"
        return $ Just subRoot
    let r = IntrospectionResult (G.SchemaIntrospection (fmap fromIntrospection types))
            queryRoot mutationRoot subsRoot
    return $ FromIntrospection r

execRemoteGQ'
  :: ( HasVersion
     , MonadIO m
     , MonadError QErr m
     , Tracing.MonadTrace m
     )
  => Env.Environment
  -> HTTP.Manager
  -> UserInfo
  -> [N.Header]
  -> GQLReqUnparsed
  -> RemoteSchemaInfo
  -> G.OperationType
  -> m (DiffTime, [N.Header], BL.ByteString)
execRemoteGQ' env manager userInfo reqHdrs q rsi opType =  do
  when (opType == G.OperationTypeSubscription) $
    throw400 NotSupported "subscription to remote server is not supported"
  confHdrs <- makeHeadersFromConf env hdrConf
  let clientHdrs = bool [] (mkClientHeadersForward reqHdrs) fwdClientHdrs
      -- filter out duplicate headers
      -- priority: conf headers > resolved userinfo vars > client headers
      hdrMaps    = [ Map.fromList confHdrs
                   , Map.fromList userInfoToHdrs
                   , Map.fromList clientHdrs
                   ]
      headers  = Map.toList $ foldr Map.union Map.empty hdrMaps
      finalHeaders = addDefaultHeaders headers
  initReqE <- liftIO $ try $ HTTP.parseRequest (show url)
  initReq <- either httpThrow pure initReqE
  let req = initReq
           { HTTP.method = "POST"
           , HTTP.requestHeaders = finalHeaders
           , HTTP.requestBody = HTTP.RequestBodyLBS (J.encode q)
           , HTTP.responseTimeout = HTTP.responseTimeoutMicro (timeout * 1000000)
           }
  Tracing.tracedHttpRequest req \req' -> do
    (time, res)  <- withElapsedTime $ liftIO $ try $ HTTP.httpLbs req' manager
    resp <- either httpThrow return res
    pure (time, mkSetCookieHeaders resp, resp ^. Wreq.responseBody)
  where
    RemoteSchemaInfo url hdrConf fwdClientHdrs timeout = rsi
    httpThrow :: (MonadError QErr m) => HTTP.HttpException -> m a
    httpThrow = \case
      HTTP.HttpExceptionRequest _req content -> throw500 $ T.pack . show $ content
      HTTP.InvalidUrlException _url reason -> throw500 $ T.pack . show $ reason

    userInfoToHdrs = sessionVariablesToHeaders $ _uiSession userInfo
