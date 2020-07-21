module Hasura.GraphQL.RemoteServer where

import           Control.Exception                      (try)
import           Control.Lens                           ((^.))
import           Data.Aeson                             ((.:), (.:?))
import           Data.Foldable                          (foldlM)
import           Hasura.HTTP
import           Hasura.Prelude

import qualified Data.Aeson                             as J
import qualified Data.ByteString.Lazy                   as BL
import qualified Data.Environment                       as Env
import qualified Data.HashMap.Strict                    as Map
import qualified Data.Text                              as T
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Language.GraphQL.Draft.Parser          as G
import qualified Language.Haskell.TH.Syntax             as TH
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Types                     as N
import qualified Network.Wreq                           as Wreq

import           Hasura.GraphQL.Schema.Merge
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.RQL.DDL.Headers                 (makeHeadersFromConf)
import           Hasura.RQL.Types
import           Hasura.Server.Utils
import           Hasura.Server.Version                  (HasVersion)
import           Hasura.Session

import qualified Hasura.GraphQL.Context                 as GC
import qualified Hasura.GraphQL.Schema                  as GS
import qualified Hasura.GraphQL.Validate.Types          as VT
import qualified Hasura.Tracing                         as Tracing

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
  :: (HasVersion, MonadIO m, MonadError QErr m)
  => Env.Environment
  -> HTTP.Manager
  -> RemoteSchemaInfo
  -> m GC.RemoteGCtx
fetchRemoteSchema env manager def@(RemoteSchemaInfo name url headerConf _ timeout) = do
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

  introspectRes :: (FromIntrospection IntrospectionResult) <-
    either (remoteSchemaErr . T.pack) return $ J.eitherDecode respData
  let (sDoc, qRootN, mRootN, sRootN) =
        fromIntrospection introspectRes
  typMap <- either remoteSchemaErr return $ VT.fromSchemaDoc sDoc $
     VT.TLRemoteType name def
  let mQrTyp = Map.lookup qRootN typMap
      mMrTyp = (`Map.lookup` typMap) =<< mRootN
      mSrTyp = (`Map.lookup` typMap) =<< sRootN
  qrTyp <- liftMaybe noQueryRoot mQrTyp
  let mRmQR = VT.getObjTyM qrTyp
      mRmMR = VT.getObjTyM =<< mMrTyp
      mRmSR = VT.getObjTyM =<< mSrTyp
  rmQR <- liftMaybe (err400 Unexpected "query root has to be an object type") mRmQR
  return $ GC.RemoteGCtx typMap rmQR mRmMR mRmSR

  where
    noQueryRoot = err400 Unexpected "query root not found in remote schema"
    remoteSchemaErr :: (MonadError QErr m) => T.Text -> m a
    remoteSchemaErr = throw400 RemoteSchemaError

    throwHttpErr :: (MonadError QErr m) => HTTP.HttpException -> m a
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

mergeSchemas
  :: (MonadError QErr m)
  => RemoteSchemaMap
  -> GS.GCtxMap
  -- the merged GCtxMap and the default GCtx without roles
  -> m (GS.GCtxMap, GS.GCtx)
mergeSchemas rmSchemaMap gCtxMap = do
  def <- mkDefaultRemoteGCtx remoteSchemas
  merged <- mergeRemoteSchema gCtxMap def
  return (merged, def)
  where
    remoteSchemas = map rscGCtx $ Map.elems rmSchemaMap

mkDefaultRemoteGCtx
  :: (MonadError QErr m)
  => [GC.GCtx] -> m GS.GCtx
mkDefaultRemoteGCtx =
  foldlM mergeGCtx GC.emptyGCtx

-- merge a remote schema `gCtx` into current `gCtxMap`
mergeRemoteSchema
  :: (MonadError QErr m)
  => GS.GCtxMap
  -> GS.GCtx
  -> m GS.GCtxMap
mergeRemoteSchema ctxMap mergedRemoteGCtx =
  flip Map.traverseWithKey ctxMap $ \_ schemaCtx ->
    for schemaCtx $ \gCtx -> mergeGCtx gCtx mergedRemoteGCtx

-- | Parsing the introspection query result
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
    kind <- o .: "kind"
    name <- o .:  "name"
    desc <- o .:? "description"
    when (kind /= "SCALAR") $ kindErr kind "scalar"
    let desc' = fmap fromIntrospection desc
        r = G.ScalarTypeDefinition desc' name []
    return $ FromIntrospection r

instance J.FromJSON (FromIntrospection G.ObjectTypeDefinition) where
  parseJSON = J.withObject "ObjectTypeDefinition" $ \o -> do
    kind       <- o .: "kind"
    name       <- o .:  "name"
    desc       <- o .:? "description"
    fields     <- o .:? "fields"
    interfaces <- o .:? "interfaces"
    when (kind /= "OBJECT") $ kindErr kind "object"
    let implIfaces = map (G.NamedType . G._itdName) $
                     maybe [] (fmap fromIntrospection) interfaces
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
        return $ G.TypeList (G.Nullability True)
                 (G.ListType $ fromIntrospection typ)
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

instance J.FromJSON (FromIntrospection G.ValueConst) where
   parseJSON = J.withText "defaultValue" $ \t -> fmap FromIntrospection
     $ either (fail . T.unpack) return $ G.parseValueConst t

instance J.FromJSON (FromIntrospection G.InterfaceTypeDefinition) where
  parseJSON = J.withObject "InterfaceTypeDefinition" $ \o -> do
    kind  <- o .: "kind"
    name  <- o .:  "name"
    desc  <- o .:? "description"
    fields <- o .:? "fields"
    let flds = maybe [] (fmap fromIntrospection) fields
        desc' = fmap fromIntrospection desc
    when (kind /= "INTERFACE") $ kindErr kind "interface"
    let r = G.InterfaceTypeDefinition desc' name [] flds
    return $ FromIntrospection r

instance J.FromJSON (FromIntrospection G.UnionTypeDefinition) where
  parseJSON = J.withObject "UnionTypeDefinition" $ \o -> do
    kind  <- o .: "kind"
    name  <- o .:  "name"
    desc  <- o .:? "description"
    possibleTypes <- o .: "possibleTypes"
    let memberTys = map (G.NamedType . G._otdName) $
                    fmap fromIntrospection possibleTypes
        desc' = fmap fromIntrospection desc
    when (kind /= "UNION") $ kindErr kind "union"
    let r = G.UnionTypeDefinition desc' name [] memberTys
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

instance J.FromJSON (FromIntrospection G.TypeDefinition) where
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

type IntrospectionResult = ( G.SchemaDocument
                           , G.NamedType
                           , Maybe G.NamedType
                           , Maybe G.NamedType
                           )

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
    let r = ( G.SchemaDocument (fmap fromIntrospection types)
            , queryRoot
            , mutationRoot
            , subsRoot
            )
    return $ FromIntrospection r


getNamedTyp :: G.TypeDefinition -> G.Name
getNamedTyp ty = case ty of
  G.TypeDefinitionScalar t      -> G._stdName t
  G.TypeDefinitionObject t      -> G._otdName t
  G.TypeDefinitionInterface t   -> G._itdName t
  G.TypeDefinitionUnion t       -> G._utdName t
  G.TypeDefinitionEnum t        -> G._etdName t
  G.TypeDefinitionInputObject t -> G._iotdName t

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
execRemoteGQ' env manager userInfo reqHdrs q rsi opType = Tracing.traceHttpRequest (T.pack (show url)) $ do
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
  pure $ Tracing.SuspendedRequest req \req' -> do
    (time, res)  <- withElapsedTime $ liftIO $ try $ HTTP.httpLbs req' manager
    resp <- either httpThrow return res
    pure (time, mkSetCookieHeaders resp, resp ^. Wreq.responseBody)
  where
    RemoteSchemaInfo _ url hdrConf fwdClientHdrs timeout = rsi
    httpThrow :: (MonadError QErr m) => HTTP.HttpException -> m a
    httpThrow = \case
      HTTP.HttpExceptionRequest _req content -> throw500 $ T.pack . show $ content
      HTTP.InvalidUrlException _url reason -> throw500 $ T.pack . show $ reason

    userInfoToHdrs = sessionVariablesToHeaders $ _uiSession userInfo
