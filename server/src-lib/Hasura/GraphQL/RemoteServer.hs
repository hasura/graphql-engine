module Hasura.GraphQL.RemoteServer where

import           Control.Exception             (try)
import           Control.Lens                  ((^.))
import           Data.Aeson                    ((.:), (.:?))
import           Data.FileEmbed                (embedStringFile)
import           Data.Foldable                 (foldlM)
import           Hasura.Prelude

import qualified Data.Aeson                    as J
import qualified Data.ByteString.Lazy          as BL
import qualified Data.CaseInsensitive          as CI
import qualified Data.HashMap.Strict           as Map
import qualified Data.HashSet                  as Set
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Language.GraphQL.Draft.Parser as G
import qualified Language.GraphQL.Draft.Syntax as G
import qualified Network.HTTP.Client           as HTTP
import qualified Network.Wreq                  as Wreq

import           Hasura.HTTP                   (wreqOptions)
import           Hasura.RQL.DDL.Headers        (getHeadersFromConf)
import           Hasura.RQL.Types
import           Hasura.Server.Utils           (bsToTxt, httpExceptToJSON)

import qualified Hasura.GraphQL.Context        as GC
import qualified Hasura.GraphQL.Schema         as GS
import qualified Hasura.GraphQL.Validate.Types as VT

introspectionQuery :: BL.ByteString
introspectionQuery = $(embedStringFile "src-rsr/introspection.json")

fetchRemoteSchema
  :: (MonadIO m, MonadError QErr m)
  => HTTP.Manager
  -> RemoteSchemaName
  -> RemoteSchemaInfo
  -> m GC.RemoteGCtx
fetchRemoteSchema manager name def@(RemoteSchemaInfo url headerConf _) = do
  headers <- getHeadersFromConf headerConf
  let hdrs = flip map headers $
             \(hn, hv) -> (CI.mk . T.encodeUtf8 $ hn, T.encodeUtf8 hv)
      options = wreqOptions manager hdrs
  res  <- liftIO $ try $ Wreq.postWith options (show url) introspectionQuery
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
      mMrTyp = maybe Nothing (`Map.lookup` typMap) mRootN
      mSrTyp = maybe Nothing (`Map.lookup` typMap) sRootN
  qrTyp <- liftMaybe noQueryRoot mQrTyp
  let mRmQR = VT.getObjTyM qrTyp
      mRmMR = join $ VT.getObjTyM <$> mMrTyp
      mRmSR = join $ VT.getObjTyM <$> mSrTyp
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
  => [GC.RemoteGCtx] -> m GS.GCtx
mkDefaultRemoteGCtx =
  foldlM (\combG -> mergeGCtx combG . convRemoteGCtx) GS.emptyGCtx

-- merge a remote schema `gCtx` into current `gCtxMap`
mergeRemoteSchema
  :: (MonadError QErr m)
  => GS.GCtxMap
  -> GS.GCtx
  -> m GS.GCtxMap
mergeRemoteSchema ctxMap mergedRemoteGCtx = do
  res <- forM (Map.toList ctxMap) $ \(role, gCtx) -> do
    updatedGCtx <- mergeGCtx gCtx mergedRemoteGCtx
    return (role, updatedGCtx)
  return $ Map.fromList res

mergeGCtx
  :: (MonadError QErr m)
  => GS.GCtx
  -> GS.GCtx
  -> m GS.GCtx
mergeGCtx gCtx rmMergedGCtx = do
  let rmTypes = GS._gTypes rmMergedGCtx
      hsraTyMap = GS._gTypes gCtx
  GS.checkSchemaConflicts gCtx rmMergedGCtx
  let newQR = mergeQueryRoot gCtx rmMergedGCtx
      newMR = mergeMutRoot gCtx rmMergedGCtx
      newSR = mergeSubRoot gCtx rmMergedGCtx
      newTyMap = mergeTyMaps hsraTyMap rmTypes newQR newMR
      updatedGCtx = gCtx { GS._gTypes = newTyMap
                         , GS._gQueryRoot = newQR
                         , GS._gMutRoot = newMR
                         , GS._gSubRoot = newSR
                         }
  return updatedGCtx

convRemoteGCtx :: GC.RemoteGCtx -> GS.GCtx
convRemoteGCtx rmGCtx =
  GS.emptyGCtx { GS._gTypes     = GC._rgTypes rmGCtx
               , GS._gQueryRoot = GC._rgQueryRoot rmGCtx
               , GS._gMutRoot   = GC._rgMutationRoot rmGCtx
               , GS._gSubRoot   = GC._rgSubscriptionRoot rmGCtx
               }


mergeQueryRoot :: GS.GCtx -> GS.GCtx -> VT.ObjTyInfo
mergeQueryRoot a b = GS._gQueryRoot a <> GS._gQueryRoot b

mergeMutRoot :: GS.GCtx -> GS.GCtx -> Maybe VT.ObjTyInfo
mergeMutRoot a b =
  let objA' = fromMaybe mempty $ GS._gMutRoot a
      objB  = fromMaybe mempty $ GS._gMutRoot b
      objA  = newRootOrEmpty objA' objB
      merged = objA <> objB
  in bool (Just merged) Nothing $ merged == mempty
  where
    newRootOrEmpty x y =
      if x == mempty && y /= mempty
      then mkNewEmptyMutRoot
      else x

mkNewEmptyMutRoot :: VT.ObjTyInfo
mkNewEmptyMutRoot = VT.ObjTyInfo (Just "mutation root")
                    (G.NamedType "mutation_root") Set.empty Map.empty

mkNewMutRoot :: VT.ObjFieldMap -> VT.ObjTyInfo
mkNewMutRoot flds = VT.ObjTyInfo (Just "mutation root")
                    (G.NamedType "mutation_root") Set.empty flds

mergeSubRoot :: GS.GCtx -> GS.GCtx -> Maybe VT.ObjTyInfo
mergeSubRoot a b =
  let objA' = fromMaybe mempty $ GS._gSubRoot a
      objB  = fromMaybe mempty $ GS._gSubRoot b
      objA  = newRootOrEmpty objA' objB
      merged = objA <> objB
  in bool (Just merged) Nothing $ merged == mempty
  where
    newRootOrEmpty x y =
      if x == mempty && y /= mempty
      then mkNewEmptySubRoot
      else x

mkNewEmptySubRoot :: VT.ObjTyInfo
mkNewEmptySubRoot = VT.ObjTyInfo (Just "subscription root")
                    (G.NamedType "subscription_root") Set.empty Map.empty


mergeTyMaps
  :: VT.TypeMap
  -> VT.TypeMap
  -> VT.ObjTyInfo
  -> Maybe VT.ObjTyInfo
  -> VT.TypeMap
mergeTyMaps hTyMap rmTyMap newQR newMR =
  let newTyMap  = hTyMap <> rmTyMap
      newTyMap' =
        Map.insert (G.NamedType "query_root") (VT.TIObj newQR) newTyMap
  in maybe newTyMap' (\mr -> Map.insert
                              (G.NamedType "mutation_root")
                              (VT.TIObj mr) newTyMap') newMR


-- parsing the introspection query result

newtype FromIntrospection a
  = FromIntrospection { fromIntrospection :: a }
  deriving (Show, Eq, Generic)

pErr :: (Monad m) => Text -> m a
pErr = fail . T.unpack

kindErr :: (Monad m) => Text -> Text -> m a
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
