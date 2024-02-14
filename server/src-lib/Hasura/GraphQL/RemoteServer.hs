{-# LANGUAGE TemplateHaskell #-}

module Hasura.GraphQL.RemoteServer
  ( fetchRemoteSchema,
    stitchRemoteSchema,
    execRemoteGQ,
    FromIntrospection (..),
  )
where

import Control.Arrow.Extended (left)
import Control.Exception (try)
import Control.Lens (set, (^.))
import Control.Monad.Memoize
import Data.Aeson ((.:), (.:?))
import Data.Aeson qualified as J
import Data.ByteString.Lazy qualified as BL
import Data.Environment qualified as Env
import Data.FileEmbed (makeRelativeToProject)
import Data.HashMap.Strict.Extended qualified as HashMap
import Data.HashSet qualified as Set
import Data.List.Extended (duplicates)
import Data.Text qualified as T
import Data.Text.Extended (dquoteList, (<<>))
import Hasura.Base.Error
import Hasura.GraphQL.Parser.Monad (Parse)
import Hasura.GraphQL.Parser.Name qualified as GName
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Remote (buildRemoteParser)
import Hasura.GraphQL.Schema.Typename
import Hasura.GraphQL.Transport.HTTP.Protocol
import Hasura.HTTP
import Hasura.Prelude
import Hasura.RQL.DDL.Headers (makeHeadersFromConf)
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Roles (adminRoleName)
import Hasura.RQL.Types.Schema.Options qualified as Options
import Hasura.RemoteSchema.Metadata
import Hasura.RemoteSchema.SchemaCache.Types
import Hasura.Server.Utils
import Hasura.Services.Network
import Hasura.Session (UserInfo, adminUserInfo, sessionVariablesToHeaders, _uiSession)
import Hasura.Tracing qualified as Tracing
import Language.GraphQL.Draft.Parser qualified as G
import Language.GraphQL.Draft.Syntax qualified as G
import Language.Haskell.TH.Syntax qualified as TH
import Network.HTTP.Client.Transformable qualified as HTTP
import Network.URI (URI)
import Network.Wreq qualified as Wreq

-------------------------------------------------------------------------------
-- Core API

-- | Make an introspection query to the remote graphql server for the data we
-- need to present and stitch the remote schema. This powers add_remote_schema,
-- and also is called by schema cache rebuilding code in "Hasura.RQL.DDL.Schema.Cache".
fetchRemoteSchema ::
  forall m.
  (MonadIO m, MonadError QErr m, Tracing.MonadTrace m, ProvidesNetwork m) =>
  Env.Environment ->
  SchemaSampledFeatureFlags ->
  ValidatedRemoteSchemaDef ->
  m (IntrospectionResult, BL.ByteString, RemoteSchemaInfo)
fetchRemoteSchema env schemaSampledFeatureFlags rsDef = do
  (_, _, rawIntrospectionResult) <-
    execRemoteGQ env Tracing.b3TraceContextPropagator adminUserInfo [] rsDef introspectionQuery
  (ir, rsi) <- stitchRemoteSchema schemaSampledFeatureFlags rawIntrospectionResult rsDef
  -- The 'rawIntrospectionResult' contains the 'Bytestring' response of
  -- the introspection result of the remote server. We store this in the
  -- 'RemoteSchemaCtx' because we can use this when the 'introspect_remote_schema'
  -- is called by simple encoding the result to JSON.
  pure (ir, rawIntrospectionResult, rsi)

-- | Parses the remote schema introspection result, and check whether it looks
-- like it's a valid GraphQL endpoint even under the configured customization.
stitchRemoteSchema ::
  (MonadIO m, MonadError QErr m) =>
  SchemaSampledFeatureFlags ->
  BL.ByteString ->
  ValidatedRemoteSchemaDef ->
  m (IntrospectionResult, RemoteSchemaInfo)
stitchRemoteSchema schemaSampledFeatureFlags rawIntrospectionResult rsDef@ValidatedRemoteSchemaDef {..} = do
  -- Parse the JSON into flat GraphQL type AST.
  FromIntrospection _rscIntroOriginal <-
    J.eitherDecode rawIntrospectionResult `onLeft` (throwRemoteSchema . T.pack)

  -- Possibly transform type names from the remote schema, per the user's 'RemoteSchemaDef'.
  let rsCustomizer = getCustomizer (addDefaultRoots _rscIntroOriginal) _vrsdCustomization
  validateSchemaCustomizations rsCustomizer (irDoc _rscIntroOriginal)

  let remoteSchemaInfo = RemoteSchemaInfo {..}

  -- Check that the parsed GraphQL type info is valid by running the schema
  -- generation. The result is discarded, as the local schema will be built
  -- properly for each role at schema generation time, but this allows us to
  -- quickly reject an invalid schema.
  void
    $ runMemoizeT
    $ runRemoteSchema minimumValidContext Options.RemoteForwardAccurately
    $ buildRemoteParser @_ @_ @Parse
      _rscIntroOriginal
      mempty -- remote relationships
      remoteSchemaInfo
  return (_rscIntroOriginal, remoteSchemaInfo)
  where
    -- If there is no explicit mutation or subscription root type we need to check for
    -- objects type definitions with the default names "Mutation" and "Subscription".
    -- If found, we add the default roots explicitly to the IntrospectionResult.
    -- This simplifies the customization code.
    addDefaultRoots :: IntrospectionResult -> IntrospectionResult
    addDefaultRoots IntrospectionResult {..} =
      IntrospectionResult
        { irMutationRoot = getRootTypeName GName._Mutation irMutationRoot,
          irSubscriptionRoot = getRootTypeName GName._Subscription irSubscriptionRoot,
          ..
        }
      where
        getRootTypeName defaultName providedName =
          providedName <|> (defaultName <$ lookupObject irDoc defaultName)

    -- Minimum valid information required to run schema generation for
    -- the remote schema.
    minimumValidContext =
      SchemaContext
        HasuraSchema
        ignoreRemoteRelationship
        adminRoleName
        schemaSampledFeatureFlags

-- | Sends a GraphQL query to the given server.
execRemoteGQ ::
  ( MonadIO m,
    MonadError QErr m,
    Tracing.MonadTrace m,
    ProvidesNetwork m
  ) =>
  Env.Environment ->
  Tracing.HttpPropagator ->
  UserInfo ->
  [HTTP.Header] ->
  ValidatedRemoteSchemaDef ->
  GQLReqOutgoing ->
  -- | Returns the response body and headers, along with the time taken for the
  -- HTTP request to complete
  m (DiffTime, [HTTP.Header], BL.ByteString)
execRemoteGQ env tracesPropagator userInfo reqHdrs rsdef gqlReq@GQLReq {..} = do
  let gqlReqUnparsed = renderGQLReqOutgoing gqlReq

  when (G._todType _grQuery == G.OperationTypeSubscription)
    $ throwRemoteSchema "subscription to remote server is not supported"
  confHdrs <- makeHeadersFromConf env hdrConf
  let clientHdrs = bool [] (mkClientHeadersForward reqHdrs) fwdClientHdrs
      -- filter out duplicate headers
      -- priority: conf headers > resolved userinfo vars > client headers
      hdrMaps =
        [ HashMap.fromList confHdrs,
          HashMap.fromList userInfoToHdrs,
          HashMap.fromList clientHdrs
        ]
      headers = HashMap.toList $ foldr HashMap.union HashMap.empty hdrMaps
      finalHeaders = addDefaultHeaders headers
  initReq <- onLeft (HTTP.mkRequestEither $ tshow url) (throwRemoteSchemaHttp webhookEnvRecord)
  let req =
        initReq
          & set HTTP.method "POST"
          & set HTTP.headers finalHeaders
          & set HTTP.body (HTTP.RequestBodyLBS $ J.encode gqlReqUnparsed)
          & set HTTP.timeout (HTTP.responseTimeoutMicro (timeout * 1000000))

  manager <- askHTTPManager
  Tracing.traceHTTPRequest tracesPropagator req \req' -> do
    (time, res) <- withElapsedTime $ liftIO $ try $ HTTP.httpLbs req' manager
    resp <- onLeft res (throwRemoteSchemaHttp webhookEnvRecord)
    pure (time, mkSetCookieHeaders resp, resp ^. Wreq.responseBody)
  where
    ValidatedRemoteSchemaDef _name webhookEnvRecord hdrConf fwdClientHdrs timeout _mPrefix = rsdef
    url = _envVarValue webhookEnvRecord
    userInfoToHdrs = sessionVariablesToHeaders $ _uiSession userInfo

-------------------------------------------------------------------------------
-- Validation

validateSchemaCustomizations ::
  forall m.
  (MonadError QErr m) =>
  RemoteSchemaCustomizer ->
  RemoteSchemaIntrospection ->
  m ()
validateSchemaCustomizations remoteSchemaCustomizer remoteSchemaIntrospection = do
  validateSchemaCustomizationsConsistent remoteSchemaCustomizer remoteSchemaIntrospection
  validateSchemaCustomizationsDistinct remoteSchemaCustomizer remoteSchemaIntrospection

validateSchemaCustomizationsConsistent ::
  forall m.
  (MonadError QErr m) =>
  RemoteSchemaCustomizer ->
  RemoteSchemaIntrospection ->
  m ()
validateSchemaCustomizationsConsistent remoteSchemaCustomizer (RemoteSchemaIntrospection typeDefinitions) = do
  traverse_ validateInterfaceFields typeDefinitions
  where
    customizeFieldName = remoteSchemaCustomizeFieldName remoteSchemaCustomizer

    validateInterfaceFields :: G.TypeDefinition [G.Name] a -> m ()
    validateInterfaceFields = \case
      G.TypeDefinitionInterface G.InterfaceTypeDefinition {..} ->
        for_ _itdPossibleTypes $ \typeName ->
          for_ _itdFieldsDefinition $ \G.FieldDefinition {..} -> do
            let interfaceCustomizedFieldName = runCustomizeRemoteFieldName customizeFieldName _itdName _fldName
                typeCustomizedFieldName = runCustomizeRemoteFieldName customizeFieldName typeName _fldName
            when (interfaceCustomizedFieldName /= typeCustomizedFieldName)
              $ throwRemoteSchema
              $ "Remote schema customization inconsistency: field name mapping for field "
              <> _fldName
              <<> " of interface "
              <> _itdName
              <<> " is inconsistent with mapping for type "
              <> typeName
              <<> ". Interface field name maps to "
              <> interfaceCustomizedFieldName
              <<> ". Type field name maps to "
              <> typeCustomizedFieldName
              <<> "."
      _ -> pure ()

validateSchemaCustomizationsDistinct ::
  forall m.
  (MonadError QErr m) =>
  RemoteSchemaCustomizer ->
  RemoteSchemaIntrospection ->
  m ()
validateSchemaCustomizationsDistinct remoteSchemaCustomizer (RemoteSchemaIntrospection typeDefinitions) = do
  validateTypeMappingsAreDistinct
  traverse_ validateFieldMappingsAreDistinct typeDefinitions
  where
    customizeTypeName = remoteSchemaCustomizeTypeName remoteSchemaCustomizer
    customizeFieldName = runCustomizeRemoteFieldName (remoteSchemaCustomizeFieldName remoteSchemaCustomizer)

    validateTypeMappingsAreDistinct :: m ()
    validateTypeMappingsAreDistinct = do
      let dups = duplicates $ runMkTypename customizeTypeName <$> HashMap.keys typeDefinitions
      unless (Set.null dups)
        $ throwRemoteSchema
        $ "Type name mappings are not distinct; the following types appear more than once: "
        <> dquoteList dups

    validateFieldMappingsAreDistinct :: G.TypeDefinition a b -> m ()
    validateFieldMappingsAreDistinct = \case
      G.TypeDefinitionInterface G.InterfaceTypeDefinition {..} -> do
        let dups = duplicates $ customizeFieldName _itdName . G._fldName <$> _itdFieldsDefinition
        unless (Set.null dups)
          $ throwRemoteSchema
          $ "Field name mappings for interface type "
          <> _itdName
          <<> " are not distinct; the following fields appear more than once: "
          <> dquoteList dups
      G.TypeDefinitionObject G.ObjectTypeDefinition {..} -> do
        let dups = duplicates $ customizeFieldName _otdName . G._fldName <$> _otdFieldsDefinition
        unless (Set.null dups)
          $ throwRemoteSchema
          $ "Field name mappings for object type "
          <> _otdName
          <<> " are not distinct; the following fields appear more than once: "
          <> dquoteList dups
      _ -> pure ()

-------------------------------------------------------------------------------
-- Introspection

introspectionQuery :: GQLReqOutgoing
introspectionQuery =
  $( do
       fp <- makeRelativeToProject "src-rsr/introspection.json"
       TH.qAddDependentFile fp
       eitherResult <- TH.runIO $ J.eitherDecodeFileStrict fp
       either fail TH.lift $ do
         r@GQLReq {..} <- eitherResult
         op <- left (T.unpack . showQErr) $ getSingleOperation r
         pure GQLReq {_grQuery = op, ..}
   )

-- | Parsing the introspection query result.  We use this newtype wrapper to
-- avoid orphan instances and parse JSON in the way that we need for GraphQL
-- introspection results.
newtype FromIntrospection a = FromIntrospection {fromIntrospection :: a}
  deriving (Show, Eq, Generic, Functor)

instance J.FromJSON (FromIntrospection G.Description) where
  parseJSON = fmap (FromIntrospection . G.Description) . J.parseJSON

instance (J.FromJSON (FromIntrospection a)) => J.FromJSON (FromIntrospection (G.FieldDefinition a)) where
  parseJSON = J.withObject "FieldDefinition" $ \o -> do
    name <- o .: "name"
    desc <- fmap fromIntrospection <$> o .:? "description"
    args <- fmap fromIntrospection <$> o .: "args"
    type' <- fromIntrospection <$> o .: "type"
    let r =
          G.FieldDefinition
            desc
            name
            args
            type'
            []
    return $ FromIntrospection r

instance J.FromJSON (FromIntrospection G.GType) where
  parseJSON = J.withObject "GType" $ \o -> do
    kind <- o .: "kind"
    mName <- o .:? "name"
    mType <- o .:? "ofType"
    r <- case (kind, mName, mType) of
      ("NON_NULL", _, Just typ) -> return $ mkNotNull (fromIntrospection typ)
      ("NON_NULL", _, Nothing) -> pErr "NON_NULL should have `ofType`"
      ("LIST", _, Just typ) ->
        return $ G.TypeList (G.Nullability True) (fromIntrospection typ)
      ("LIST", _, Nothing) -> pErr "LIST should have `ofType`"
      (_, Just name, _) -> return $ G.TypeNamed (G.Nullability True) name
      _ -> pErr $ "kind: " <> kind <> " should have name"
    return $ FromIntrospection r
    where
      mkNotNull typ = case typ of
        G.TypeList _ ty -> G.TypeList (G.Nullability False) ty
        G.TypeNamed _ n -> G.TypeNamed (G.Nullability False) n

instance J.FromJSON (FromIntrospection G.InputValueDefinition) where
  parseJSON = J.withObject "InputValueDefinition" $ \o -> do
    name <- o .: "name"
    desc <- fmap fromIntrospection <$> o .:? "description"
    type' <- fromIntrospection <$> o .: "type"
    defVal <- fmap fromIntrospection <$> o .:? "defaultValue"
    return $ FromIntrospection $ G.InputValueDefinition desc name type' defVal []

instance J.FromJSON (FromIntrospection (G.Value Void)) where
  parseJSON = J.withText "Value Void" $ \t ->
    let parseValueConst = G.runParser G.value
     in FromIntrospection <$> onLeft (parseValueConst t) (fail . T.unpack)

instance J.FromJSON (FromIntrospection G.EnumValueDefinition) where
  parseJSON = J.withObject "EnumValueDefinition" $ \o -> do
    name <- o .: "name"
    desc <- fmap fromIntrospection <$> o .:? "description"
    return $ FromIntrospection $ G.EnumValueDefinition desc name []

instance J.FromJSON (FromIntrospection (G.TypeDefinition [G.Name] G.InputValueDefinition)) where
  parseJSON = J.withObject "TypeDefinition" $ \o -> do
    kind :: Text <- o .: "kind"
    name <- o .: "name"
    desc <- fmap fromIntrospection <$> o .:? "description"
    r <- case kind of
      "SCALAR" ->
        pure $ G.TypeDefinitionScalar $ G.ScalarTypeDefinition desc name []
      "OBJECT" -> do
        fields <- o .:? "fields"
        interfaces :: Maybe [FromIntrospection (G.TypeDefinition [G.Name] G.InputValueDefinition)] <- o .:? "interfaces"
        implIfaces <- for (foldMap (fmap fromIntrospection) interfaces) \case
          G.TypeDefinitionInterface (G.InterfaceTypeDefinition {..}) -> pure _itdName
          _ -> pErr $ "Error: object type " <> G.unName name <> " can only implement interfaces"
        let flds = foldMap (fmap fromIntrospection) fields
        pure $ G.TypeDefinitionObject $ G.ObjectTypeDefinition desc name implIfaces [] flds
      "INTERFACE" -> do
        fields <- o .:? "fields"
        possibleTypes :: Maybe [FromIntrospection (G.TypeDefinition [G.Name] G.InputValueDefinition)] <- o .:? "possibleTypes"
        let flds = maybe [] (fmap fromIntrospection) fields
        -- TODO (non PDV) track which interfaces implement which other interfaces, after a
        -- GraphQL spec > Jun 2018 is released.
        possTps <- for (foldMap (fmap fromIntrospection) possibleTypes) \case
          G.TypeDefinitionObject (G.ObjectTypeDefinition {..}) -> pure _otdName
          _ -> pErr $ "Error: interface type " <> G.unName name <> " can only be implemented by objects"
        pure $ G.TypeDefinitionInterface $ G.InterfaceTypeDefinition desc name [] flds possTps
      "UNION" -> do
        possibleTypes :: [FromIntrospection (G.TypeDefinition [G.Name] G.InputValueDefinition)] <- o .: "possibleTypes"
        possibleTypes' <- for (fromIntrospection <$> possibleTypes) \case
          G.TypeDefinitionObject (G.ObjectTypeDefinition {..}) -> pure _otdName
          _ -> pErr $ "Error: union type " <> G.unName name <> " can only be implemented by objects"
        pure $ G.TypeDefinitionUnion $ G.UnionTypeDefinition desc name [] possibleTypes'
      "ENUM" -> do
        vals <- fmap fromIntrospection <$> o .: "enumValues"
        pure $ G.TypeDefinitionEnum $ G.EnumTypeDefinition desc name [] vals
      "INPUT_OBJECT" -> do
        inputFields <- foldMap (fmap fromIntrospection) <$> o .:? "inputFields"
        pure $ G.TypeDefinitionInputObject $ G.InputObjectTypeDefinition desc name [] inputFields
      _ -> pErr $ "unknown kind: " <> kind
    return $ FromIntrospection r

instance J.FromJSON (FromIntrospection IntrospectionResult) where
  parseJSON = J.withObject "SchemaDocument" $ \o -> do
    data' <- o .: "data"
    schema <- data' .: "__schema"
    -- the list of types
    types <- schema .: "types"
    -- query root
    queryType <- schema .: "queryType"
    queryRoot <- queryType .: "name"
    -- mutation root
    mMutationType <- schema .:? "mutationType"
    mutationRoot <- for mMutationType (.: "name")
    -- subscription root
    mSubsType <- schema .:? "subscriptionType"
    subsRoot <- for mSubsType (.: "name")
    let types' =
          (fmap . fmap . fmap)
            -- presets are only defined for non-admin roles,
            -- an admin will not have any presets
            -- defined and the admin will be the one,
            -- who'll be adding the remote schema,
            -- hence presets are set to `Nothing`
            (`RemoteSchemaInputValueDefinition` Nothing)
            types
        r =
          IntrospectionResult
            (RemoteSchemaIntrospection $ HashMap.fromListOn getTypeName $ fromIntrospection <$> types')
            queryRoot
            mutationRoot
            subsRoot
    return $ FromIntrospection r

-------------------------------------------------------------------------------
-- Customization

getCustomizer :: IntrospectionResult -> Maybe RemoteSchemaCustomization -> RemoteSchemaCustomizer
getCustomizer _ Nothing = identityCustomizer
getCustomizer IntrospectionResult {..} (Just RemoteSchemaCustomization {..}) = RemoteSchemaCustomizer {..}
  where
    rootTypeNames =
      if isNothing _rscRootFieldsNamespace
        then Set.fromList $ catMaybes [Just irQueryRoot, irMutationRoot, irSubscriptionRoot]
        else mempty
    -- root type names should not be prefixed or suffixed unless
    -- there is a custom root namespace field
    protectedTypeNames = GName.builtInScalars <> rootTypeNames
    nameFilter name = not $ "__" `T.isPrefixOf` G.unName name || name `Set.member` protectedTypeNames

    mkPrefixSuffixMap :: Maybe G.Name -> Maybe G.Name -> [G.Name] -> HashMap G.Name G.Name
    mkPrefixSuffixMap mPrefix mSuffix names = HashMap.fromList $ case (mPrefix, mSuffix) of
      (Nothing, Nothing) -> []
      (Just prefix, Nothing) -> map (\name -> (name, prefix <> name)) names
      (Nothing, Just suffix) -> map (\name -> (name, name <> suffix)) names
      (Just prefix, Just suffix) -> map (\name -> (name, prefix <> name <> suffix)) names

    RemoteSchemaIntrospection typeDefinitions = irDoc
    typesToRename = filter nameFilter $ HashMap.keys typeDefinitions

    -- NOTE: We are creating a root type name mapping, this will be used to
    -- prefix the root field names with the root field namespace. We are doing
    -- this inorder to reduce typename conflicts while adding the root field
    -- namespace. Please note that this will have lower precedence order than
    -- the _rtcMapping. This means that a user can still change the root type
    -- name.
    rootTypeNameMap =
      mkPrefixSuffixMap _rscRootFieldsNamespace Nothing
        $ catMaybes [Just irQueryRoot, irMutationRoot, irSubscriptionRoot]

    typeRenameMap =
      case _rscTypeNames of
        Nothing -> rootTypeNameMap
        Just RemoteTypeCustomization {..} ->
          _rtcMapping <> rootTypeNameMap <> mkPrefixSuffixMap _rtcPrefix _rtcSuffix typesToRename

    typeFieldMap :: HashMap G.Name [G.Name] -- typeName -> fieldNames
    typeFieldMap =
      mapMaybe getFieldsNames typeDefinitions
      where
        getFieldsNames = \case
          G.TypeDefinitionObject G.ObjectTypeDefinition {..} -> Just $ G._fldName <$> _otdFieldsDefinition
          G.TypeDefinitionInterface G.InterfaceTypeDefinition {..} -> Just $ G._fldName <$> _itdFieldsDefinition
          _ -> Nothing

    mkFieldRenameMap RemoteFieldCustomization {..} fieldNames =
      _rfcMapping <> mkPrefixSuffixMap _rfcPrefix _rfcSuffix fieldNames

    fieldRenameMap =
      case _rscFieldNames of
        Nothing -> HashMap.empty
        Just fieldNameCustomizations ->
          let customizationMap = HashMap.fromList $ map (\rfc -> (_rfcParentType rfc, rfc)) fieldNameCustomizations
           in HashMap.intersectionWith mkFieldRenameMap customizationMap typeFieldMap

    _rscNamespaceFieldName = _rscRootFieldsNamespace
    _rscCustomizeTypeName = typeRenameMap
    _rscCustomizeFieldName = fieldRenameMap

------------------------------------------------------------------------------
-- Local error handling

pErr :: (MonadFail m) => Text -> m a
pErr = fail . T.unpack

throwRemoteSchema :: (QErrM m) => Text -> m a
throwRemoteSchema = throw400 RemoteSchemaError

throwRemoteSchemaHttp ::
  (QErrM m) =>
  EnvRecord URI ->
  HTTP.HttpException ->
  m a
throwRemoteSchemaHttp urlEnvRecord exception =
  throwError
    $ (baseError urlEnvRecord)
      { qeInternal = Just $ ExtraInternal $ getHttpExceptionJson (ShowErrorInfo True) $ HttpException exception
      }
  where
    baseError val = err400 RemoteSchemaError (httpExceptMsg val)
    httpExceptMsg val = "HTTP exception occurred while sending the request to " <> tshow (_envVarName val)
