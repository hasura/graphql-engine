module Hasura.GraphQL.RemoteServer
  ( fetchRemoteSchema
  , IntrospectionResult
  , execRemoteGQ
  , identityCustomizer
  , customizeIntrospectionResult
  -- The following exports are needed for unit tests
  , introspectionResultToJSON
  , parseIntrospectionResult
  , getCustomizer
  , validateSchemaCustomizationsDistinct
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                             as J
import qualified Data.Aeson.Types                       as J
import qualified Data.ByteString.Lazy                   as BL
import qualified Data.Environment                       as Env
import qualified Data.HashMap.Strict                    as Map
import qualified Data.HashSet                           as Set
import qualified Data.Text                              as T
import qualified Hasura.Tracing                         as Tracing
import qualified Language.GraphQL.Draft.Parser          as G
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Language.Haskell.TH.Syntax             as TH
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Types                     as N
import qualified Network.Wreq                           as Wreq

import           Control.Arrow.Extended                 (left)
import           Control.Exception                      (try)
import           Control.Lens                           ((^.))
import           Control.Monad.Unique
import           Data.Aeson                             ((.:), (.:?), (.=))
import           Data.FileEmbed                         (makeRelativeToProject)
import           Data.List.Extended                     (duplicates)
import           Data.Text.Extended                     (dquoteList, toTxt, (<<>))
import           Data.Tuple                             (swap)
import           Network.URI                            (URI)

import qualified Hasura.GraphQL.Parser.Monad            as P

import           Hasura.Base.Error
import           Hasura.GraphQL.Parser.Collect          ()
   -- Needed for GHCi and HLS due to TH in cyclically dependent modules (see https://gitlab.haskell.org/ghc/ghc/-/issues/1012)
import           Hasura.GraphQL.Schema.Remote           (buildRemoteParser)
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.HTTP
import           Hasura.RQL.DDL.Headers                 (makeHeadersFromConf)
import           Hasura.RQL.Types
import           Hasura.Server.Utils
import           Hasura.Server.Version                  (HasVersion)
import           Hasura.Session


introspectionQuery :: GQLReqOutgoing
introspectionQuery =
  $(do
       fp <- makeRelativeToProject "src-rsr/introspection.json"
       TH.qAddDependentFile fp
       eitherResult <- TH.runIO $ J.eitherDecodeFileStrict fp
       either fail TH.lift $ do
         r@GQLReq{..} <- eitherResult
         op <- left show $ getSingleOperation r
         pure GQLReq{_grQuery = op, ..}
   )

validateSchemaCustomizations
  :: forall m
   . MonadError QErr m
  => RemoteSchemaCustomizer
  -> RemoteSchemaIntrospection
  -> m ()
validateSchemaCustomizations remoteSchemaCustomizer remoteSchemaIntrospection = do
  validateSchemaCustomizationsConsistent remoteSchemaCustomizer remoteSchemaIntrospection
  validateSchemaCustomizationsDistinct remoteSchemaCustomizer remoteSchemaIntrospection

validateSchemaCustomizationsConsistent
  :: forall m
   . MonadError QErr m
  => RemoteSchemaCustomizer
  -> RemoteSchemaIntrospection
  -> m ()
validateSchemaCustomizationsConsistent remoteSchemaCustomizer (RemoteSchemaIntrospection typeDefinitions) = do
  traverse_ validateInterfaceFields typeDefinitions
  where
    customizeFieldName = remoteSchemaCustomizeFieldName remoteSchemaCustomizer

    validateInterfaceFields :: G.TypeDefinition [G.Name] a -> m ()
    validateInterfaceFields = \case
      G.TypeDefinitionInterface G.InterfaceTypeDefinition{..} ->
        for_ _itdPossibleTypes $ \typeName ->
          for_ _itdFieldsDefinition $ \G.FieldDefinition{..} -> do
            let interfaceCustomizedFieldName = customizeFieldName _itdName _fldName
                typeCustomizedFieldName = customizeFieldName typeName _fldName
            when (interfaceCustomizedFieldName /= typeCustomizedFieldName) $
              throwRemoteSchema
                $ "Remote schema customization inconsistency: field name mapping for field "
                <> _fldName <<> " of interface " <>  _itdName
                <<> " is inconsistent with mapping for type " <> typeName
                <<> ". Interface field name maps to " <> interfaceCustomizedFieldName
                <<> ". Type field name maps to " <> typeCustomizedFieldName <<> "."
      _ -> pure ()


validateSchemaCustomizationsDistinct
  :: forall m
   . MonadError QErr m
  => RemoteSchemaCustomizer
  -> RemoteSchemaIntrospection
  -> m ()
validateSchemaCustomizationsDistinct remoteSchemaCustomizer (RemoteSchemaIntrospection typeDefinitions) = do
  validateTypeMappingsAreDistinct
  traverse_ validateFieldMappingsAreDistinct typeDefinitions
  where
    customizeTypeName = remoteSchemaCustomizeTypeName remoteSchemaCustomizer
    customizeFieldName = remoteSchemaCustomizeFieldName remoteSchemaCustomizer

    validateTypeMappingsAreDistinct :: m ()
    validateTypeMappingsAreDistinct = do
      let dups = duplicates $ (customizeTypeName . typeDefinitionName) <$> typeDefinitions
      unless (Set.null dups) $
        throwRemoteSchema $
          "Type name mappings are not distinct; the following types appear more than once: " <>
          dquoteList dups

    validateFieldMappingsAreDistinct :: G.TypeDefinition a b -> m ()
    validateFieldMappingsAreDistinct = \case
      G.TypeDefinitionInterface G.InterfaceTypeDefinition{..} -> do
        let dups = duplicates $ (customizeFieldName _itdName . G._fldName) <$> _itdFieldsDefinition
        unless (Set.null dups) $
          throwRemoteSchema $
            "Field name mappings for interface type " <> _itdName <<>
            " are not distinct; the following fields appear more than once: " <>
            dquoteList dups
      G.TypeDefinitionObject G.ObjectTypeDefinition{..} -> do
        let dups = duplicates $ (customizeFieldName _otdName . G._fldName) <$> _otdFieldsDefinition
        unless (Set.null dups) $
          throwRemoteSchema $
            "Field name mappings for object type " <> _otdName <<>
            " are not distinct; the following fields appear more than once: " <>
            dquoteList dups
      _ -> pure ()

-- | Make an introspection query to the remote graphql server for the data we
-- need to present and stitch the remote schema. This powers add_remote_schema,
-- and also is called by schema cache rebuilding code in "Hasura.RQL.DDL.Schema.Cache".
fetchRemoteSchema
  :: forall m
   . (HasVersion, MonadIO m, MonadUnique m, MonadError QErr m, Tracing.MonadTrace m)
  => Env.Environment
  -> HTTP.Manager
  -> RemoteSchemaName
  -> ValidatedRemoteSchemaDef
  -> m RemoteSchemaCtx
fetchRemoteSchema env manager _rscName rsDef@ValidatedRemoteSchemaDef{..} = do
  (_, _, rscRawIntrospectionResultDirty) <-
    execRemoteGQ env manager adminUserInfo [] rsDef introspectionQuery

  -- Parse the JSON into flat GraphQL type AST
  FromIntrospection _rscIntroOriginal <-
    J.eitherDecode rscRawIntrospectionResultDirty `onLeft` (throwRemoteSchema . T.pack)

  -- possibly transform type names from the remote schema, per the user's 'RemoteSchemaDef'
  let rsCustomizer = getCustomizer (addDefaultRoots _rscIntroOriginal) _vrsdCustomization

  validateSchemaCustomizations rsCustomizer (irDoc _rscIntroOriginal)

  let customizedIntro = customizeIntrospectionResult rsCustomizer _rscIntroOriginal
      _rscRawIntrospectionResult = J.encode $ FromIntrospection customizedIntro

  let _rscInfo = RemoteSchemaInfo{..}
  -- Check that the parsed GraphQL type info is valid by running the schema generation
  (piQuery, piMutation, piSubscription) <-
    P.runSchemaT @m @(P.ParseT Identity) $ buildRemoteParser _rscIntroOriginal _rscInfo

  -- The 'rawIntrospectionResult' contains the 'Bytestring' response of
  -- the introspection result of the remote server. We store this in the
  -- 'RemoteSchemaCtx' because we can use this when the 'introspect_remote_schema'
  -- is called by simple encoding the result to JSON.
  return RemoteSchemaCtx
    { _rscPermissions = mempty
    , _rscParsed = ParsedIntrospection{..}
    , ..}

  where
    -- If there is no explicit mutation or subscription root type we need to check for
    -- objects type definitions with the default names "Mutation" and "Subscription".
    -- If found, we add the default roots explicitly to the IntrospectionResult.
    -- This simplifies the customization code.
    addDefaultRoots :: IntrospectionResult -> IntrospectionResult
    addDefaultRoots IntrospectionResult {..} =
      IntrospectionResult
      { irMutationRoot = getRootTypeName $$(G.litName "Mutation") irMutationRoot
      , irSubscriptionRoot = getRootTypeName $$(G.litName "Subscription") irSubscriptionRoot
      , ..
      }
      where
        getRootTypeName defaultName providedName =
          providedName <|> (defaultName <$ lookupObject irDoc defaultName)

-- | Parsing the introspection query result.  We use this newtype wrapper to
-- avoid orphan instances and parse JSON in the way that we need for GraphQL
-- introspection results.
newtype FromIntrospection a
  = FromIntrospection { fromIntrospection :: a }
  deriving (Show, Eq, Generic, Functor)

-- | Include a map from type name to kind. This allows us to pass
-- extra type information required to convert our schema
-- back into JSON.
data WithKinds a
  = WithKinds !(HashMap G.Name Text) !a
  deriving (Show, Eq, Generic, Functor)

pErr :: (MonadFail m) => Text -> m a
pErr = fail . T.unpack

kindErr :: (MonadFail m) => Text -> Text -> m a
kindErr gKind eKind = pErr $ "Invalid `kind: " <> gKind <> "` in " <> eKind


instance J.FromJSON (FromIntrospection G.Description) where
  parseJSON = fmap (FromIntrospection . G.Description) . J.parseJSON

instance J.ToJSON (FromIntrospection G.Description) where
  toJSON = J.toJSON . G.unDescription . fromIntrospection

instance J.FromJSON (FromIntrospection G.ScalarTypeDefinition) where
  parseJSON = J.withObject "ScalarTypeDefinition" $ \o -> do
    kind <- o .:  "kind"
    name <- o .:  "name"
    desc <- o .:? "description"
    when (kind /= "SCALAR") $ kindErr kind "scalar"
    let desc' = fmap fromIntrospection desc
        r = G.ScalarTypeDefinition desc' name []
    return $ FromIntrospection r

instance J.ToJSON (FromIntrospection G.ScalarTypeDefinition) where
  toJSON (FromIntrospection G.ScalarTypeDefinition {..}) = objectWithoutNullValues
    [ "kind" .= J.String "SCALAR"
    , "name" .= _stdName
    , "description" .= fmap FromIntrospection _stdDescription
    ]

instance J.FromJSON (FromIntrospection (G.ObjectTypeDefinition G.InputValueDefinition)) where
  parseJSON = J.withObject "ObjectTypeDefinition" $ \o -> do
    kind   <- o .:  "kind"
    name   <- o .:  "name"
    desc   <- o .:? "description"
    fields <- o .:? "fields"
    interfaces :: Maybe [FromIntrospection (G.InterfaceTypeDefinition [G.Name] G.InputValueDefinition)] <- o .:? "interfaces"
    when (kind /= "OBJECT") $ kindErr kind "object"
    let implIfaces = map G._itdName $ maybe [] (fmap fromIntrospection) interfaces
        flds = maybe [] (fmap fromIntrospection) fields
        desc' = fmap fromIntrospection desc
        r = G.ObjectTypeDefinition desc' name implIfaces [] flds
    return $ FromIntrospection r

instance J.ToJSON (WithKinds (G.ObjectTypeDefinition G.InputValueDefinition)) where
  toJSON (WithKinds kinds G.ObjectTypeDefinition {..}) = objectWithoutNullValues
    [ "kind" .= J.String "OBJECT"
    , "name" .= _otdName
    , "description" .= fmap FromIntrospection _otdDescription
    , "fields" .= fmap (WithKinds kinds) _otdFieldsDefinition
    , "interfaces" .= fmap (WithKinds kinds . toInterfaceTypeDefinition) _otdImplementsInterfaces
    ]
    where
      toInterfaceTypeDefinition :: G.Name -> G.InterfaceTypeDefinition [G.Name] G.InputValueDefinition
      toInterfaceTypeDefinition name = G.InterfaceTypeDefinition Nothing name [] [] []

instance (J.FromJSON (FromIntrospection a)) => J.FromJSON (FromIntrospection (G.FieldDefinition a)) where
  parseJSON = J.withObject "FieldDefinition" $ \o -> do
    name  <- o .:  "name"
    desc  <- o .:? "description"
    args  <- o .: "args"
    _type <- o .: "type"
    let desc' = fmap fromIntrospection desc
        r = G.FieldDefinition desc' name (fmap fromIntrospection args)
            (fromIntrospection _type) []
    return $ FromIntrospection r

instance J.ToJSON (WithKinds a) => J.ToJSON (WithKinds (G.FieldDefinition a)) where
  toJSON (WithKinds kinds G.FieldDefinition {..}) = objectWithoutNullValues
    [ "name" .= _fldName
    , "description" .= fmap FromIntrospection _fldDescription
    , "args" .= fmap (WithKinds kinds) _fldArgumentsDefinition
    , "type" .= WithKinds kinds _fldType
    ]

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

instance J.ToJSON (WithKinds G.GType) where
  toJSON (WithKinds kinds gtype) = objectWithoutNullValues $ case gtype of
    G.TypeNamed (G.Nullability True) name ->
      [ "kind" .= Map.lookup name kinds
      , "name" .= name
      ]
    G.TypeNamed (G.Nullability False) name ->
      [ "kind" .= J.String "NON_NULL"
      , "ofType" .= WithKinds kinds (G.TypeNamed (G.Nullability True) name)
      ]
    G.TypeList (G.Nullability True) ty ->
      [ "kind" .= J.String "LIST"
      , "ofType" .= WithKinds kinds ty
      ]
    G.TypeList (G.Nullability False) ty ->
      [ "kind" .= J.String "NON_NULL"
      , "ofType" .= WithKinds kinds (G.TypeList (G.Nullability True) ty)
      ]

instance J.FromJSON (FromIntrospection G.InputValueDefinition) where
  parseJSON = J.withObject "InputValueDefinition" $ \o -> do
    name  <- o .:  "name"
    desc  <- o .:? "description"
    _type <- o .: "type"
    defVal <- o .:? "defaultValue"
    let desc' = fmap fromIntrospection desc
    let defVal' = fmap fromIntrospection defVal
        r = G.InputValueDefinition desc' name (fromIntrospection _type) defVal' []
    return $ FromIntrospection r

instance J.ToJSON (WithKinds G.InputValueDefinition) where
  toJSON (WithKinds kinds G.InputValueDefinition {..}) = objectWithoutNullValues
    [ "name" .= _ivdName
    , "description" .= fmap FromIntrospection _ivdDescription
    , "type" .= WithKinds kinds _ivdType
    , "defaultValue" .= fmap FromIntrospection _ivdDefaultValue
    ]

instance J.FromJSON (FromIntrospection (G.Value Void)) where
   parseJSON = J.withText "Value Void" $ \t ->
     let parseValueConst = G.runParser G.value
     in FromIntrospection <$> onLeft (parseValueConst t) (fail . T.unpack)

instance J.ToJSON (FromIntrospection (G.Value Void)) where
  toJSON = J.String . toTxt . fromIntrospection

instance J.FromJSON (FromIntrospection (G.InterfaceTypeDefinition [G.Name] G.InputValueDefinition)) where
  parseJSON = J.withObject "InterfaceTypeDefinition" $ \o -> do
    kind  <- o .: "kind"
    name  <- o .:  "name"
    desc  <- o .:? "description"
    fields <- o .:? "fields"
    possibleTypes :: Maybe [FromIntrospection (G.ObjectTypeDefinition G.InputValueDefinition)] <- o .:? "possibleTypes"
    let flds = maybe [] (fmap fromIntrospection) fields
        desc' = fmap fromIntrospection desc
        possTps = map G._otdName $ maybe [] (fmap fromIntrospection) possibleTypes
    when (kind /= "INTERFACE") $ kindErr kind "interface"
    -- TODO (non PDV) track which interfaces implement which other interfaces, after a
    -- GraphQL spec > Jun 2018 is released.
    let r = G.InterfaceTypeDefinition desc' name [] flds possTps
    return $ FromIntrospection r

instance J.ToJSON (WithKinds (G.InterfaceTypeDefinition [G.Name] G.InputValueDefinition)) where
  toJSON (WithKinds kinds G.InterfaceTypeDefinition {..}) = objectWithoutNullValues
    [ "kind" .= J.String "INTERFACE"
    , "name" .= _itdName
    , "description" .= fmap FromIntrospection _itdDescription
    , "fields" .= fmap (WithKinds kinds) _itdFieldsDefinition
    , "possibleTypes" .= fmap (WithKinds kinds . toObjectTypeDefinition) _itdPossibleTypes
    ]

instance J.FromJSON (FromIntrospection G.UnionTypeDefinition) where
  parseJSON = J.withObject "UnionTypeDefinition" $ \o -> do
    kind  <- o .: "kind"
    name  <- o .:  "name"
    desc  <- o .:? "description"
    possibleTypes :: [FromIntrospection (G.ObjectTypeDefinition G.InputValueDefinition)] <- o .: "possibleTypes"
    let possibleTypes' = map G._otdName $ fmap fromIntrospection possibleTypes
        desc' = fmap fromIntrospection desc
    when (kind /= "UNION") $ kindErr kind "union"
    let r = G.UnionTypeDefinition desc' name [] possibleTypes'
    return $ FromIntrospection r

instance J.ToJSON (WithKinds G.UnionTypeDefinition) where
  toJSON (WithKinds kinds G.UnionTypeDefinition {..}) = objectWithoutNullValues
    [ "kind" .= J.String "UNION"
    , "name" .= _utdName
    , "description" .= fmap FromIntrospection _utdDescription
    , "possibleTypes" .= fmap (WithKinds kinds . toObjectTypeDefinition) _utdMemberTypes
    ]

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

instance J.ToJSON (FromIntrospection G.EnumTypeDefinition) where
  toJSON (FromIntrospection G.EnumTypeDefinition {..}) = objectWithoutNullValues
    [ "kind" .= J.String "ENUM"
    , "name" .= _etdName
    , "description" .= fmap FromIntrospection _etdDescription
    , "enumValues" .= fmap FromIntrospection _etdValueDefinitions
    ]

instance J.FromJSON (FromIntrospection G.EnumValueDefinition) where
  parseJSON = J.withObject "EnumValueDefinition" $ \o -> do
    name  <- o .:  "name"
    desc  <- o .:? "description"
    let desc' = fmap fromIntrospection desc
    let r = G.EnumValueDefinition desc' name []
    return $ FromIntrospection r

instance J.ToJSON (FromIntrospection G.EnumValueDefinition) where
  toJSON (FromIntrospection G.EnumValueDefinition {..}) = objectWithoutNullValues
    [ "name" .= _evdName
    , "description" .= fmap FromIntrospection _evdDescription
    ]

instance J.FromJSON (FromIntrospection (G.InputObjectTypeDefinition G.InputValueDefinition)) where
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

instance J.ToJSON (WithKinds (G.InputObjectTypeDefinition G.InputValueDefinition)) where
  toJSON (WithKinds kinds G.InputObjectTypeDefinition {..}) = objectWithoutNullValues
    [ "kind" .= J.String "INPUT_OBJECT"
    , "name" .= _iotdName
    , "description" .= fmap FromIntrospection _iotdDescription
    , "inputFields" .= fmap (WithKinds kinds) _iotdValueDefinitions
    ]

instance J.FromJSON (FromIntrospection (G.TypeDefinition [G.Name] G.InputValueDefinition)) where
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

instance J.ToJSON (WithKinds (G.TypeDefinition [G.Name] G.InputValueDefinition)) where
  toJSON (WithKinds kinds typeDefinition) = case typeDefinition of
    G.TypeDefinitionScalar scalarTypeDefinition -> J.toJSON $ FromIntrospection scalarTypeDefinition
    G.TypeDefinitionObject objectTypeDefinition -> J.toJSON $ WithKinds kinds objectTypeDefinition
    G.TypeDefinitionInterface interfaceTypeDefinition -> J.toJSON $ WithKinds kinds interfaceTypeDefinition
    G.TypeDefinitionUnion unionTypeDefinition -> J.toJSON $ WithKinds kinds unionTypeDefinition
    G.TypeDefinitionEnum enumTypeDefinition -> J.toJSON $ FromIntrospection enumTypeDefinition
    G.TypeDefinitionInputObject inputObjectTypeDefinition -> J.toJSON $ WithKinds kinds inputObjectTypeDefinition

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
          (RemoteSchemaIntrospection (fmap fromIntrospection types'))
            queryRoot mutationRoot subsRoot
    return $ FromIntrospection r

instance J.ToJSON (FromIntrospection IntrospectionResult) where
  toJSON (FromIntrospection IntrospectionResult{..}) = objectWithoutNullValues ["data" .= _data]
    where
      _data = objectWithoutNullValues ["__schema" .= schema]
      schema = objectWithoutNullValues
                [ "types" .= fmap (WithKinds kinds . fmap _rsitdDefinition) types
                , "queryType" .= queryType
                , "mutationType" .= mutationType
                , "subscriptionType" .= subscriptionType
                ]
      RemoteSchemaIntrospection types = irDoc
      kinds = Map.fromList $ types <&> \case
        G.TypeDefinitionScalar G.ScalarTypeDefinition{..}           -> (_stdName, "SCALAR")
        G.TypeDefinitionObject G.ObjectTypeDefinition{..}           -> (_otdName, "OBJECT")
        G.TypeDefinitionInterface G.InterfaceTypeDefinition{..}     -> (_itdName, "INTERFACE")
        G.TypeDefinitionUnion G.UnionTypeDefinition{..}             -> (_utdName, "UNION")
        G.TypeDefinitionEnum G.EnumTypeDefinition{..}               -> (_etdName, "ENUM")
        G.TypeDefinitionInputObject G.InputObjectTypeDefinition{..} -> (_iotdName, "INPUT_OBJECT")
      named :: G.Name -> J.Object
      named = ("name" .=)
      queryType = named irQueryRoot
      mutationType = named <$> irMutationRoot
      subscriptionType = named <$> irSubscriptionRoot

parseIntrospectionResult :: J.Value -> J.Parser IntrospectionResult
parseIntrospectionResult value = fromIntrospection <$> J.parseJSON value

introspectionResultToJSON :: IntrospectionResult -> J.Value
introspectionResultToJSON = J.toJSON . FromIntrospection

objectWithoutNullValues :: [J.Pair] -> J.Value
objectWithoutNullValues = J.object . filter notNull
  where
    notNull (_, J.Null) = False
    notNull _           = True

toObjectTypeDefinition :: G.Name -> G.ObjectTypeDefinition G.InputValueDefinition
toObjectTypeDefinition name = G.ObjectTypeDefinition Nothing name [] [] []

execRemoteGQ
  :: ( HasVersion
     , MonadIO m
     , MonadError QErr m
     , Tracing.MonadTrace m
     )
  => Env.Environment
  -> HTTP.Manager
  -> UserInfo
  -> [N.Header]
  -> ValidatedRemoteSchemaDef
  -> GQLReqOutgoing
  -> m (DiffTime, [N.Header], BL.ByteString)
  -- ^ Returns the response body and headers, along with the time taken for the
  -- HTTP request to complete
execRemoteGQ env manager userInfo reqHdrs rsdef gqlReq@GQLReq{..} =  do
  let gqlReqUnparsed = renderGQLReqOutgoing gqlReq

  when (G._todType _grQuery == G.OperationTypeSubscription) $
    throwRemoteSchema "subscription to remote server is not supported"
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
  initReq <- onLeft initReqE (throwRemoteSchemaHttp url)
  let req = initReq
           { HTTP.method = "POST"
           , HTTP.requestHeaders = finalHeaders
           , HTTP.requestBody = HTTP.RequestBodyLBS (J.encode gqlReqUnparsed)
           , HTTP.responseTimeout = HTTP.responseTimeoutMicro (timeout * 1000000)
           }
  Tracing.tracedHttpRequest req \req' -> do
    (time, res)  <- withElapsedTime $ liftIO $ try $ HTTP.httpLbs req' manager
    resp <- onLeft res (throwRemoteSchemaHttp url)
    pure (time, mkSetCookieHeaders resp, resp ^. Wreq.responseBody)
  where
    ValidatedRemoteSchemaDef url hdrConf fwdClientHdrs timeout _mPrefix = rsdef

    userInfoToHdrs = sessionVariablesToHeaders $ _uiSession userInfo

identityCustomizer :: RemoteSchemaCustomizer
identityCustomizer = RemoteSchemaCustomizer Nothing mempty mempty mempty mempty

typeDefinitionName :: G.TypeDefinition a b -> G.Name
typeDefinitionName = \case
  G.TypeDefinitionScalar G.ScalarTypeDefinition{..}           -> _stdName
  G.TypeDefinitionObject G.ObjectTypeDefinition{..}           -> _otdName
  G.TypeDefinitionInterface G.InterfaceTypeDefinition{..}     -> _itdName
  G.TypeDefinitionUnion G.UnionTypeDefinition{..}             -> _utdName
  G.TypeDefinitionEnum G.EnumTypeDefinition{..}               -> _etdName
  G.TypeDefinitionInputObject G.InputObjectTypeDefinition{..} -> _iotdName

getCustomizer :: IntrospectionResult -> Maybe RemoteSchemaCustomization -> RemoteSchemaCustomizer
getCustomizer _ Nothing = identityCustomizer
getCustomizer IntrospectionResult{..} (Just RemoteSchemaCustomization{..}) = RemoteSchemaCustomizer{..}
  where
    mapMap f = Map.fromList . map f . Map.toList
    invertMap = mapMap swap -- key collisions are checked for later in validateSchemaCustomizations
    rootTypeNames = if isNothing _rscRootFieldsNamespace
                    then catMaybes [Just irQueryRoot, irMutationRoot, irSubscriptionRoot]
                    else []
                    -- root type names should not be prefixed or suffixed unless
                    -- there is a custom root namespace field
    scalarTypeNames = [intScalar, floatScalar, stringScalar, boolScalar, idScalar]
    protectedTypeNames = scalarTypeNames ++ rootTypeNames
    nameFilter name = not $ "__" `T.isPrefixOf` G.unName name || name `elem` protectedTypeNames

    mkPrefixSuffixMap :: Maybe G.Name -> Maybe G.Name -> [G.Name] -> HashMap G.Name G.Name
    mkPrefixSuffixMap mPrefix mSuffix names = Map.fromList $ case (mPrefix, mSuffix) of
      (Nothing, Nothing)         -> []
      (Just prefix, Nothing)     -> map (\name -> (name, prefix <> name)) names
      (Nothing, Just suffix)     -> map (\name -> (name, name <> suffix)) names
      (Just prefix, Just suffix) -> map (\name -> (name, prefix <> name <> suffix)) names

    RemoteSchemaIntrospection typeDefinitions = irDoc
    typesToRename = filter nameFilter $ typeDefinitionName <$> typeDefinitions
    typeRenameMap =
      case _rscTypeNames of
        Nothing -> Map.empty
        Just RemoteTypeCustomization{..} ->
          _rtcMapping <> mkPrefixSuffixMap _rtcPrefix _rtcSuffix typesToRename

    typeFieldMap :: HashMap G.Name [G.Name] -- typeName -> fieldNames
    typeFieldMap = Map.fromList $ typeDefinitions >>= \case
      G.TypeDefinitionObject G.ObjectTypeDefinition{..} -> pure (_otdName, G._fldName <$> _otdFieldsDefinition)
      G.TypeDefinitionInterface G.InterfaceTypeDefinition{..} -> pure (_itdName, G._fldName <$> _itdFieldsDefinition)
      _ -> []

    mkFieldRenameMap RemoteFieldCustomization{..} fieldNames =
      _rfcMapping <> mkPrefixSuffixMap _rfcPrefix _rfcSuffix fieldNames

    fieldRenameMap =
      case _rscFieldNames of
        Nothing -> Map.empty
        Just fieldNameCustomizations ->
          let customizationMap = Map.fromList $ map (\rfc -> (_rfcParentType rfc, rfc)) fieldNameCustomizations
          in Map.intersectionWith mkFieldRenameMap customizationMap typeFieldMap

    mapLookup :: (Eq a, Hashable a) => HashMap a a -> a -> a
    mapLookup m a = fromMaybe a $ Map.lookup a m

    _rscNamespaceFieldName = _rscRootFieldsNamespace
    _rscCustomizeTypeName = typeRenameMap
    _rscCustomizeFieldName = fieldRenameMap
    _rscDecustomizeTypeName = invertMap typeRenameMap
    _rscDecustomizeFieldName = mapMap (mapLookup typeRenameMap *** invertMap) fieldRenameMap

customizeIntrospectionResult :: RemoteSchemaCustomizer -> IntrospectionResult -> IntrospectionResult
customizeIntrospectionResult remoteSchemaCustomizer IntrospectionResult{..} = IntrospectionResult
    { irDoc = customizeRemoteSchemaIntrospection irDoc
    , irQueryRoot = customizedQueryRoot
    , irMutationRoot = customizedMutationRoot
    , irSubscriptionRoot = customizedSubscriptionRoot
    }
  where

    namespaceField = _rscNamespaceFieldName remoteSchemaCustomizer
    customizeTypeName = remoteSchemaCustomizeTypeName remoteSchemaCustomizer
    customizeFieldName = remoteSchemaCustomizeFieldName remoteSchemaCustomizer

    -- Create customized root type names by appending "Query", "Mutation" or "Subscription" to the custom namespace field name
    customizeRootTypeName suffix = maybe id (const . (<> suffix)) namespaceField
    customizedQueryRoot = customizeRootTypeName $$(G.litName "Query") irQueryRoot
    customizedMutationRoot = customizeRootTypeName $$(G.litName "Mutation") <$> irMutationRoot
    customizedSubscriptionRoot = customizeRootTypeName $$(G.litName "Subscription") <$> irSubscriptionRoot

    -- Create object type definitions for each of the custom namespace root types.
    -- Each object type has a single field where the field name is
    -- the custom namespace and the type is the original root type.
    namespaceRootTypeDefinitions = case namespaceField of
      Nothing -> []
      Just namespaceFieldName ->
        let mkNamespaceTypeDef originalRootTypeName customizedRootTypeName =
              G.TypeDefinitionObject $ G.ObjectTypeDefinition (Just "custom namespace root type") customizedRootTypeName [] []
                [G.FieldDefinition (Just "custom namespace field") namespaceFieldName []
                  (G.TypeNamed (G.Nullability True) $ customizeTypeName originalRootTypeName) []]
        in catMaybes
            [ pure $ mkNamespaceTypeDef irQueryRoot customizedQueryRoot
            , mkNamespaceTypeDef <$> irMutationRoot <*> customizedMutationRoot
            , mkNamespaceTypeDef <$> irSubscriptionRoot <*> customizedSubscriptionRoot
            ]

    customizeRemoteSchemaIntrospection :: RemoteSchemaIntrospection -> RemoteSchemaIntrospection
    customizeRemoteSchemaIntrospection (RemoteSchemaIntrospection typeDefinitions) =
      RemoteSchemaIntrospection $ namespaceRootTypeDefinitions ++ customizeTypeDefinitions typeDefinitions
      where
        customizeTypeDefinitions =
          if hasTypeOrFieldCustomizations remoteSchemaCustomizer
            then fmap customizeTypeDefinition
            else id -- no need to traverse the schema if there are no type or field name customizations

    customizeTypeDefinition :: G.TypeDefinition [G.Name] RemoteSchemaInputValueDefinition -> G.TypeDefinition [G.Name] RemoteSchemaInputValueDefinition
    customizeTypeDefinition = \case
      G.TypeDefinitionScalar scalarTypeDefinition -> G.TypeDefinitionScalar $ customizeScalarTypeDefinition scalarTypeDefinition
      G.TypeDefinitionObject objectTypeDefinition -> G.TypeDefinitionObject $ customizeObjectTypeDefinition objectTypeDefinition
      G.TypeDefinitionInterface interfaceTypeDefinition -> G.TypeDefinitionInterface $ customizeInterfaceTypeDefinition interfaceTypeDefinition
      G.TypeDefinitionUnion unionTypeDefinition -> G.TypeDefinitionUnion $ customizeUnionTypeDefinition unionTypeDefinition
      G.TypeDefinitionEnum enumTypeDefinition -> G.TypeDefinitionEnum $ customizeEnumTypeDefinition enumTypeDefinition
      G.TypeDefinitionInputObject inputObjectTypeDefinition -> G.TypeDefinitionInputObject $ customizeInputObjectTypeDefinition inputObjectTypeDefinition

    customizeScalarTypeDefinition :: G.ScalarTypeDefinition  -> G.ScalarTypeDefinition
    customizeScalarTypeDefinition G.ScalarTypeDefinition{..} =
      G.ScalarTypeDefinition { _stdName = customizeTypeName _stdName, ..}

    customizeObjectTypeDefinition :: G.ObjectTypeDefinition RemoteSchemaInputValueDefinition -> G.ObjectTypeDefinition RemoteSchemaInputValueDefinition
    customizeObjectTypeDefinition G.ObjectTypeDefinition{..} =
      G.ObjectTypeDefinition
      { _otdName = customizeTypeName _otdName
      , _otdImplementsInterfaces = customizeTypeName <$> _otdImplementsInterfaces
      , _otdFieldsDefinition = customizeFieldDefinition (customizeFieldName _otdName) <$> _otdFieldsDefinition
      , ..
      }

    customizeType :: G.GType -> G.GType
    customizeType = \case
      G.TypeNamed nullability name -> G.TypeNamed nullability $ customizeTypeName name
      G.TypeList nullability gtype -> G.TypeList nullability $ customizeType gtype

    customizeFieldDefinition :: (G.Name -> G.Name) -> G.FieldDefinition RemoteSchemaInputValueDefinition -> G.FieldDefinition RemoteSchemaInputValueDefinition
    customizeFieldDefinition customizeFieldName' G.FieldDefinition{..} =
      G.FieldDefinition
      { _fldName = customizeFieldName' _fldName
      , _fldType = customizeType _fldType
      , _fldArgumentsDefinition = customizeRemoteSchemaInputValueDefinition <$> _fldArgumentsDefinition
      , ..
      }

    customizeRemoteSchemaInputValueDefinition :: RemoteSchemaInputValueDefinition -> RemoteSchemaInputValueDefinition
    customizeRemoteSchemaInputValueDefinition RemoteSchemaInputValueDefinition{..} =
      RemoteSchemaInputValueDefinition
      { _rsitdDefinition = customizeInputValueDefinition _rsitdDefinition
      , ..
      }

    customizeInputValueDefinition :: G.InputValueDefinition -> G.InputValueDefinition
    customizeInputValueDefinition G.InputValueDefinition{..} =
      G.InputValueDefinition
      { _ivdType = customizeType _ivdType
      , ..
      }

    customizeInterfaceTypeDefinition :: G.InterfaceTypeDefinition [G.Name] RemoteSchemaInputValueDefinition -> G.InterfaceTypeDefinition [G.Name] RemoteSchemaInputValueDefinition
    customizeInterfaceTypeDefinition G.InterfaceTypeDefinition{..} =
      G.InterfaceTypeDefinition
      { _itdName = customizeTypeName _itdName
      , _itdFieldsDefinition = customizeFieldDefinition (customizeFieldName _itdName) <$> _itdFieldsDefinition
      , _itdPossibleTypes = customizeTypeName <$> _itdPossibleTypes
      , ..
      }

    customizeUnionTypeDefinition :: G.UnionTypeDefinition -> G.UnionTypeDefinition
    customizeUnionTypeDefinition G.UnionTypeDefinition{..} =
      G.UnionTypeDefinition
      { _utdName = customizeTypeName _utdName
      , _utdMemberTypes = customizeTypeName <$> _utdMemberTypes
      , ..
      }

    customizeEnumTypeDefinition :: G.EnumTypeDefinition  -> G.EnumTypeDefinition
    customizeEnumTypeDefinition G.EnumTypeDefinition{..} =
      G.EnumTypeDefinition { _etdName = customizeTypeName _etdName, ..}

    customizeInputObjectTypeDefinition :: G.InputObjectTypeDefinition RemoteSchemaInputValueDefinition -> G.InputObjectTypeDefinition RemoteSchemaInputValueDefinition
    customizeInputObjectTypeDefinition G.InputObjectTypeDefinition{..} =
      G.InputObjectTypeDefinition
      { _iotdName = customizeTypeName _iotdName
      , _iotdValueDefinitions = customizeRemoteSchemaInputValueDefinition <$> _iotdValueDefinitions
      , ..
      }

throwRemoteSchema
  :: QErrM m
  => Text -> m a
throwRemoteSchema = throw400 RemoteSchemaError

throwRemoteSchemaWithInternal
  :: (QErrM m, J.ToJSON a)
  => Text -> a -> m b
throwRemoteSchemaWithInternal msg v =
  let err = err400 RemoteSchemaError msg
  in throwError err{qeInternal = Just $ J.toJSON v}

throwRemoteSchemaHttp
  :: QErrM m
  => URI -> HTTP.HttpException -> m a
throwRemoteSchemaHttp url =
  throwRemoteSchemaWithInternal (T.pack httpExceptMsg) . httpExceptToJSON
  where
    httpExceptMsg =
      "HTTP exception occurred while sending the request to " <> show url
