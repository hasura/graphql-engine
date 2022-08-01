{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.Types.SourceCustomization
  ( SourceTypeCustomization,
    RootFieldsCustomization (..),
    mkCustomizedTypename,
    emptySourceCustomization,
    getSourceTypeCustomization,
    getRootFieldsCustomization,
    mkRootFieldName,
    SourceCustomization (..),
    withSourceCustomization,
    MkRootFieldName,
    CustomizeRemoteFieldName (..),
    withRemoteFieldNameCustomization,

    -- * Naming Convention specific
    applyEnumValueCase,
    applyFieldNameCaseCust,
    applyTypeNameCaseCust,
    applyFieldNameCaseIdentifier,
    applyTypeNameCaseIdentifier,
    getNamingConvention,
    getTextFieldName,
    getTextTypeName,

    -- * Field name builders
    mkSelectField,
    mkSelectAggregateField,
    mkSelectByPkField,
    mkSelectStreamField,
    mkInsertField,
    mkInsertOneField,
    mkUpdateField,
    mkUpdateByPkField,
    mkUpdateManyField,
    mkDeleteField,
    mkDeleteByPkField,
    mkRelayConnectionField,
  )
where

import Control.Lens
import Data.Aeson.Extended
import Data.Has
import Data.List.NonEmpty qualified as NE
import Data.Monoid
import Data.Text qualified as T
import Data.Text.Casing (GQLNameIdentifier (..))
import Data.Text.Casing qualified as C
import Hasura.Base.Error (Code (NotSupported), QErr, throw400)
import Hasura.GraphQL.Schema.NamingCase
import Hasura.GraphQL.Schema.Typename
import Hasura.Incremental.Internal.Dependency (Cacheable)
import Hasura.Name qualified as Name
import Hasura.Prelude
import Hasura.RQL.Types.Backend (SupportedNamingCase (..))
import Hasura.RQL.Types.Instances ()
import Language.GraphQL.Draft.Syntax qualified as G

data RootFieldsCustomization = RootFieldsCustomization
  { _rootfcNamespace :: Maybe G.Name,
    _rootfcPrefix :: Maybe G.Name,
    _rootfcSuffix :: Maybe G.Name
  }
  deriving (Eq, Show, Generic)

instance Cacheable RootFieldsCustomization

instance ToJSON RootFieldsCustomization where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}

instance FromJSON RootFieldsCustomization where
  parseJSON = genericParseJSON hasuraJSON

emptyRootFieldsCustomization :: RootFieldsCustomization
emptyRootFieldsCustomization = RootFieldsCustomization Nothing Nothing Nothing

data SourceTypeCustomization = SourceTypeCustomization
  { _stcPrefix :: Maybe G.Name,
    _stcSuffix :: Maybe G.Name
  }
  deriving (Eq, Show, Generic)

instance Cacheable SourceTypeCustomization

instance ToJSON SourceTypeCustomization where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}

instance FromJSON SourceTypeCustomization where
  parseJSON = genericParseJSON hasuraJSON

emptySourceTypeCustomization :: SourceTypeCustomization
emptySourceTypeCustomization = SourceTypeCustomization Nothing Nothing

mkCustomizedTypename :: Maybe SourceTypeCustomization -> NamingCase -> MkTypename
mkCustomizedTypename stc tCase = MkTypename ((applyTypeNameCaseCust tCase) . (applyTypeCust stc tCase))

mkCustomizedFieldName :: Maybe RootFieldsCustomization -> NamingCase -> MkRootFieldName
mkCustomizedFieldName rtc tCase = MkRootFieldName (applyFieldCust rtc tCase)

-- | apply prefix and suffix to type name according to the source customization
applyTypeCust :: Maybe SourceTypeCustomization -> NamingCase -> (G.Name -> G.Name)
applyTypeCust Nothing _ = id
applyTypeCust (Just SourceTypeCustomization {..}) tCase = applyPrefixSuffix _stcPrefix _stcSuffix tCase True

-- | apply prefix and suffix to field name according to the source customization
applyFieldCust :: Maybe RootFieldsCustomization -> NamingCase -> (G.Name -> G.Name)
applyFieldCust Nothing _ = id
applyFieldCust (Just RootFieldsCustomization {..}) tCase = applyPrefixSuffix _rootfcPrefix _rootfcSuffix tCase False

-- | apply naming convention to type name
applyTypeNameCaseCust :: NamingCase -> G.Name -> G.Name
applyTypeNameCaseCust tCase name = case tCase of
  HasuraCase -> name
  GraphqlCase -> C.transformNameWith (C.snakeToPascal) name

-- | apply naming convention to field name
applyFieldNameCaseCust :: NamingCase -> G.Name -> G.Name
applyFieldNameCaseCust tCase name = case tCase of
  HasuraCase -> name
  GraphqlCase -> C.transformNameWith (C.snakeToCamel) name

-- | returns field name according to the naming conventions as @Text@
getTextFieldName :: NamingCase -> GQLNameIdentifier -> Text
getTextFieldName tCase nameLst' = case tCase of
  HasuraCase -> C.toSnakeT nameLst
  GraphqlCase -> C.toCamelT nameLst
  where
    nameLst = C.identifierToList nameLst'

-- | applies naming convention and returns field name
--
--  Note: This can't possibly fail as @GQLNameIdentifier@ contains already
--  validated identifiers
applyTypeNameCaseIdentifier :: NamingCase -> GQLNameIdentifier -> G.Name
applyTypeNameCaseIdentifier tCase nameLst = case tCase of
  HasuraCase -> C.toSnakeG nameLst
  GraphqlCase -> C.toPascalG nameLst

-- | returns type name according to the naming conventions as @Text@
getTextTypeName :: NamingCase -> GQLNameIdentifier -> Text
getTextTypeName tCase nameLst' = case tCase of
  HasuraCase -> C.toSnakeT nameLst
  GraphqlCase -> C.toPascalT nameLst
  where
    nameLst = C.identifierToList nameLst'

-- | applies naming convention and returns type name
--
--  Note: This can't possibly fail as @GQLNameIdentifier@ contains already
--  validated identifiers
applyFieldNameCaseIdentifier :: NamingCase -> GQLNameIdentifier -> G.Name
applyFieldNameCaseIdentifier tCase nameLst = case tCase of
  HasuraCase -> C.toSnakeG nameLst
  GraphqlCase -> C.toCamelG nameLst

applyEnumValueCase :: NamingCase -> G.Name -> G.Name
applyEnumValueCase tCase v = case tCase of
  HasuraCase -> v
  GraphqlCase -> C.transformNameWith (T.toUpper) v

-- | append/prepend the suffix/prefix in the graphql name
applyPrefixSuffix :: Maybe G.Name -> Maybe G.Name -> NamingCase -> Bool -> G.Name -> G.Name
applyPrefixSuffix Nothing Nothing tCase isTypeName name = concatPrefixSuffix tCase isTypeName $ NE.fromList [name]
applyPrefixSuffix (Just prefix) Nothing tCase isTypeName name = concatPrefixSuffix tCase isTypeName $ NE.fromList [prefix, name]
applyPrefixSuffix Nothing (Just suffix) tCase isTypeName name = concatPrefixSuffix tCase isTypeName $ NE.fromList [name, suffix]
applyPrefixSuffix (Just prefix) (Just suffix) tCase isTypeName name = concatPrefixSuffix tCase isTypeName $ NE.fromList [prefix, name, suffix]

concatPrefixSuffix :: NamingCase -> Bool -> NonEmpty G.Name -> G.Name
concatPrefixSuffix (HasuraCase) _ neList = sconcat neList
concatPrefixSuffix (GraphqlCase) isTypeName neList =
  if isTypeName
    then C.toPascalG prefixSuffixGQLIdent
    else C.transformPrefixAndSuffixAndConcat prefixSuffixGQLIdent id C.upperFirstChar
  where
    prefixSuffixGQLIdent = C.fromNonEmptyList neList

data SourceCustomization = SourceCustomization
  { _scRootFields :: Maybe RootFieldsCustomization,
    _scTypeNames :: Maybe SourceTypeCustomization,
    _scNamingConvention :: Maybe NamingCase
  }
  deriving (Eq, Show, Generic)

instance Cacheable SourceCustomization

instance ToJSON SourceCustomization where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}

instance FromJSON SourceCustomization where
  parseJSON = genericParseJSON hasuraJSON

emptySourceCustomization :: SourceCustomization
emptySourceCustomization = SourceCustomization Nothing Nothing Nothing

getRootFieldsCustomization :: SourceCustomization -> RootFieldsCustomization
getRootFieldsCustomization = fromMaybe emptyRootFieldsCustomization . _scRootFields

getSourceTypeCustomization :: SourceCustomization -> SourceTypeCustomization
getSourceTypeCustomization = fromMaybe emptySourceTypeCustomization . _scTypeNames

getNamingConvention :: SourceCustomization -> Maybe NamingCase -> NamingCase
getNamingConvention sc defaultNC = defaultNC `seq` fromMaybe HasuraCase $ _scNamingConvention sc <|> defaultNC

-- | Function to apply root field name customizations.
newtype MkRootFieldName = MkRootFieldName {runMkRootFieldName :: G.Name -> G.Name}
  deriving (Semigroup, Monoid) via (Endo G.Name)

-- | Inject a new root field name customization function into the environment.
-- This can be used by schema-building code (with @MonadBuildSchema@ constraint) to ensure
-- the correct root field name customizations are applied.
withRootFieldNameCustomization :: forall m r a. (MonadReader r m, Has MkRootFieldName r) => MkRootFieldName -> m a -> m a
withRootFieldNameCustomization = local . set hasLens

-- | Apply the root field name customization function from the current environment.
mkRootFieldName :: (MonadReader r m, Has MkRootFieldName r) => G.Name -> m G.Name
mkRootFieldName name =
  ($ name) . runMkRootFieldName <$> asks getter

-- | Inject NamingCase, typename and root field name customizations from @SourceCustomization@ into
-- the environment.
withSourceCustomization ::
  forall m r a.
  (MonadReader r m, Has MkTypename r, Has MkRootFieldName r, Has NamingCase r, MonadError QErr m) =>
  SourceCustomization ->
  SupportedNamingCase ->
  Maybe NamingCase ->
  m a ->
  m a
withSourceCustomization sc@SourceCustomization {..} namingConventionSupport defaultNC m = do
  let namingConv = getNamingConvention sc defaultNC
  -- The console currently constructs a graphql query based on table name and
  -- schema name to fetch the data from the database (other than postgres).
  -- Now, when we set @GraphqlCase@ for other (than postgres) databases, this
  -- custom query constructed by console won't work (because the field names
  -- has changed) and thus the data explorer tab won't work properly. So, we
  -- have restricted this feature to postgres for now.
  tCase <-
    case namingConventionSupport of
      AllConventions -> pure namingConv
      OnlyHasuraCase -> case namingConv of
        GraphqlCase -> throw400 NotSupported $ "sources other than postgres do not support graphql-default as naming convention yet"
        HasuraCase -> pure HasuraCase

  withTypenameCustomization (mkCustomizedTypename _scTypeNames tCase)
    . withRootFieldNameCustomization (mkCustomizedFieldName _scRootFields tCase)
    . withNamingCaseCustomization tCase
    $ m

withNamingCaseCustomization :: forall m r a. (MonadReader r m, Has NamingCase r) => NamingCase -> m a -> m a
withNamingCaseCustomization = local . set hasLens

newtype CustomizeRemoteFieldName = CustomizeRemoteFieldName
  { runCustomizeRemoteFieldName :: G.Name -> G.Name -> G.Name
  }
  deriving (Semigroup, Monoid) via (G.Name -> Endo G.Name)

withRemoteFieldNameCustomization :: forall m r a. (MonadReader r m, Has CustomizeRemoteFieldName r) => CustomizeRemoteFieldName -> m a -> m a
withRemoteFieldNameCustomization = local . set hasLens

-------------------------------------------------------------------------------
-- Some helper functions to build the field names as an identifier

mkSelectField :: GQLNameIdentifier -> GQLNameIdentifier
mkSelectField = id

mkSelectAggregateField :: GQLNameIdentifier -> GQLNameIdentifier
mkSelectAggregateField name = name <> C.fromName Name._aggregate

mkSelectByPkField :: GQLNameIdentifier -> GQLNameIdentifier
mkSelectByPkField name = name <> C.fromTuple $$(G.litGQLIdentifier ["by", "pk"])

mkInsertField :: GQLNameIdentifier -> GQLNameIdentifier
mkInsertField name = C.fromName Name._insert <> name

mkInsertOneField :: GQLNameIdentifier -> GQLNameIdentifier
mkInsertOneField name = C.fromName Name._insert <> name <> C.fromName Name._one

mkUpdateField :: GQLNameIdentifier -> GQLNameIdentifier
mkUpdateField name = C.fromName Name._update <> name

mkUpdateByPkField :: GQLNameIdentifier -> GQLNameIdentifier
mkUpdateByPkField name = C.fromName Name._update <> name <> C.fromTuple $$(G.litGQLIdentifier ["by", "pk"])

mkUpdateManyField :: GQLNameIdentifier -> GQLNameIdentifier
mkUpdateManyField name = C.fromName Name._update <> name <> C.fromName Name._many

mkDeleteField :: GQLNameIdentifier -> GQLNameIdentifier
mkDeleteField name = C.fromName Name._delete <> name

mkDeleteByPkField :: GQLNameIdentifier -> GQLNameIdentifier
mkDeleteByPkField name = C.fromName Name._delete <> name <> C.fromTuple $$(G.litGQLIdentifier ["by", "pk"])

mkRelayConnectionField :: GQLNameIdentifier -> GQLNameIdentifier
mkRelayConnectionField name = name <> C.fromName Name._connection

mkSelectStreamField :: GQLNameIdentifier -> GQLNameIdentifier
mkSelectStreamField name = name <> C.fromName Name._stream
