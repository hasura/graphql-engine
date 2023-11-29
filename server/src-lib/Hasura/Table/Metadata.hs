{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | In order to avoid circular dependencies while splitting
-- 'Hasura.RQL.Types.Metadata' into multiple modules, some definitions must be
-- moved out of that module. This module is the bucket for definitions that have
-- not been specifically moved elsewhere.
module Hasura.Table.Metadata
  ( ComputedFieldMetadata (..),
    ComputedFields,
    Permissions,
    Relationships,
    TableMetadata (..),
    mkTableMeta,
    tmArrayRelationships,
    tmComputedFields,
    tmConfiguration,
    tmDeletePermissions,
    tmApolloFederationConfig,
    tmEventTriggers,
    tmInsertPermissions,
    tmIsEnum,
    tmLogicalModel,
    tmObjectRelationships,
    tmRemoteRelationships,
    tmSelectPermissions,
    tmTable,
    tmUpdatePermissions,
  )
where

import Autodocodec hiding (object, (.=))
import Autodocodec qualified as AC
import Control.Lens hiding (set, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types
import Data.HashMap.Strict.InsOrd.Autodocodec (sortedElemsCodec)
import Data.HashSet qualified as HS
import Data.List.Extended qualified as L
import Data.Text qualified as T
import Data.Text.Extended qualified as T
import Hasura.LogicalModel.Types
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendTag (backendPrefix)
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.EventTrigger
import Hasura.RQL.Types.Permission
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.Relationships.Remote
import Hasura.RQL.Types.Roles
import Hasura.Table.Cache (TableConfig (..), emptyTableConfig)

-- | Parse a list of objects into a map from a derived key,
-- failing if the list has duplicates.
parseListAsMap ::
  (Hashable k, T.ToTxt k) =>
  Text ->
  (a -> k) ->
  Parser [a] ->
  Parser (InsOrdHashMap k a)
parseListAsMap things mapFn listP = do
  list <- listP
  let duplicates = toList $ L.duplicates $ map mapFn list
  unless (null duplicates)
    $ fail
    $ T.unpack
    $ "multiple declarations exist for the following "
    <> things
    <> ": "
    <> T.commaSeparated duplicates
  pure $ oMapFromL mapFn list

data ComputedFieldMetadata b = ComputedFieldMetadata
  { _cfmName :: ComputedFieldName,
    _cfmDefinition :: ComputedFieldDefinition b,
    _cfmComment :: Comment
  }
  deriving (Generic)

deriving instance (Backend b) => Show (ComputedFieldMetadata b)

deriving instance (Backend b) => Eq (ComputedFieldMetadata b)

instance (Backend b) => HasCodec (ComputedFieldMetadata b) where
  codec =
    AC.object (backendPrefix @b <> "ComputedFieldMetadata")
      $ ComputedFieldMetadata
      <$> requiredField' "name"
      AC..= _cfmName
        <*> requiredField' "definition"
      AC..= _cfmDefinition
        <*> optionalFieldWithOmittedDefault' "comment" Automatic
      AC..= _cfmComment

instance (Backend b) => ToJSON (ComputedFieldMetadata b) where
  toJSON ComputedFieldMetadata {..} =
    object
      $ [ "name" .= _cfmName,
          "definition" .= _cfmDefinition,
          "comment" .= _cfmComment
        ]

instance (Backend b) => FromJSON (ComputedFieldMetadata b) where
  parseJSON = withObject "ComputedFieldMetadata" $ \obj ->
    ComputedFieldMetadata
      <$> obj
      .: "name"
      <*> obj
      .: "definition"
      <*> obj
      .:? "comment"
      .!= Automatic

type Relationships a = InsOrdHashMap RelName a

type ComputedFields b = InsOrdHashMap ComputedFieldName (ComputedFieldMetadata b)

type RemoteRelationships = InsOrdHashMap RelName RemoteRelationship

type Permissions a = InsOrdHashMap RoleName a

type EventTriggers b = InsOrdHashMap TriggerName (EventTriggerConf b)

data TableMetadata b = TableMetadata
  { _tmTable :: TableName b,
    _tmIsEnum :: Bool,
    _tmConfiguration :: TableConfig b,
    _tmObjectRelationships :: Relationships (ObjRelDef b),
    _tmArrayRelationships :: Relationships (ArrRelDef b),
    _tmComputedFields :: ComputedFields b,
    _tmRemoteRelationships :: RemoteRelationships,
    _tmInsertPermissions :: Permissions (InsPermDef b),
    _tmSelectPermissions :: Permissions (SelPermDef b),
    _tmUpdatePermissions :: Permissions (UpdPermDef b),
    _tmDeletePermissions :: Permissions (DelPermDef b),
    _tmEventTriggers :: EventTriggers b,
    _tmApolloFederationConfig :: Maybe ApolloFederationConfig,
    _tmLogicalModel :: Maybe LogicalModelName
  }
  deriving (Generic)

deriving instance (Backend b) => Show (TableMetadata b)

deriving instance (Backend b) => Eq (TableMetadata b)

instance (Backend b) => ToJSON (TableMetadata b) where
  toJSON = genericToJSON hasuraJSON

instance (Backend b) => HasCodec (TableMetadata b) where
  codec =
    CommentCodec "Representation of a table in metadata, 'tables.yaml' and 'metadata.json'"
      $ AC.object (backendPrefix @b <> "TableMetadata")
      $ TableMetadata
      <$> requiredField' "table"
      .== _tmTable
        <*> optionalFieldWithOmittedDefault' "is_enum" False
      .== _tmIsEnum
        <*> optionalFieldWithOmittedDefault "configuration" emptyTableConfig configDoc
      .== _tmConfiguration
        <*> optSortedList "object_relationships" _rdName
      .== _tmObjectRelationships
        <*> optSortedList "array_relationships" _rdName
      .== _tmArrayRelationships
        <*> optSortedList "computed_fields" _cfmName
      .== _tmComputedFields
        <*> optSortedList "remote_relationships" _rrName
      .== _tmRemoteRelationships
        <*> optSortedList "insert_permissions" _pdRole
      .== _tmInsertPermissions
        <*> optSortedList "select_permissions" _pdRole
      .== _tmSelectPermissions
        <*> optSortedList "update_permissions" _pdRole
      .== _tmUpdatePermissions
        <*> optSortedList "delete_permissions" _pdRole
      .== _tmDeletePermissions
        <*> eventTriggers
        <*> optionalFieldOrNull' "apollo_federation_config"
      .== _tmApolloFederationConfig
        <*> optionalFieldOrNull' "logical_model"
      .== _tmLogicalModel
    where
      -- Some backends do not implement event triggers. In those cases we tailor
      -- the codec to omit the @"event_triggers"@ field from the API.
      eventTriggers = case defaultTriggerOnReplication @b of
        Just _ -> optSortedList "event_triggers" etcName .== _tmEventTriggers
        Nothing -> pure mempty

      optSortedList ::
        (HasCodec a, Eq a, Hashable k, Ord k, T.ToTxt k) =>
        Text ->
        (a -> k) ->
        ObjectCodec (InsOrdHashMap k a) (InsOrdHashMap k a)
      optSortedList name keyForElem =
        AC.optionalFieldWithOmittedDefaultWith' name (sortedElemsCodec keyForElem) mempty

      configDoc =
        T.unlines
          [ "Configuration for the table/view",
            "",
            "https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/table-view.html#table-config"
          ]

      (.==) = (AC..=)

$(makeLenses ''TableMetadata)

mkTableMeta :: TableName b -> Bool -> TableConfig b -> TableMetadata b
mkTableMeta qt isEnum config =
  TableMetadata
    qt
    isEnum
    config
    mempty
    mempty
    mempty
    mempty
    mempty
    mempty
    mempty
    mempty
    mempty
    Nothing
    Nothing

instance (Backend b) => FromJSON (TableMetadata b) where
  parseJSON = withObject "Object" $ \o -> do
    let unexpectedKeys = getUnexpectedKeys o
    unless (null unexpectedKeys)
      $ fail
      $ "unexpected keys when parsing TableMetadata: "
      <> show (HS.toList unexpectedKeys)

    TableMetadata
      <$> o
      .: tableKey
      <*> o
      .:? isEnumKey
      .!= False
      <*> o
      .:? configKey
      .!= emptyTableConfig
      <*> parseListAsMap "object relationships" _rdName (o .:? orKey .!= [])
      <*> parseListAsMap "array relationships" _rdName (o .:? arKey .!= [])
      <*> parseListAsMap "computed fields" _cfmName (o .:? cfKey .!= [])
      <*> parseListAsMap "remote relationships" _rrName (o .:? rrKey .!= [])
      <*> parseListAsMap "insert permissions" _pdRole (o .:? ipKey .!= [])
      <*> parseListAsMap "select permissions" _pdRole (o .:? spKey .!= [])
      <*> parseListAsMap "update permissions" _pdRole (o .:? upKey .!= [])
      <*> parseListAsMap "delete permissions" _pdRole (o .:? dpKey .!= [])
      <*> parseListAsMap "event triggers" etcName (o .:? etKey .!= [])
      <*> o
      .:? enableAFKey
      <*> o
      .:? logicalModelKey
    where
      tableKey = "table"
      isEnumKey = "is_enum"
      configKey = "configuration"
      orKey = "object_relationships"
      arKey = "array_relationships"
      ipKey = "insert_permissions"
      spKey = "select_permissions"
      upKey = "update_permissions"
      dpKey = "delete_permissions"
      etKey = "event_triggers"
      cfKey = "computed_fields"
      rrKey = "remote_relationships"
      enableAFKey = "apollo_federation_config"
      logicalModelKey = "logical_model"

      getUnexpectedKeys o =
        HS.fromList (KM.keys o) `HS.difference` expectedKeySet

      expectedKeySet =
        HS.fromList
          [ tableKey,
            isEnumKey,
            configKey,
            orKey,
            arKey,
            ipKey,
            spKey,
            upKey,
            dpKey,
            etKey,
            cfKey,
            rrKey,
            enableAFKey,
            logicalModelKey
          ]
