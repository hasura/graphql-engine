{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.Types.Permission
  ( DelPerm (..),
    DelPermDef,
    InsPerm (..),
    InsPermDef,
    ValidateInput (..),
    ValidateInputHttpDefinition (..),
    PermColSpec (..),
    PermDef (..),
    PermType (..),
    SelPerm (..),
    SelPermDef,
    UpdPerm (..),
    UpdPermDef,
    pdComment,
    pdPermission,
    pdRole,
    permTypeToCode,
    PermDefPermission (..),
    unPermDefPermission,
    reflectPermDefPermission,
    SubscriptionRootFieldType (..),
    QueryRootFieldType (..),
    AllowedRootFields (..),
    isRootFieldAllowed,
  )
where

import Autodocodec hiding (object, (.=))
import Autodocodec qualified as AC
import Autodocodec.Extended (optionalFieldOrIncludedNull', typeableName)
import Control.Lens (makeLenses)
import Data.Aeson
import Data.Aeson.Casing (snakeCase)
import Data.HashSet qualified as Set
import Data.Hashable
import Data.Kind (Type)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as T
import Data.Typeable (Typeable)
import Database.PG.Query qualified as PG
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendTag (backendPrefix)
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.Headers
import Hasura.RQL.Types.Roles (RoleName)
import PostgreSQL.Binary.Decoding qualified as PD

data PermType
  = PTInsert
  | PTSelect
  | PTUpdate
  | PTDelete
  deriving (Eq, Ord, Generic)

instance NFData PermType

instance Hashable PermType

instance PG.FromCol PermType where
  fromCol bs = flip PG.fromColHelper bs
    $ PD.enum
    $ \case
      "insert" -> Just PTInsert
      "update" -> Just PTUpdate
      "select" -> Just PTSelect
      "delete" -> Just PTDelete
      _ -> Nothing

permTypeToCode :: PermType -> Text
permTypeToCode = tshow

instance Show PermType where
  show PTInsert = "insert"
  show PTSelect = "select"
  show PTUpdate = "update"
  show PTDelete = "delete"

instance FromJSON PermType where
  parseJSON (String "insert") = return PTInsert
  parseJSON (String "select") = return PTSelect
  parseJSON (String "update") = return PTUpdate
  parseJSON (String "delete") = return PTDelete
  parseJSON _ =
    fail "perm_type should be one of 'insert', 'select', 'update', 'delete'"

instance ToJSON PermType where
  toJSON = String . permTypeToCode

data PermColSpec b
  = PCStar
  | PCCols [Column b]
  deriving (Generic)

deriving instance (Backend b) => Show (PermColSpec b)

deriving instance (Backend b) => Eq (PermColSpec b)

instance (Backend b) => HasCodec (PermColSpec b) where
  codec =
    dimapCodec
      (either (const PCStar) PCCols)
      (\case PCStar -> Left "*"; PCCols cols -> Right cols)
      $ disjointEitherCodec (literalTextCodec "*") (listCodec $ codec @(Column b))

instance (Backend b) => FromJSON (PermColSpec b) where
  parseJSON (String "*") = return PCStar
  parseJSON x = PCCols <$> parseJSON x

instance (Backend b) => ToJSON (PermColSpec b) where
  toJSON (PCCols cols) = toJSON cols
  toJSON PCStar = "*"

data PermDef (b :: BackendType) (perm :: BackendType -> Type) = PermDef
  { _pdRole :: RoleName,
    _pdPermission :: PermDefPermission b perm,
    _pdComment :: Maybe T.Text
  }
  deriving (Show, Eq, Generic)

-- | The permission data as it appears in a 'PermDef'.
-- Since this type is a GADT it facilitates that values which are polymorphic
-- may re-discover its specific type of permission by case analysis.
--
-- The fact that permission types are tracked in types are more accidental than
-- intentional and something we want to move away from, see
-- https://github.com/hasura/graphql-engine-mono/issues/4076.
data PermDefPermission (b :: BackendType) (perm :: BackendType -> Type) where
  SelPerm' :: SelPerm b -> PermDefPermission b SelPerm
  InsPerm' :: InsPerm b -> PermDefPermission b InsPerm
  UpdPerm' :: UpdPerm b -> PermDefPermission b UpdPerm
  DelPerm' :: DelPerm b -> PermDefPermission b DelPerm

instance (Backend b) => FromJSON (PermDefPermission b SelPerm) where
  parseJSON = fmap SelPerm' . parseJSON

instance (Backend b) => FromJSON (PermDefPermission b InsPerm) where
  parseJSON = fmap InsPerm' . parseJSON

instance (Backend b) => FromJSON (PermDefPermission b UpdPerm) where
  parseJSON = fmap UpdPerm' . parseJSON

instance (Backend b) => FromJSON (PermDefPermission b DelPerm) where
  parseJSON = fmap DelPerm' . parseJSON

instance (Backend b) => ToJSON (PermDefPermission b perm) where
  toJSON = \case
    SelPerm' p -> toJSON p
    InsPerm' p -> toJSON p
    UpdPerm' p -> toJSON p
    DelPerm' p -> toJSON p

instance (Backend b, HasCodec (perm b), IsPerm perm) => HasCodec (PermDefPermission b perm) where
  codec = dimapCodec mkPermDefPermission unPermDefPermission codec

deriving stock instance (Backend b) => Show (PermDefPermission b perm)

deriving stock instance (Backend b) => Eq (PermDefPermission b perm)

-----------------------------

class IsPerm perm where
  mkPermDefPermission :: perm b -> PermDefPermission b perm
  permType :: PermType

instance IsPerm SelPerm where
  mkPermDefPermission = SelPerm'
  permType = PTSelect

instance IsPerm InsPerm where
  mkPermDefPermission = InsPerm'
  permType = PTInsert

instance IsPerm UpdPerm where
  mkPermDefPermission = UpdPerm'
  permType = PTUpdate

instance IsPerm DelPerm where
  mkPermDefPermission = DelPerm'
  permType = PTDelete

unPermDefPermission :: PermDefPermission b perm -> perm b
unPermDefPermission = \case
  SelPerm' p -> p
  InsPerm' p -> p
  UpdPerm' p -> p
  DelPerm' p -> p

reflectPermDefPermission :: PermDefPermission b a -> PermType
reflectPermDefPermission = \case
  SelPerm' _ -> PTSelect
  InsPerm' _ -> PTInsert
  UpdPerm' _ -> PTUpdate
  DelPerm' _ -> PTDelete

instance (Backend b, ToJSON (perm b)) => ToJSON (PermDef b perm) where
  toJSON = object . toAesonPairs

instance (Backend b) => ToAesonPairs (PermDef b perm) where
  toAesonPairs (PermDef rn perm comment) =
    [ "role" .= rn,
      "permission" .= perm,
      "comment" .= comment
    ]

instance (Backend b, HasCodec (perm b), IsPerm perm) => HasCodec (PermDef b perm) where
  codec =
    AC.object (backendPrefix @b <> T.toTitle (permTypeToCode (permType @perm)) <> "PermDef")
      $ PermDef
      <$> requiredField' "role"
      .== _pdRole
        <*> requiredField' "permission"
      .== _pdPermission
        <*> optionalFieldOrNull' "comment"
      .== _pdComment
    where
      (.==) = (AC..=)

data QueryRootFieldType
  = QRFTSelect
  | QRFTSelectByPk
  | QRFTSelectAggregate
  deriving stock (Show, Eq, Generic, Enum, Bounded)
  deriving anyclass (Hashable, NFData)

instance FromJSON QueryRootFieldType where
  parseJSON = genericParseJSON defaultOptions {constructorTagModifier = snakeCase . drop 4}

instance ToJSON QueryRootFieldType where
  toJSON = genericToJSON defaultOptions {constructorTagModifier = snakeCase . drop 4}

instance HasCodec QueryRootFieldType where
  codec =
    stringConstCodec
      $ NonEmpty.fromList
      $ (\x -> (x, T.pack $ snakeCase $ drop 4 $ show x))
      <$> [minBound ..]

data SubscriptionRootFieldType
  = SRFTSelect
  | SRFTSelectByPk
  | SRFTSelectAggregate
  | SRFTSelectStream
  deriving stock (Show, Eq, Generic, Enum, Bounded)
  deriving anyclass (Hashable, NFData)

instance FromJSON SubscriptionRootFieldType where
  parseJSON = genericParseJSON defaultOptions {constructorTagModifier = snakeCase . drop 4}

instance ToJSON SubscriptionRootFieldType where
  toJSON = genericToJSON defaultOptions {constructorTagModifier = snakeCase . drop 4}

instance HasCodec SubscriptionRootFieldType where
  codec =
    stringConstCodec
      $ NonEmpty.fromList
      $ (\x -> (x, T.pack $ snakeCase $ drop 4 $ show x))
      <$> [minBound ..]

-- Insert permission
data InsPerm (b :: BackendType) = InsPerm
  { ipCheck :: BoolExp b,
    ipSet :: Maybe (ColumnValues b Value),
    ipColumns :: Maybe (PermColSpec b),
    ipBackendOnly :: Bool, -- see Note [Backend only permissions]
    ipValidateInput :: Maybe (ValidateInput InputWebhook)
  }
  deriving (Show, Eq, Generic)

instance (Backend b) => FromJSON (InsPerm b) where
  parseJSON = withObject "InsPerm" $ \o ->
    InsPerm
      <$> o
      .: "check"
      <*> o
      .:? "set"
      <*> o
      .:? "columns"
      <*> o
      .:? "backend_only"
      .!= False
      <*> o
      .:? "validate_input"

instance (Backend b) => ToJSON (InsPerm b) where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}

instance (Backend b) => HasCodec (InsPerm b) where
  codec =
    AC.object (backendPrefix @b <> "InsPerm")
      $ InsPerm
      <$> requiredField' "check"
      AC..= ipCheck
        <*> optionalField' "set"
      AC..= ipSet
        <*> optionalField' "columns"
      AC..= ipColumns
        <*> optionalFieldWithDefault' "backend_only" False
      AC..= ipBackendOnly
        <*> optionalField' "validate_input"
      AC..= ipValidateInput

type InsPermDef b = PermDef b InsPerm

data AllowedRootFields rootFieldType
  = ARFAllowAllRootFields
  | ARFAllowConfiguredRootFields (Set.HashSet rootFieldType)
  deriving (Show, Eq, Generic)

instance (NFData rootFieldType) => NFData (AllowedRootFields rootFieldType)

instance (ToJSON rootFieldType) => ToJSON (AllowedRootFields rootFieldType) where
  toJSON = \case
    ARFAllowAllRootFields -> String "allow all root fields"
    ARFAllowConfiguredRootFields configuredRootFields -> toJSON configuredRootFields

-- | Serializes set of allowed fields as a nullable array, where @null@ maps to
-- 'ARFAllowAllRootFields', and any array value maps to
-- 'ARFAllowConfiguredRootFields'.
instance (Hashable rootFieldType, HasCodec rootFieldType) => HasCodec (AllowedRootFields rootFieldType) where
  codec = dimapCodec dec enc $ maybeCodec $ listCodec codec
    where
      dec (Just fields) = ARFAllowConfiguredRootFields $ Set.fromList fields
      dec (Nothing) = ARFAllowAllRootFields

      enc ARFAllowAllRootFields = Nothing
      enc (ARFAllowConfiguredRootFields fields) = Just $ Set.toList fields

instance (Semigroup (HashSet rootFieldType)) => Semigroup (AllowedRootFields rootFieldType) where
  ARFAllowAllRootFields <> _ = ARFAllowAllRootFields
  _ <> ARFAllowAllRootFields = ARFAllowAllRootFields
  ARFAllowConfiguredRootFields rfL <> ARFAllowConfiguredRootFields rfR =
    ARFAllowConfiguredRootFields (rfL <> rfR)

isRootFieldAllowed :: (Eq rootField) => rootField -> AllowedRootFields rootField -> Bool
isRootFieldAllowed rootField = \case
  ARFAllowAllRootFields -> True
  ARFAllowConfiguredRootFields allowedRootFields -> rootField `elem` allowedRootFields

-- Input validation for inserts
data ValidateInputHttpDefinition webhook = ValidateInputHttpDefinition
  { _vihdUrl :: webhook,
    _vihdHeaders :: [HeaderConf],
    _vihdTimeout :: Timeout,
    _vihdForwardClientHeaders :: Bool
  }
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance (NFData webhook) => NFData (ValidateInputHttpDefinition webhook)

instance (FromJSON webhook) => FromJSON (ValidateInputHttpDefinition webhook) where
  parseJSON = withObject "ValidateInputHttpDefinition" $ \o ->
    ValidateInputHttpDefinition
      <$> o
      .: "url"
      <*> o
      .:? "headers"
      .!= []
      <*> o
      .:? "timeout"
      .!= (Timeout 10)
      <*> o
      .:? "forward_client_headers"
      .!= False

instance (ToJSON webhook) => ToJSON (ValidateInputHttpDefinition webhook) where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}

instance (HasCodec webhook) => HasCodec (ValidateInputHttpDefinition webhook) where
  codec =
    AC.object "ValidateInputHttpDefinition"
      $ ValidateInputHttpDefinition
      <$> requiredField' "url"
      AC..= _vihdUrl
        <*> optionalFieldWithOmittedDefault' "headers" []
      AC..= _vihdHeaders
        <*> optionalFieldWithOmittedDefault' "timeout" (Timeout 10)
      AC..= _vihdTimeout
        <*> optionalFieldWithOmittedDefault' "forward_client_headers" False
      AC..= _vihdForwardClientHeaders

data ValidateInput webhook
  = VIHttp (ValidateInputHttpDefinition webhook)
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance (NFData webhook) => NFData (ValidateInput webhook)

instance (FromJSON webhook) => FromJSON (ValidateInput webhook) where
  parseJSON = withObject "ValidateInput" $ \o -> do
    ty <- o .: "type"
    case ty of
      "http" -> VIHttp <$> o .: "definition"
      _ -> fail $ "expecting only 'http' for 'type' but got " <> ty

instance (ToJSON webhook) => ToJSON (ValidateInput webhook) where
  toJSON v =
    let (ty, def) = case v of
          VIHttp def' -> (String "http", toJSON def')
     in object ["type" .= ty, "definition" .= def]

instance (HasCodec webhook, Typeable webhook) => HasCodec (ValidateInput webhook) where
  codec =
    AC.object ("ValidateInput" <> typeableName @webhook)
      $ VIHttp
      <$ requiredFieldWith' "type" (AC.literalTextCodec "http")
      AC..= const "http"
        <*> requiredField' "definition"
      AC..= ((\(VIHttp def) -> def))

-- Select constraint
data SelPerm (b :: BackendType) = SelPerm
  { -- | Allowed columns
    spColumns :: PermColSpec b,
    -- | Filter expression
    spFilter :: BoolExp b,
    -- | Limit value
    spLimit :: Maybe Int,
    -- | Allow aggregation
    spAllowAggregations :: Bool,
    -- | Allowed computed fields which should not
    -- include the fields returning rows of existing table.
    spComputedFields :: [ComputedFieldName],
    spAllowedQueryRootFields :: AllowedRootFields QueryRootFieldType,
    spAllowedSubscriptionRootFields :: AllowedRootFields SubscriptionRootFieldType
  }
  deriving (Show, Eq, Generic)

instance (Backend b) => ToJSON (SelPerm b) where
  toJSON SelPerm {..} =
    let queryRootFieldsPair =
          case spAllowedQueryRootFields of
            ARFAllowAllRootFields -> mempty
            ARFAllowConfiguredRootFields configuredRootFields ->
              ["query_root_fields" .= configuredRootFields]
        subscriptionRootFieldsPair =
          case spAllowedSubscriptionRootFields of
            ARFAllowAllRootFields -> mempty
            ARFAllowConfiguredRootFields configuredRootFields ->
              ["subscription_root_fields" .= configuredRootFields]
        limitPair =
          case spLimit of
            Nothing -> mempty
            Just limit -> ["limit" .= limit]
     in object
          $ [ "columns" .= spColumns,
              "filter" .= spFilter,
              "allow_aggregations" .= spAllowAggregations,
              "computed_fields" .= spComputedFields
            ]
          <> queryRootFieldsPair
          <> subscriptionRootFieldsPair
          <> limitPair

instance (Backend b) => FromJSON (SelPerm b) where
  parseJSON = do
    withObject "SelPerm" $ \o -> do
      queryRootFieldsMaybe <- o .:? "query_root_fields"
      subscriptionRootFieldsMaybe <- o .:? "subscription_root_fields"
      allowedQueryRootFields <-
        case queryRootFieldsMaybe of
          Just configuredQueryRootFields -> do
            pure $ ARFAllowConfiguredRootFields configuredQueryRootFields
          Nothing -> pure ARFAllowAllRootFields
      allowedSubscriptionRootFields <-
        case subscriptionRootFieldsMaybe of
          Just configuredSubscriptionRootFields -> pure $ ARFAllowConfiguredRootFields configuredSubscriptionRootFields
          Nothing -> pure $ ARFAllowAllRootFields

      SelPerm
        <$> o
        .: "columns"
        <*> o
        .: "filter"
        <*> o
        .:? "limit"
        <*> o
        .:? "allow_aggregations"
        .!= False
        <*> o
        .:? "computed_fields"
        .!= []
        <*> pure allowedQueryRootFields
        <*> pure allowedSubscriptionRootFields

instance (Backend b) => HasCodec (SelPerm b) where
  codec =
    AC.object (backendPrefix @b <> "SelPerm")
      $ SelPerm
      <$> requiredField' "columns"
      AC..= spColumns
        <*> requiredField' "filter"
      AC..= spFilter
        <*> optionalField' "limit"
      AC..= spLimit
        <*> optionalFieldWithOmittedDefault' "allow_aggregations" False
      AC..= spAllowAggregations
        <*> optionalFieldWithOmittedDefault' "computed_fields" []
      AC..= spComputedFields
        <*> optionalFieldWithOmittedDefault' "query_root_fields" ARFAllowAllRootFields
      AC..= spAllowedQueryRootFields
        <*> optionalFieldWithOmittedDefault' "subscription_root_fields" ARFAllowAllRootFields
      AC..= spAllowedSubscriptionRootFields

type SelPermDef b = PermDef b SelPerm

-- Delete permission
data DelPerm (b :: BackendType) = DelPerm
  { dcFilter :: BoolExp b,
    dcBackendOnly :: Bool, -- see Note [Backend only permissions]
    dcValidateInput :: Maybe (ValidateInput InputWebhook)
  }
  deriving (Show, Eq, Generic)

instance (Backend b) => FromJSON (DelPerm b) where
  parseJSON = withObject "DelPerm" $ \o ->
    DelPerm
      <$> o
      .: "filter"
      <*> o
      .:? "backend_only"
      .!= False
      <*> o
      .:? "validate_input"

instance (Backend b) => ToJSON (DelPerm b) where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}

instance (Backend b) => HasCodec (DelPerm b) where
  codec =
    AC.object (backendPrefix @b <> "DelPerm")
      $ DelPerm
      <$> requiredField' "filter"
      .== dcFilter
        <*> optionalFieldWithOmittedDefault' "backend_only" False
      .== dcBackendOnly
        <*> optionalField' "validate_input"
      .== dcValidateInput
    where
      (.==) = (AC..=)

type DelPermDef b = PermDef b DelPerm

-- Update constraint
data UpdPerm (b :: BackendType) = UpdPerm
  { ucColumns :: PermColSpec b, -- Allowed columns
    ucSet :: Maybe (ColumnValues b Value), -- Preset columns
    ucFilter :: BoolExp b, -- Filter expression (applied before update)

    -- | Check expression, which must be true after update.
    -- This is optional because we don't want to break the v1 API
    -- but Nothing should be equivalent to the expression which always
    -- returns true.
    ucCheck :: Maybe (BoolExp b),
    ucBackendOnly :: Bool, -- see Note [Backend only permissions]
    ucValidateInput :: Maybe (ValidateInput InputWebhook)
  }
  deriving (Show, Eq, Generic)

instance (Backend b) => FromJSON (UpdPerm b) where
  parseJSON = withObject "UpdPerm" $ \o ->
    UpdPerm
      <$> o
      .: "columns"
      <*> o
      .:? "set"
      <*> o
      .: "filter"
      <*> o
      .:? "check"
      <*> o
      .:? "backend_only"
      .!= False
      <*> o
      .:? "validate_input"

instance (Backend b) => ToJSON (UpdPerm b) where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}

instance (Backend b) => HasCodec (UpdPerm b) where
  codec =
    AC.object (backendPrefix @b <> "UpdPerm")
      $ UpdPerm
      <$> requiredField "columns" "Allowed columns"
      AC..= ucColumns
        <*> optionalField "set" "Preset columns"
      AC..= ucSet
        <*> requiredField' "filter"
      AC..= ucFilter
        -- Include @null@ in serialized output for this field because that is
        -- the way the @toOrdJSON@ serialization is written.
        <*> optionalFieldOrIncludedNull' "check"
      AC..= ucCheck
        <*> optionalFieldWithOmittedDefault' "backend_only" False
      AC..= ucBackendOnly
        <*> optionalField' "validate_input"
      AC..= ucValidateInput

type UpdPermDef b = PermDef b UpdPerm

-- The Expression-level TemplateHaskell splices below fail unless there is a
-- declaration-level splice before to ensure phase separation.
-- See https://gitlab.haskell.org/ghc/ghc/-/issues/9813
$(return [])

instance (Backend b) => FromJSON (PermDef b SelPerm) where
  parseJSON = genericParseJSON hasuraJSON

instance (Backend b) => FromJSON (PermDef b InsPerm) where
  parseJSON = genericParseJSON hasuraJSON

instance (Backend b) => FromJSON (PermDef b UpdPerm) where
  parseJSON = genericParseJSON hasuraJSON

instance (Backend b) => FromJSON (PermDef b DelPerm) where
  parseJSON = genericParseJSON hasuraJSON

$(makeLenses ''PermDef)
