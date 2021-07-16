{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module generates a random 'Metadata' object, using a number of
-- 'Arbitrary' instances. This is used by the QuickCheck-based testing suite.
-- This module is not used by the graphql-engine library itself, and we may wish
-- to relocate it, for instance to Hasura.Generator.
--
-- TODO: for consistency, we need to decide if we want to move all Arbitrary
-- instances to this module, or if we want to associate them back to their
-- types. Either choice will require some amount of cleanup.
-- See https://github.com/hasura/graphql-engine-mono/issues/1736.

module Hasura.RQL.DDL.Metadata.Generator (genMetadata) where

import           Hasura.Prelude

import qualified Data.Aeson                                    as J
import qualified Data.HashMap.Strict                           as Map
import qualified Data.HashMap.Strict.InsOrd                    as OM
import qualified Data.HashSet.InsOrd                           as SetIns
import qualified Data.List.NonEmpty                            as NE
import qualified Data.Text                                     as T
import qualified Data.Vector                                   as V
import qualified Language.GraphQL.Draft.Parser                 as G
import qualified Language.GraphQL.Draft.Syntax                 as G
import qualified Language.Haskell.TH.Syntax                    as TH
import qualified Network.URI                                   as N
import qualified System.Cron.Parser                            as Cr

import           Data.Functor.Compose
import           Data.List.Extended                            (duplicates)
import           Data.Maybe                                    (fromJust)
import           Data.Scientific
import           System.Cron.Types
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.Partial
import           Test.QuickCheck.Instances.Semigroup           ()
import           Test.QuickCheck.Instances.Time                ()
import           Test.QuickCheck.Instances.UnorderedContainers ()

import           Hasura.RQL.DDL.Headers
import           Hasura.RQL.DDL.Metadata.Types
import           Hasura.RQL.Types


-- Metadata

genMetadata :: Gen Metadata
genMetadata = fromJust genericPartialArbitrary


-- Containers

instance (Arbitrary k, Eq k, Hashable k, Arbitrary v) => Arbitrary (InsOrdHashMap k v) where
  arbitrary = OM.fromList <$> arbitrary

instance (Arbitrary a, Eq a, Hashable a) => Arbitrary (SetIns.InsOrdHashSet a) where
  arbitrary = SetIns.fromList <$> arbitrary

instance (PartialArbitrary a) => PartialArbitrary (NE.NonEmpty a) where
  partialArbitrary = getCompose do
    h <- Compose partialArbitrary
    t <- Compose $ listOf <$> partialArbitrary
    pure $ h NE.:| t

instance (PartialArbitrary k, Eq k, Hashable k, PartialArbitrary v) => PartialArbitrary (HashMap k v) where
  partialArbitrary = (fmap . fmap) Map.fromList partialArbitrary

instance (PartialArbitrary k, Eq k, Hashable k, PartialArbitrary v) => PartialArbitrary (InsOrdHashMap k v) where
  partialArbitrary = (fmap . fmap) OM.fromList partialArbitrary

instance (PartialArbitrary a, Eq a, Hashable a) => PartialArbitrary (SetIns.InsOrdHashSet a) where
  partialArbitrary = (fmap . fmap) SetIns.fromList partialArbitrary


-- Arbitrary instances
-- Those types, like Metadata, need an arbitrary instance, but may hit @Void@,
-- and therefore delegate their arbitrary instance to 'PartialArbitrary'

instance PartialArbitrary a => Arbitrary (G.Directive a) where
  arbitrary = fromJust genericPartialArbitrary

instance PartialArbitrary a => Arbitrary (G.Value a) where
  arbitrary = fromJust genericPartialArbitrary


-- Generic types.
-- All those types may contain Void, and therefore declare their own custom
-- instance of PartialArbitrary, using the generic implementation which
-- disregards all unrepresentable branches. Those types cannot and do not expose
-- a regular Arbitrary instance. See Test.QuickCheck.Arbitrary.Partial.

type ArbitraryBackend b =
  ( Backend b
  , PartialArbitrary (TableName b)
  , PartialArbitrary (FunctionName b)
  , PartialArbitrary (SourceConnConfiguration b)
  , PartialArbitrary (Column b)
  )

instance ArbitraryBackend b => PartialArbitrary (FunctionMetadata b)
instance ArbitraryBackend b => PartialArbitrary (SourceMetadata b)
instance ArbitraryBackend b => PartialArbitrary (TableConfig b)
instance ArbitraryBackend b => PartialArbitrary (RelManualConfig b)
instance ArbitraryBackend b => PartialArbitrary (ObjRelUsingChoice b)
instance ArbitraryBackend b => PartialArbitrary (ArrRelUsingFKeyOn b)
instance ArbitraryBackend b => PartialArbitrary (ComputedFieldDefinition b)
instance ArbitraryBackend b => PartialArbitrary (ComputedFieldMetadata b)
instance ArbitraryBackend b => PartialArbitrary (GExists b ColExp)
instance ArbitraryBackend b => PartialArbitrary (GBoolExp b ColExp)
instance ArbitraryBackend b => PartialArbitrary (BoolExp b)
instance ArbitraryBackend b => PartialArbitrary (PermColSpec b)
instance ArbitraryBackend b => PartialArbitrary (InsPerm b)
instance ArbitraryBackend b => PartialArbitrary (SelPerm b)
instance ArbitraryBackend b => PartialArbitrary (UpdPerm b)
instance ArbitraryBackend b => PartialArbitrary (DelPerm b)
instance ArbitraryBackend b => PartialArbitrary (TableMetadata b)
instance PartialArbitrary a => PartialArbitrary (PermDef a)
instance PartialArbitrary a => PartialArbitrary (RelDef a)
instance (ArbitraryBackend b, PartialArbitrary a) => PartialArbitrary (RelUsing b a)


-- Regular types.
-- All those types are known to be representable, and we can write a regular
-- Arbitrary instance for each of them. They will use the default generic
-- overlappable instance of PartialArbitrary that simply defers back to
-- Arbitrary.

instance Arbitrary MetadataVersion where
  arbitrary = genericArbitrary

instance Arbitrary FunctionPermissionMetadata where
  arbitrary = genericArbitrary

instance Arbitrary InsertOrder where
  arbitrary = genericArbitrary

instance Arbitrary InheritedRole where
  arbitrary = genericArbitrary

instance Arbitrary ColExp where
  arbitrary = genericArbitrary

instance Arbitrary SubscribeColumns where
  arbitrary = genericArbitrary

instance Arbitrary SubscribeOpSpec where
  arbitrary = genericArbitrary

instance Arbitrary TriggerOpsDef where
  arbitrary = genericArbitrary

instance Arbitrary RetryConf where
  arbitrary = genericArbitrary

instance Arbitrary HeaderValue where
  arbitrary = genericArbitrary

instance Arbitrary HeaderConf where
  arbitrary = genericArbitrary

instance Arbitrary EventTriggerConf where
  arbitrary = genericArbitrary

instance Arbitrary FunctionConfig where
  arbitrary = genericArbitrary

instance Arbitrary FunctionExposedAs where
  arbitrary = genericArbitrary

instance Arbitrary RemoteSchemaDef where
  arbitrary = genericArbitrary

instance Arbitrary AddRemoteSchemaQuery where
  arbitrary = genericArbitrary

instance Arbitrary ListedQuery where
  arbitrary = genericArbitrary

instance Arbitrary CollectionDef where
  arbitrary = genericArbitrary

instance Arbitrary CreateCollection where
  arbitrary = genericArbitrary

instance Arbitrary CollectionReq where
  arbitrary = genericArbitrary

instance Arbitrary query => Arbitrary (EndpointMetadata query) where
  arbitrary = genericArbitrary

instance Arbitrary query => Arbitrary (EndpointDef query) where
  arbitrary = genericArbitrary

instance Arbitrary QueryReference where
  arbitrary = genericArbitrary

instance Arbitrary G.Nullability where
  arbitrary = genericArbitrary

instance Arbitrary G.GType where
  arbitrary = genericArbitrary

instance Arbitrary InputObjectTypeName where
  arbitrary = genericArbitrary

instance Arbitrary InputObjectFieldName where
  arbitrary = genericArbitrary

instance Arbitrary GraphQLType where
  arbitrary = genericArbitrary

instance Arbitrary InputObjectFieldDefinition where
  arbitrary = genericArbitrary

instance Arbitrary InputObjectTypeDefinition where
  arbitrary = genericArbitrary

instance Arbitrary RelType where
  arbitrary = genericArbitrary

instance Arbitrary RelationshipName where
  arbitrary = genericArbitrary

instance Arbitrary ObjectFieldName where
  arbitrary = genericArbitrary

instance Arbitrary ObjectTypeName where
  arbitrary = genericArbitrary

instance Arbitrary ScalarTypeDefinition where
  arbitrary = genericArbitrary

instance Arbitrary EnumTypeName where
  arbitrary = genericArbitrary

instance Arbitrary EnumValueDefinition where
  arbitrary = genericArbitrary

instance Arbitrary EnumTypeDefinition where
  arbitrary = genericArbitrary

instance Arbitrary CustomTypes where
  arbitrary = genericArbitrary

instance Arbitrary ArgumentName where
  arbitrary = genericArbitrary

instance Arbitrary ActionMutationKind where
  arbitrary = genericArbitrary

instance Arbitrary ActionType where
  arbitrary = genericArbitrary

instance Arbitrary ActionName where
  arbitrary = genericArbitrary

instance Arbitrary ActionPermissionMetadata where
  arbitrary = genericArbitrary

instance Arbitrary ActionMetadata where
  arbitrary = genericArbitrary

deriving instance Arbitrary RemoteArguments

instance Arbitrary FieldCall where
  arbitrary = genericArbitrary

deriving instance Arbitrary RemoteFields

instance Arbitrary RemoteRelationshipDef where
  arbitrary = genericArbitrary

instance Arbitrary RemoteRelationshipMetadata where
  arbitrary = genericArbitrary

instance Arbitrary CronTriggerMetadata where
  arbitrary = genericArbitrary

instance Arbitrary STRetryConf where
  arbitrary = genericArbitrary

instance Arbitrary NonNegativeDiffTime where
  arbitrary = genericArbitrary

instance Arbitrary G.ScalarTypeDefinition where
  arbitrary = genericArbitrary

instance Arbitrary G.InputValueDefinition where
  arbitrary = genericArbitrary

instance Arbitrary G.RootOperationTypeDefinition where
  arbitrary = genericArbitrary

instance Arbitrary G.OperationType where
  arbitrary = genericArbitrary

instance Arbitrary G.UnionTypeDefinition where
  arbitrary = genericArbitrary

instance Arbitrary G.EnumValueDefinition where
  arbitrary = genericArbitrary

instance Arbitrary G.EnumTypeDefinition where
  arbitrary = genericArbitrary

instance (Arbitrary a) => Arbitrary (G.FieldDefinition a) where
  arbitrary = genericArbitrary

instance (Arbitrary a) => Arbitrary (G.InputObjectTypeDefinition a) where
  arbitrary = genericArbitrary

instance (Arbitrary a) => Arbitrary (G.ObjectTypeDefinition a) where
  arbitrary = genericArbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (G.InterfaceTypeDefinition a b) where
  arbitrary = genericArbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (G.TypeDefinition a b) where
  arbitrary = genericArbitrary

instance Arbitrary G.TypeSystemDefinition where
  arbitrary = genericArbitrary

instance Arbitrary G.SchemaDefinition where
  arbitrary = genericArbitrary

instance Arbitrary G.SchemaDocument where
  arbitrary = genericArbitrary

instance Arbitrary RemoteSchemaPermissionDefinition where
  arbitrary = genericArbitrary

instance Arbitrary RemoteSchemaPermissionMetadata where
  arbitrary = genericArbitrary

instance Arbitrary RemoteSchemaMetadata where
  arbitrary = genericArbitrary

instance Arbitrary MetricsConfig where
  arbitrary = genericArbitrary

instance Arbitrary ApiLimit where
  arbitrary = genericArbitrary

instance Arbitrary DepthLimit where
  arbitrary = genericArbitrary

instance Arbitrary NodeLimit where
  arbitrary = genericArbitrary

instance Arbitrary RateLimit where
  arbitrary = genericArbitrary

instance Arbitrary RateLimitConfig where
  arbitrary = genericArbitrary

instance Arbitrary SetGraphqlIntrospectionOptions where
  arbitrary = genericArbitrary

instance Arbitrary a => Arbitrary (ObjectFieldDefinition a) where
  arbitrary = genericArbitrary

instance Arbitrary a => Arbitrary (ArgumentDefinition a) where
  arbitrary = genericArbitrary

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (ObjectTypeDefinition a b c) where
  arbitrary = genericArbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (TypeRelationship a b) where
  arbitrary = genericArbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (ActionDefinition a b) where
  arbitrary = genericArbitrary


-- Custom instances
-- All non-generic non-partial instances.

instance Arbitrary G.Name where
  arbitrary = G.unsafeMkName . T.pack <$> listOf1 (elements ['a'..'z'])

instance Arbitrary G.Description where
  arbitrary = G.Description <$> arbitrary

instance Arbitrary G.EnumValue where
  arbitrary = G.EnumValue <$> arbitrary

instance Arbitrary Scientific where
  arbitrary = ((fromRational . toRational) :: Int -> Scientific) <$> arbitrary

instance Arbitrary J.Value where
  arbitrary = sized sizedArbitraryValue
    where
      sizedArbitraryValue n
        | n <= 0 = oneof [pure J.Null, boolean, number, string]
        | otherwise = resize n' $ oneof [pure J.Null, boolean, number, string, array, object']
        where
          n' = n `div` 2
          boolean = J.Bool <$> arbitrary
          number = J.Number <$> arbitrary
          string = J.String <$> arbitrary
          array = J.Array . V.fromList <$> arbitrary
          object' = J.Object <$> arbitrary

-- FIXME:- URI type do not have Arbitrary class implemented.
-- For time being a singe URI value is generated every time
instance Arbitrary N.URI where
  arbitrary = pure $ N.URI "http:" (Just $ N.URIAuth "" "localhost" ":8080" ) "/path" "" ""

-- FIXME:- The GraphQL AST has 'Gen' by Hedgehog testing package which lacks the
-- 'Arbitrary' class implementation. For time being, a single query is generated every time.
instance Arbitrary GQLQueryWithText where
  arbitrary = pure $ GQLQueryWithText ( "query {author {id name}}"
                                      , GQLQuery simpleQuery
                                      )
    where
      simpleQuery = $(either (fail . T.unpack) TH.lift $ G.parseExecutableDoc "query {author {id name}}")

instance Arbitrary TableCustomRootFields where
  arbitrary = uniqueRootFields
    where
      uniqueRootFields = do
        (a, b, c, d, e, f, g, h, i) <- arbitrary
        if null $ duplicates [a, b, c, d, e, f, g, h, i] then
          pure $ TableCustomRootFields a b c d e f g h i
        else uniqueRootFields

instance Arbitrary MetadataResourceVersion where
  arbitrary = MetadataResourceVersion <$> arbitrary

instance Arbitrary UniqueParamConfig where
  arbitrary = elements
    [ UPCIpAddress
    , UPCSessionVar ["x-hasura-user-id"]
    , UPCSessionVar ["x-hasura-user-id", "x-hasura-team-id"]
    , UPCSessionVar ["x-hasura-user-id", "x-hasura-team-id", "x-hasura-org-id"]
    ]

instance Arbitrary CronSchedule where
  arbitrary = elements $ rights $ map Cr.parseCronSchedule
    [ "* * * * *"
    -- every minute
    , "5 * * * *"
    -- every hour at the 5th minute
    , "\5 * * * *"
    -- every 5 minutes
    , "* 5 * * *"
    -- every minute of the 5th hour of the day
    , "5 5 * * *"
    -- fifth minute of the fifth hour every day
    , "0 0 5 * *"
    -- 00:00 of the 5th day of every month
    , "0 0 1 1 *"
    -- 00:00 of 1st of January
    , "0 0 * * 0"
    -- Every sunday at 00:00
    ]

instance Arbitrary ParentRoles where
  arbitrary = genericArbitrary
