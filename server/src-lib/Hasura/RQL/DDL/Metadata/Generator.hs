{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module generates a random 'Metadata' object, using a number of
-- 'Arbitrary' instances.  This is used by the QuickCheck-based testing suite.
-- This module is not used by the graphql-engine library itself, and we may wish
-- to relocate it, for instance to Hasura.Generator.

module Hasura.RQL.DDL.Metadata.Generator
  (genMetadata)
where

import           Hasura.Prelude

import qualified Data.Aeson                                    as J
import qualified Data.HashMap.Strict.InsOrd                    as OM
import qualified Data.HashSet.InsOrd                           as SetIns
import qualified Data.Text                                     as T
import qualified Data.Vector                                   as V
import qualified Language.GraphQL.Draft.Parser                 as G
import qualified Language.GraphQL.Draft.Syntax                 as G
import qualified Language.Haskell.TH.Syntax                    as TH
import qualified Network.URI                                   as N
import qualified System.Cron.Parser                            as Cr

import           Data.List.Extended                            (duplicates)
import           Data.Scientific
import           System.Cron.Types
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Semigroup           ()
import           Test.QuickCheck.Instances.Time                ()
import           Test.QuickCheck.Instances.UnorderedContainers ()

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.GraphQL.Utils                          (simpleGraphQLQuery)
import           Hasura.RQL.DDL.Headers
import           Hasura.RQL.DDL.Metadata.Types
import           Hasura.RQL.Types

genMetadata :: Gen Metadata
genMetadata = do
  version <- arbitrary
  Metadata
    <$> arbitrary
    <*> genFunctionsMetadata version
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
  where
    genFunctionsMetadata :: MetadataVersion -> Gen Functions
    genFunctionsMetadata = \case
      MVVersion1 -> OM.fromList . map (\qf -> (qf, FunctionMetadata qf emptyFunctionConfig)) <$> arbitrary
      MVVersion2 -> arbitrary

instance (Arbitrary k, Eq k, Hashable k, Arbitrary v) => Arbitrary (InsOrdHashMap k v) where
  arbitrary = OM.fromList <$> arbitrary

instance (Arbitrary a, Eq a, Hashable a) => Arbitrary (SetIns.InsOrdHashSet a) where
  arbitrary = SetIns.fromList <$> arbitrary

instance Arbitrary G.Name where
  arbitrary = G.unsafeMkName . T.pack <$> listOf1 (elements ['a'..'z'])

instance Arbitrary MetadataVersion where
  arbitrary = genericArbitrary

instance Arbitrary FunctionMetadata where
  arbitrary = genericArbitrary

instance Arbitrary TableCustomRootFields where
  arbitrary = uniqueRootFields
    where
      uniqueRootFields = do
        (a, b, c, d, e, f, g, h, i) <- arbitrary
        if null $ duplicates [a, b, c, d, e, f, g, h, i] then
          pure $ TableCustomRootFields a b c d e f g h i
        else uniqueRootFields

instance Arbitrary TableConfig where
  arbitrary = genericArbitrary

instance (Arbitrary a) => Arbitrary (RelUsing a) where
  arbitrary = genericArbitrary

instance (Arbitrary a) => Arbitrary (RelDef a) where
  arbitrary = genericArbitrary

instance Arbitrary RelManualConfig where
  arbitrary = genericArbitrary

instance Arbitrary ArrRelUsingFKeyOn where
  arbitrary = genericArbitrary

instance (Arbitrary a) => Arbitrary (PermDef a) where
  arbitrary = genericArbitrary

instance Arbitrary ComputedFieldDefinition where
  arbitrary = genericArbitrary

instance Arbitrary ComputedFieldMetadata where
  arbitrary = genericArbitrary

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

instance Arbitrary ColExp where
  arbitrary = genericArbitrary

instance Arbitrary (GExists b ColExp) where
  arbitrary = genericArbitrary

instance Arbitrary (GBoolExp b ColExp) where
  arbitrary = genericArbitrary

instance Arbitrary (BoolExp b) where
  arbitrary = genericArbitrary

instance Arbitrary PermColSpec where
  arbitrary = genericArbitrary

instance Arbitrary (InsPerm b) where
  arbitrary = genericArbitrary

instance Arbitrary (SelPerm b) where
  arbitrary = genericArbitrary

instance Arbitrary (UpdPerm b) where
  arbitrary = genericArbitrary

instance Arbitrary (DelPerm b) where
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

instance Arbitrary TableMetadata where
  arbitrary = genericArbitrary

instance Arbitrary FunctionConfig where
  arbitrary = genericArbitrary

instance Arbitrary TrackFunctionV2 where
  arbitrary = genericArbitrary

instance Arbitrary QualifiedTable where
  arbitrary = genericArbitrary

instance Arbitrary QualifiedFunction where
  arbitrary = genericArbitrary

-- FIXME:- URI type do not have Arbitrary class implemented.
-- For time being a singe URI value is generated every time
instance Arbitrary N.URI where
  arbitrary = pure $ N.URI "http:" (Just $ N.URIAuth "" "localhost" ":8080" ) "/path" "" ""

instance Arbitrary RemoteSchemaDef where
  arbitrary = genericArbitrary

instance Arbitrary AddRemoteSchemaQuery where
  arbitrary = genericArbitrary

-- FIXME:- The GraphQL AST has 'Gen' by Hedgehog testing package which lacks the
-- 'Arbitrary' class implementation. For time being, a single query is generated every time.
instance Arbitrary GQLQueryWithText where
  arbitrary = pure $ GQLQueryWithText ( simpleGraphQLQuery
                                      , GQLQuery simpleQuery
                                      )
    where
      simpleQuery = $(either (fail . T.unpack) TH.lift $ G.parseExecutableDoc simpleGraphQLQuery)

instance Arbitrary ListedQuery where
  arbitrary = genericArbitrary

instance Arbitrary CollectionDef where
  arbitrary = genericArbitrary

instance Arbitrary CreateCollection where
  arbitrary = genericArbitrary

instance Arbitrary CollectionReq where
  arbitrary = genericArbitrary

instance Arbitrary G.Description where
  arbitrary = G.Description <$> arbitrary

instance Arbitrary G.Nullability where
  arbitrary = genericArbitrary

instance Arbitrary G.GType where
  arbitrary = genericArbitrary

instance Arbitrary G.EnumValue where
  arbitrary = G.EnumValue <$> arbitrary

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

instance (Arbitrary a, Arbitrary b) => Arbitrary (TypeRelationship a b)  where
  arbitrary = genericArbitrary

instance Arbitrary ObjectTypeName where
  arbitrary = genericArbitrary

instance (Arbitrary a) => Arbitrary (ObjectFieldDefinition a) where
  arbitrary = genericArbitrary

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (ObjectTypeDefinition a b c) where
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

instance (Arbitrary a) => Arbitrary (ArgumentDefinition a) where
  arbitrary = genericArbitrary

instance Arbitrary ActionMutationKind where
  arbitrary = genericArbitrary

instance Arbitrary ActionType where
  arbitrary = genericArbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (ActionDefinition a b) where
  arbitrary = genericArbitrary

instance Arbitrary ActionName where
  arbitrary = genericArbitrary

instance Arbitrary InputWebhook where
  arbitrary = genericArbitrary

instance Arbitrary ActionPermissionMetadata where
  arbitrary = genericArbitrary

instance Arbitrary ActionMetadata where
  arbitrary = genericArbitrary

deriving instance Arbitrary RemoteArguments

instance Arbitrary a => Arbitrary (G.Value a) where
  arbitrary = genericArbitrary

instance Arbitrary FieldCall where
  arbitrary = genericArbitrary

deriving instance Arbitrary RemoteFields

instance Arbitrary RemoteRelationshipDef where
  arbitrary = genericArbitrary

instance Arbitrary RemoteRelationshipMetadata where
  arbitrary = genericArbitrary

instance Arbitrary CronTriggerMetadata where
  arbitrary = genericArbitrary

instance Arbitrary UrlConf where
  arbitrary = genericArbitrary

instance Arbitrary STRetryConf where
  arbitrary = genericArbitrary

instance Arbitrary NonNegativeDiffTime where
  arbitrary = genericArbitrary

instance Arbitrary CronSchedule where
  arbitrary = elements sampleCronSchedules

sampleCronSchedules :: [CronSchedule]
sampleCronSchedules = rights $ map Cr.parseCronSchedule
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
