{-# OPTIONS_GHC -fno-warn-orphans #-}
module Hasura.RQL.DDL.Metadata.Generator
  (genReplaceMetadata)
where

import           Hasura.GraphQL.Utils               (simpleGraphQLQuery)
import           Hasura.Prelude
import           Hasura.RQL.DDL.Headers
import           Hasura.RQL.DDL.Metadata.Types
import           Hasura.RQL.Types
import           Hasura.Server.Utils
import           Hasura.SQL.Types

import qualified Hasura.GraphQL.Context             as GC
import qualified Hasura.RQL.DDL.ComputedField       as ComputedField
import qualified Hasura.RQL.DDL.Permission          as Permission
import qualified Hasura.RQL.DDL.Permission.Internal as Permission
import qualified Hasura.RQL.DDL.QueryCollection     as Collection
import qualified Hasura.RQL.DDL.Relationship        as Relationship
import qualified Hasura.RQL.DDL.Schema              as Schema

import qualified Data.Aeson                         as J
import qualified Data.HashMap.Strict                as HM
import qualified Data.Text                          as T
import qualified Data.Vector                        as V
import qualified Language.GraphQL.Draft.Parser      as G
import qualified Language.GraphQL.Draft.Syntax      as G
import qualified Language.Haskell.TH.Syntax         as TH
import qualified Network.URI                        as N

import           Test.QuickCheck

genReplaceMetadata :: Gen ReplaceMetadata
genReplaceMetadata = do
  version <- arbitrary
  ReplaceMetadata version
    <$> arbitrary
    <*> genFunctionsMetadata version
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
  where
    genFunctionsMetadata :: MetadataVersion -> Gen FunctionsMetadata
    genFunctionsMetadata = \case
      MVVersion1 -> FMVersion1 <$> arbitrary
      MVVersion2 -> FMVersion2 <$> arbitrary

instance (Eq k, Hashable k, Arbitrary k, Arbitrary v) => Arbitrary (HM.HashMap k v) where
  arbitrary = HM.fromList <$> arbitrary

instance Arbitrary G.Name where
  arbitrary = G.Name . T.pack <$> listOf1 (elements ['a'..'z'])

instance Arbitrary MetadataVersion where
  arbitrary = genericArbitrary

instance Arbitrary GC.TableCustomRootFields where
  arbitrary = uniqueRootFields
    where
      uniqueRootFields = do
        (a, b, c, d, e, f) <- arbitrary
        if null $ duplicates [a, b, c, d, e, f] then
          pure $ GC.TableCustomRootFields a b c d e f
        else uniqueRootFields

instance Arbitrary TableConfig where
  arbitrary = genericArbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Relationship.RelUsing a b) where
  arbitrary = genericArbitrary

instance (Arbitrary a) => Arbitrary (Relationship.RelDef a) where
  arbitrary = genericArbitrary

instance Arbitrary Relationship.RelManualConfig where
  arbitrary = genericArbitrary

instance Arbitrary Relationship.ObjRelManualConfig where
  arbitrary = genericArbitrary

instance Arbitrary Relationship.ArrRelManualConfig where
  arbitrary = genericArbitrary

instance Arbitrary Relationship.ArrRelUsingFKeyOn where
  arbitrary = genericArbitrary

instance (Arbitrary a) => Arbitrary (Permission.PermDef a) where
  arbitrary = genericArbitrary

instance Arbitrary ComputedField.ComputedFieldDefinition where
  arbitrary = genericArbitrary

instance Arbitrary ComputedFieldMeta where
  arbitrary = genericArbitrary

instance Arbitrary J.Value where
  arbitrary = sized sizedArbitraryValue
    where
      sizedArbitraryValue n
        | n <= 0 = oneof [pure J.Null, boolean, number, string]
        | otherwise = resize n' $ oneof [pure J.Null, boolean, number, string, array, object']
        where
          n' = n `div` 2
          boolean = J.Bool <$> arbitrary
          number = (J.Number . fromRational . toRational :: Int -> J.Value) <$> arbitrary
          string = J.String <$> arbitrary
          array = J.Array . V.fromList <$> arbitrary
          object' = J.Object <$> arbitrary

instance Arbitrary ColExp where
  arbitrary = genericArbitrary

instance Arbitrary (GExists ColExp) where
  arbitrary = genericArbitrary

instance Arbitrary (GBoolExp ColExp) where
  arbitrary = genericArbitrary

instance Arbitrary BoolExp where
  arbitrary = genericArbitrary

instance Arbitrary Permission.PermColSpec where
  arbitrary = genericArbitrary

instance Arbitrary Permission.InsPerm where
  arbitrary = genericArbitrary

instance Arbitrary Permission.SelPerm where
  arbitrary = genericArbitrary

instance Arbitrary Permission.UpdPerm where
  arbitrary = genericArbitrary

instance Arbitrary Permission.DelPerm where
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

instance Arbitrary TableMeta where
  arbitrary = genericArbitrary

instance Arbitrary Schema.FunctionConfig where
  arbitrary = genericArbitrary

instance Arbitrary Schema.TrackFunctionV2 where
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
instance Arbitrary Collection.GQLQueryWithText where
  arbitrary = pure $ Collection.GQLQueryWithText ( simpleGraphQLQuery
                                                 , Collection.GQLQuery simpleQuery
                                                 )
    where
      simpleQuery = $(either (fail . T.unpack) TH.lift $ G.parseExecutableDoc simpleGraphQLQuery)

instance Arbitrary Collection.ListedQuery where
  arbitrary = genericArbitrary

instance Arbitrary Collection.CollectionDef where
  arbitrary = genericArbitrary

instance Arbitrary Collection.CreateCollection where
  arbitrary = genericArbitrary

instance Arbitrary Collection.CollectionReq where
  arbitrary = genericArbitrary
