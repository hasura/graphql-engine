{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Generator () where

import           Hasura.Prelude

import qualified Data.Aeson                         as J
import qualified Data.HashMap.Strict                as Map
import qualified Data.HashMap.Strict.InsOrd         as OM
import qualified Data.HashSet                       as Set
import qualified Data.HashSet.InsOrd                as SetIns
import qualified Data.Text                          as T
import qualified Data.Vector                        as V
import qualified Language.GraphQL.Draft.Syntax      as G

import           Data.Containers.ListUtils          (nubOrd)
import           Data.Maybe                         (fromJust)
import           Data.Scientific
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.Extended
import           Test.QuickCheck.Arbitrary.Generic
import           Test.QuickCheck.Arbitrary.Partial

import           Hasura.GraphQL.Parser.Schema       (InputValue, Variable, VariableInfo)
import           Hasura.RQL.Types
import           Hasura.Server.Utils
import           Hasura.Session


-- -- Containers

instance (Arbitrary k, Eq k, Hashable k, Arbitrary v) => Arbitrary (InsOrdHashMap k v) where
  arbitrary = OM.fromList <$> arbitrary

instance (Arbitrary k, Eq k, Hashable k, Arbitrary v) => Arbitrary (HashMap k v) where
  arbitrary = Map.fromList <$> arbitrary

instance (Arbitrary a, Eq a, Hashable a) => Arbitrary (SetIns.InsOrdHashSet a) where
  arbitrary = SetIns.fromList <$> arbitrary

instance (Arbitrary a, Eq a, Hashable a) => Arbitrary (Set.HashSet a) where
  arbitrary = Set.fromList <$> arbitrary

instance (PartialArbitrary k, Eq k, Hashable k, PartialArbitrary v) => PartialArbitrary (HashMap k v) where
  partialArbitrary = (fmap . fmap) Map.fromList partialArbitrary

-- -- Arbitrary instances
-- -- Those types, like Metadata, need an arbitrary instance, but may hit @Void@,
-- -- and therefore delegate their arbitrary instance to 'PartialArbitrary'

instance PartialArbitrary a => Arbitrary (G.Directive a) where
  arbitrary = fromJust genericPartialArbitrary

instance PartialArbitrary a => Arbitrary (G.Value a) where
  arbitrary = fromJust genericPartialArbitrary


-- Regular types.
-- All those types are known to be representable, and we can write a regular
-- Arbitrary instance for each of them. They will use the default generic
-- overlappable instance of PartialArbitrary that simply defers back to
-- Arbitrary.

instance Arbitrary Text where
  arbitrary = T.pack <$> listOf (elements alphaNumerics)

instance Arbitrary SessionVariable where
  arbitrary = (mkSessionVariable . (sessionVariablePrefix <>)) <$> arbitrary

instance Arbitrary G.Nullability where
  arbitrary = genericArbitrary

instance Arbitrary G.GType where
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

instance Arbitrary (InputValue Void) where
  arbitrary = genericArbitrary

instance Arbitrary VariableInfo where
  arbitrary = genericArbitrary

instance Arbitrary Variable where
  arbitrary = genericArbitrary

instance Arbitrary SessionArgumentPresetInfo  where
  arbitrary = genericArbitrary

instance Arbitrary RemoteSchemaVariable where
  arbitrary = genericArbitrary

instance Arbitrary RemoteSchemaInputValueDefinition where
  arbitrary = genericArbitrary

instance Arbitrary RemoteSchemaIntrospection where
  arbitrary = genericArbitrary

instance Arbitrary IntrospectionResult where
  arbitrary = do
    scalarTypeNames <- nubOrd <$> arbitrary
    objectTypeNames <- distinctExcluding1 scalarTypeNames
    interfaceTypeNames <- distinctExcluding $ scalarTypeNames ++ objectTypeNames
    unionTypeNames <- distinctExcluding $ scalarTypeNames ++ objectTypeNames ++ interfaceTypeNames
    enumTypeNames <- distinctExcluding $ scalarTypeNames ++ objectTypeNames ++ interfaceTypeNames ++ unionTypeNames
    let outputTypeNames = scalarTypeNames ++ objectTypeNames ++ interfaceTypeNames ++ unionTypeNames ++ enumTypeNames
    inputObjectTypeNames <- distinctExcluding outputTypeNames
    let inputTypeNames = scalarTypeNames ++ enumTypeNames ++ inputObjectTypeNames
    let genType typeNames = oneof
          [ G.TypeNamed <$> arbitrary <*> elements typeNames
          , G.TypeList <$> arbitrary <*> genType typeNames]
    let genInputValueDefinition =
          G.InputValueDefinition <$> arbitrary <*> arbitrary <*> genType inputTypeNames <*> arbitrary <*> pure []
    let genRemoteSchemaInputValueDefinition = RemoteSchemaInputValueDefinition <$> genInputValueDefinition <*> pure Nothing
    let genRemoteSchemaInputValueDefinitions = case inputTypeNames of
          [] -> pure []
          _  -> listOf genRemoteSchemaInputValueDefinition
    let genFieldDefinitions = do
          fieldNames <- nubOrd <$> listOf1 arbitrary
          for fieldNames $ \n ->
            G.FieldDefinition <$> arbitrary <*> pure n <*> genRemoteSchemaInputValueDefinitions <*> genType outputTypeNames <*> pure []
    let genEnumValueDefinition = G.EnumValueDefinition <$> arbitrary <*> arbitrary <*> pure []

    scalarTypeDefinitions <- for scalarTypeNames $ \n ->
      G.ScalarTypeDefinition <$> arbitrary <*> pure n <*> pure []
    objectTypeDefinitions <- for objectTypeNames $ \n ->
      G.ObjectTypeDefinition <$> arbitrary <*> pure n <*> sublistOf interfaceTypeNames <*> pure [] <*> genFieldDefinitions
    interfaceTypeDefinitions <- for interfaceTypeNames $ \n ->
      G.InterfaceTypeDefinition <$> arbitrary <*> pure n <*> pure [] <*> genFieldDefinitions <*> listOf1 arbitrary
    unionTypeDefinitions <- for unionTypeNames $ \n ->
      G.UnionTypeDefinition <$> arbitrary <*> pure n <*> pure [] <*> sublistOf1 objectTypeNames
    enumTypeDefinitions <- for enumTypeNames $ \n ->
      G.EnumTypeDefinition <$> arbitrary <*> pure n <*> pure [] <*> listOf1 genEnumValueDefinition
    inputObjectTypeDefinitions <- for inputObjectTypeNames $ \n ->
      G.InputObjectTypeDefinition <$> arbitrary <*> pure n <*> pure [] <*> genRemoteSchemaInputValueDefinitions
    let irDoc = RemoteSchemaIntrospection $
          map G.TypeDefinitionScalar scalarTypeDefinitions ++
          map G.TypeDefinitionObject objectTypeDefinitions ++
          map G.TypeDefinitionInterface interfaceTypeDefinitions ++
          map G.TypeDefinitionUnion unionTypeDefinitions ++
          map G.TypeDefinitionEnum enumTypeDefinitions ++
          map G.TypeDefinitionInputObject inputObjectTypeDefinitions
    irQueryRoot <- elements objectTypeNames
    let maybeObjectTypeName = elements $ Nothing : (Just <$> objectTypeNames)
    irMutationRoot <- maybeObjectTypeName
    irSubscriptionRoot <- maybeObjectTypeName
    pure $ IntrospectionResult {..}

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
