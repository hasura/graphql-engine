{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Generator () where

import Data.Containers.ListUtils (nubOrd)
import Data.HashMap.Strict.Extended qualified as Map
import Data.HashMap.Strict.InsOrd qualified as OMap
import Data.Ratio ((%))
import Data.Text qualified as T
import Hasura.GraphQL.Namespace
import Hasura.Prelude
import Hasura.RQL.Types.RemoteSchema
import Hasura.RQL.Types.SchemaCache (IntrospectionResult (..))
import Hasura.Server.Utils
import Hasura.Session
import Language.GraphQL.Draft.Syntax qualified as G
import Test.QuickCheck

-- Quickcheck helpers

distinct :: (Arbitrary a, Ord a) => Gen [a]
distinct = nubOrd <$> arbitrary

distinct1 :: (Arbitrary a, Ord a) => Gen [a]
distinct1 = nubOrd <$> listOf1 arbitrary

arbitraryExcluding :: (Arbitrary a, Eq a) => [a] -> Gen a
arbitraryExcluding exclusions = arbitrary `suchThat` (`notElem` exclusions)

distinctExcluding :: (Arbitrary a, Ord a) => [a] -> Gen [a]
distinctExcluding = fmap nubOrd . listOf . arbitraryExcluding

distinctExcluding1 :: (Arbitrary a, Ord a) => [a] -> Gen [a]
distinctExcluding1 = fmap nubOrd . listOf1 . arbitraryExcluding

sublistOf1 :: [a] -> Gen [a]
sublistOf1 xs = sublistOf xs `suchThat` (not . null)

-- Third party instances

instance Arbitrary Text where
  arbitrary = T.pack <$> listOf arbitraryUnicodeChar

instance (Arbitrary k, Eq k, Hashable k, Arbitrary v) => Arbitrary (HashMap k v) where
  arbitrary = Map.fromList <$> arbitrary

instance (Arbitrary k, Eq k, Hashable k, Arbitrary v) => Arbitrary (InsOrdHashMap k v) where
  arbitrary = OMap.fromList <$> arbitrary
  shrink = fmap OMap.fromList . shrink . OMap.toList

-- GraphQL syntax instances

instance Arbitrary G.Name where
  arbitrary = G.unsafeMkName . T.pack <$> listOf1 (elements ['a' .. 'z'])

instance Arbitrary G.Description where
  arbitrary = G.Description <$> arbitrary

instance Arbitrary G.EnumValue where
  arbitrary = G.EnumValue <$> arbitrary

instance Arbitrary G.EnumValueDefinition where
  arbitrary =
    G.EnumValueDefinition
      <$> arbitrary
      <*> arbitrary
      <*> pure []

instance Arbitrary G.Nullability where
  arbitrary = G.Nullability <$> arbitrary

instance Arbitrary (G.Value Void) where
  arbitrary =
    oneof
      [ pure G.VNull,
        G.VInt <$> arbitrary,
        G.VFloat <$> arbitraryScientific,
        G.VString <$> arbitrary,
        G.VBoolean <$> arbitrary,
        G.VEnum <$> arbitrary,
        -- reduce the internal size factor at every level, so that this
        -- recursion is guaranteed to terminate
        G.VList <$> scale (`div` 2) arbitrary,
        G.VObject <$> scale (`div` 2) arbitrary
      ]
    where
      arbitraryScientific = do
        -- fromRational can create invalid repeating values that loop forever
        -- we avoid this by creating known good ratios
        num :: Integer <- arbitrary
        dem :: Integer <- elements [1 .. 32]
        pure $ fromRational $ num % (10 ^ dem)

-- Hasura instances

instance Arbitrary SessionVariable where
  arbitrary = do
    name <- arbitrary
    pure $ mkSessionVariable $ sessionVariablePrefix <> name

instance Arbitrary IntrospectionResult where
  arbitrary = do
    -- first, generate distinct names for each kind of object
    scalarTypeNames <- distinct
    objectTypeNames <- distinctExcluding1 scalarTypeNames
    interfaceTypeNames <- distinctExcluding $ scalarTypeNames ++ objectTypeNames
    unionTypeNames <- distinctExcluding $ scalarTypeNames ++ objectTypeNames ++ interfaceTypeNames
    enumTypeNames <- distinctExcluding $ scalarTypeNames ++ objectTypeNames ++ interfaceTypeNames ++ unionTypeNames
    let outputTypeNames = scalarTypeNames ++ objectTypeNames ++ interfaceTypeNames ++ unionTypeNames ++ enumTypeNames
    inputObjectTypeNames <- distinctExcluding outputTypeNames
    let inputTypeNames = scalarTypeNames ++ enumTypeNames ++ inputObjectTypeNames
    let inputValues = case inputTypeNames of
          [] -> pure []
          _ -> listOf $ genRemoteSchemaInputValueDefinition inputTypeNames

    -- then, create a matching definition for each name
    scalarTypeDefinitions <-
      for scalarTypeNames $
        genScalarTypeDefinition
    objectTypeDefinitions <-
      for objectTypeNames $
        genObjectTypeDefinition inputValues outputTypeNames interfaceTypeNames
    interfaceTypeDefinitions <-
      for interfaceTypeNames $
        genInterfaceTypeDefinition inputValues outputTypeNames
    unionTypeDefinitions <-
      for unionTypeNames $
        genUnionTypeDefinition objectTypeNames
    enumTypeDefinitions <-
      for enumTypeNames $
        genEnumTypeDefinition
    inputObjectTypeDefinitions <-
      for inputObjectTypeNames $
        genInputObjectTypeDefinition inputValues

    -- finally, create an IntrospectionResult from the aggregated definitions
    let irDoc =
          RemoteSchemaIntrospection $
            Map.fromListOn getTypeName $
              concat
                [ G.TypeDefinitionScalar <$> scalarTypeDefinitions,
                  G.TypeDefinitionObject <$> objectTypeDefinitions,
                  G.TypeDefinitionInterface <$> interfaceTypeDefinitions,
                  G.TypeDefinitionUnion <$> unionTypeDefinitions,
                  G.TypeDefinitionEnum <$> enumTypeDefinitions,
                  G.TypeDefinitionInputObject <$> inputObjectTypeDefinitions
                ]
    irQueryRoot <- elements objectTypeNames
    let maybeObjectTypeName = elements $ Nothing : (Just <$> objectTypeNames)
    irMutationRoot <- maybeObjectTypeName
    irSubscriptionRoot <- maybeObjectTypeName
    pure $ IntrospectionResult {..}

-- Generator helpers

genGType :: [G.Name] -> Gen G.GType
genGType typeNames =
  frequency
    -- bias towards avoiding deeply nested lists
    [ (7, G.TypeNamed <$> arbitrary <*> elements typeNames),
      (3, G.TypeList <$> arbitrary <*> genGType typeNames)
    ]

genInputValueDefinition :: [G.Name] -> Gen G.InputValueDefinition
genInputValueDefinition inputTypeNames =
  G.InputValueDefinition
    <$> arbitrary
    <*> arbitrary
    <*> genGType inputTypeNames
    <*> arbitrary
    <*> pure []

genScalarTypeDefinition :: G.Name -> Gen G.ScalarTypeDefinition
genScalarTypeDefinition name =
  G.ScalarTypeDefinition
    <$> arbitrary
    <*> pure name
    <*> pure []

genEnumTypeDefinition :: G.Name -> Gen G.EnumTypeDefinition
genEnumTypeDefinition name =
  G.EnumTypeDefinition
    <$> arbitrary
    <*> pure name
    <*> pure []
    <*> listOf1 arbitrary

genUnionTypeDefinition :: [G.Name] -> G.Name -> Gen G.UnionTypeDefinition
genUnionTypeDefinition objectTypeNames name =
  G.UnionTypeDefinition
    <$> arbitrary
    <*> pure name
    <*> pure []
    <*> sublistOf1 objectTypeNames

genFieldDefinition ::
  Gen [inputType] ->
  [G.Name] ->
  G.Name ->
  Gen (G.FieldDefinition inputType)
genFieldDefinition inputTypes outputTypeNames name =
  G.FieldDefinition
    <$> arbitrary
    <*> pure name
    <*> inputTypes
    <*> genGType outputTypeNames
    <*> pure []

genObjectTypeDefinition ::
  Gen [inputType] ->
  [G.Name] ->
  [G.Name] ->
  G.Name ->
  Gen (G.ObjectTypeDefinition inputType)
genObjectTypeDefinition inputTypes outputTypeNames interfaceTypeNames name =
  G.ObjectTypeDefinition
    <$> arbitrary
    <*> pure name
    <*> sublistOf interfaceTypeNames
    <*> pure []
    <*> fields
  where
    fields = distinct1 >>= traverse (genFieldDefinition inputTypes outputTypeNames)

genInterfaceTypeDefinition ::
  Arbitrary possibleType =>
  Gen [inputType] ->
  [G.Name] ->
  G.Name ->
  Gen (G.InterfaceTypeDefinition [possibleType] inputType)
genInterfaceTypeDefinition inputTypes outputTypeNames name =
  G.InterfaceTypeDefinition
    <$> arbitrary
    <*> pure name
    <*> pure []
    <*> fields
    <*> listOf1 arbitrary
  where
    fields = distinct1 >>= traverse (genFieldDefinition inputTypes outputTypeNames)

genInputObjectTypeDefinition ::
  Gen [inputType] ->
  G.Name ->
  Gen (G.InputObjectTypeDefinition inputType)
genInputObjectTypeDefinition values name =
  G.InputObjectTypeDefinition
    <$> arbitrary
    <*> pure name
    <*> pure []
    <*> values

genRemoteSchemaInputValueDefinition :: [G.Name] -> Gen RemoteSchemaInputValueDefinition
genRemoteSchemaInputValueDefinition inputTypeNames =
  RemoteSchemaInputValueDefinition
    <$> genInputValueDefinition inputTypeNames
    <*> pure Nothing

instance Arbitrary a => Arbitrary (NamespacedField a) where
  arbitrary = oneof [NotNamespaced <$> arbitrary, Namespaced <$> arbitrary]
  shrink = namespacedField (fmap NotNamespaced . shrink) (fmap Namespaced . shrink)
