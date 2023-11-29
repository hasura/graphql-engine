{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.QuickCheck.Instances () where

-------------------------------------------------------------------------------

import Data.Aeson.Types qualified as Aeson.Types
import Data.HashMap.Strict.Extended qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.HashMap.Strict.Multi qualified as MMap
import Data.HashSet qualified as HashSet
import Data.Ratio ((%))
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Trie qualified as Trie
import Hasura.Base.Error (QErr (..), QErrExtra (..))
import Hasura.Base.Error qualified as Error
import Hasura.GraphQL.Namespace (NamespacedField (..), namespacedField)
import Hasura.Prelude
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Endpoint.Trie
import Hasura.RQL.Types.Metadata.Object
  ( MetadataObjId (..),
    MetadataObject (..),
  )
import Hasura.RQL.Types.SchemaCache
import Hasura.RemoteSchema.Metadata (RemoteSchemaName (..))
import Hasura.RemoteSchema.SchemaCache
  ( RemoteSchemaInputValueDefinition (..),
    RemoteSchemaIntrospection (..),
    getTypeName,
  )
import Hasura.Server.Utils qualified as Utils
import Hasura.Session (SessionVariable, mkSessionVariable)
import Hasura.Table.Cache
import Language.GraphQL.Draft.Syntax qualified as GraphQL
import Network.HTTP.Types qualified as HTTP.Types
import Test.QuickCheck.Extended

-------------------------------------------------------------------------------
-- Orphan instances for third-party libraries types

instance Arbitrary Text where
  arbitrary = T.pack <$> listOf arbitraryUnicodeChar

instance
  (Arbitrary k, Hashable k, Arbitrary v) =>
  Arbitrary (HashMap k v)
  where
  arbitrary = HashMap.fromList <$> arbitrary
  shrink = fmap HashMap.fromList . shrink . HashMap.toList

instance
  (Arbitrary k, Hashable k, Arbitrary v) =>
  Arbitrary (InsOrdHashMap k v)
  where
  arbitrary = InsOrdHashMap.fromList <$> arbitrary
  shrink = fmap InsOrdHashMap.fromList . shrink . InsOrdHashMap.toList

instance Arbitrary Aeson.Types.JSONPathElement where
  arbitrary = Aeson.Types.Index <$> arbitrary

instance Arbitrary HTTP.Types.Status where
  arbitrary = HTTP.Types.Status <$> arbitrary <*> pure mempty

-------------------------------------------------------------------------------
-- Orphan instances for types defined by us, but which are not coupled to
-- GraphQL Engine.

instance (Hashable k, Arbitrary k, Eq v, Arbitrary v) => Arbitrary (Trie.Trie k v) where
  arbitrary = Trie.Trie <$> scale (`div` 2) arbitrary <*> arbitrary
  shrink (Trie.Trie m v) =
    [Trie.Trie m v' | v' <- shrink v]
      ++ [Trie.Trie m' v | m' <- shrink m]

instance (Hashable k, Arbitrary k, Ord v, Arbitrary v) => Arbitrary (MMap.MultiMap k v) where
  arbitrary = MMap.fromMap . fmap (Set.fromList . take 5) <$> arbitrary
  shrink m = map MMap.fromMap $ shrink $ MMap.toMap m

-------------------------------------------------------------------------------
-- Orphan instances for Language.GraphQL.Draft.Syntax types
--
-- TODO: We control `graphql-parser-hs`; we should upstream these orphan
-- instances as either a separate package (e.g. `graphql-parser-hs-quickcheck`)
-- or via flag (disabled by default) which enables QuickCheck as a dependency
-- and supplies (non-orphan) instances that way.

{-# ANN arbitraryGraphQLName ("HLint: ignore Use mkName" :: String) #-}
-- Factored out so that we can annotate it.
arbitraryGraphQLName :: Gen GraphQL.Name
arbitraryGraphQLName = GraphQL.unsafeMkName . T.pack <$> listOf1 (elements ['a' .. 'z'])

instance Arbitrary GraphQL.Name where
  arbitrary = arbitraryGraphQLName

instance Arbitrary GraphQL.Description where
  arbitrary = GraphQL.Description <$> arbitrary

instance Arbitrary GraphQL.EnumValue where
  arbitrary = GraphQL.EnumValue <$> arbitrary

instance Arbitrary GraphQL.EnumValueDefinition where
  arbitrary =
    GraphQL.EnumValueDefinition
      <$> arbitrary
      <*> arbitrary
      <*> pure []

instance Arbitrary GraphQL.Nullability where
  arbitrary = GraphQL.Nullability <$> arbitrary

instance Arbitrary (GraphQL.Value Void) where
  arbitrary =
    oneof
      [ pure GraphQL.VNull,
        GraphQL.VInt <$> arbitrary,
        GraphQL.VFloat <$> arbitraryScientific,
        GraphQL.VString <$> arbitrary,
        GraphQL.VBoolean <$> arbitrary,
        GraphQL.VEnum <$> arbitrary,
        -- reduce the internal size factor at every level, so that this
        -- recursion is guaranteed to terminate
        GraphQL.VList <$> scale (`div` 2) arbitrary,
        GraphQL.VObject <$> scale (`div` 2) arbitrary
      ]
    where
      arbitraryScientific = do
        -- fromRational can create invalid repeating values that loop forever
        -- we avoid this by creating known good ratios
        num :: Integer <- arbitrary
        dem :: Integer <- elements [1 .. 32]
        pure $ fromRational $ num % (10 ^ dem)

-- Generators for Language.GraphQL.Draft.Syntax types

genGType :: [GraphQL.Name] -> Gen GraphQL.GType
genGType typeNames =
  frequency
    -- bias towards avoiding deeply nested lists
    [ (7, GraphQL.TypeNamed <$> arbitrary <*> elements typeNames),
      (3, GraphQL.TypeList <$> arbitrary <*> genGType typeNames)
    ]

genInputValueDefinition :: [GraphQL.Name] -> Gen GraphQL.InputValueDefinition
genInputValueDefinition inputTypeNames =
  GraphQL.InputValueDefinition
    <$> arbitrary
    <*> arbitrary
    <*> genGType inputTypeNames
    <*> arbitrary
    <*> pure []

genScalarTypeDefinition :: GraphQL.Name -> Gen GraphQL.ScalarTypeDefinition
genScalarTypeDefinition name =
  GraphQL.ScalarTypeDefinition
    <$> arbitrary
    <*> pure name
    <*> pure []

genEnumTypeDefinition :: GraphQL.Name -> Gen GraphQL.EnumTypeDefinition
genEnumTypeDefinition name =
  GraphQL.EnumTypeDefinition
    <$> arbitrary
    <*> pure name
    <*> pure []
    <*> listOf1 arbitrary

genUnionTypeDefinition ::
  [GraphQL.Name] -> GraphQL.Name -> Gen GraphQL.UnionTypeDefinition
genUnionTypeDefinition objectTypeNames name =
  GraphQL.UnionTypeDefinition
    <$> arbitrary
    <*> pure name
    <*> pure []
    <*> sublistOf1 objectTypeNames

genFieldDefinition ::
  Gen [inputType] ->
  [GraphQL.Name] ->
  GraphQL.Name ->
  Gen (GraphQL.FieldDefinition inputType)
genFieldDefinition inputTypes outputTypeNames name =
  GraphQL.FieldDefinition
    <$> arbitrary
    <*> pure name
    <*> inputTypes
    <*> genGType outputTypeNames
    <*> pure []

genObjectTypeDefinition ::
  Gen [inputType] ->
  [GraphQL.Name] ->
  [GraphQL.Name] ->
  GraphQL.Name ->
  Gen (GraphQL.ObjectTypeDefinition inputType)
genObjectTypeDefinition inputTypes outputTypeNames interfaceTypeNames name =
  GraphQL.ObjectTypeDefinition
    <$> arbitrary
    <*> pure name
    <*> sublistOf interfaceTypeNames
    <*> pure []
    <*> fields
  where
    fields = distinct1 >>= traverse (genFieldDefinition inputTypes outputTypeNames)

genInterfaceTypeDefinition ::
  (Arbitrary possibleType) =>
  Gen [inputType] ->
  [GraphQL.Name] ->
  GraphQL.Name ->
  Gen (GraphQL.InterfaceTypeDefinition [possibleType] inputType)
genInterfaceTypeDefinition inputTypes outputTypeNames name =
  GraphQL.InterfaceTypeDefinition
    <$> arbitrary
    <*> pure name
    <*> pure []
    <*> fields
    <*> listOf1 arbitrary
  where
    fields = distinct1 >>= traverse (genFieldDefinition inputTypes outputTypeNames)

genInputObjectTypeDefinition ::
  Gen [inputType] ->
  GraphQL.Name ->
  Gen (GraphQL.InputObjectTypeDefinition inputType)
genInputObjectTypeDefinition values name =
  GraphQL.InputObjectTypeDefinition
    <$> arbitrary
    <*> pure name
    <*> pure []
    <*> values

-------------------------------------------------------------------------------
-- Instances for GraphQL Engine types

instance (Arbitrary a) => Arbitrary (PathComponent a) where
  arbitrary =
    oneof
      [ PathLiteral <$> arbitrary,
        pure PathParam
      ]

instance Arbitrary SessionVariable where
  arbitrary = do
    name <- arbitrary
    pure $ mkSessionVariable $ Utils.sessionVariablePrefix <> name

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
      for scalarTypeNames genScalarTypeDefinition
    objectTypeDefinitions <-
      for objectTypeNames
        $ genObjectTypeDefinition inputValues outputTypeNames interfaceTypeNames
    interfaceTypeDefinitions <-
      for interfaceTypeNames
        $ genInterfaceTypeDefinition inputValues outputTypeNames
    unionTypeDefinitions <-
      for unionTypeNames
        $ genUnionTypeDefinition objectTypeNames
    enumTypeDefinitions <-
      for enumTypeNames genEnumTypeDefinition
    inputObjectTypeDefinitions <-
      for inputObjectTypeNames
        $ genInputObjectTypeDefinition inputValues

    -- finally, create an IntrospectionResult from the aggregated definitions
    let irDoc =
          RemoteSchemaIntrospection
            $ HashMap.fromListOn getTypeName
            $ concat
              [ GraphQL.TypeDefinitionScalar <$> scalarTypeDefinitions,
                GraphQL.TypeDefinitionObject <$> objectTypeDefinitions,
                GraphQL.TypeDefinitionInterface <$> interfaceTypeDefinitions,
                GraphQL.TypeDefinitionUnion <$> unionTypeDefinitions,
                GraphQL.TypeDefinitionEnum <$> enumTypeDefinitions,
                GraphQL.TypeDefinitionInputObject <$> inputObjectTypeDefinitions
              ]
    irQueryRoot <- elements objectTypeNames
    let maybeObjectTypeName = elements $ Nothing : (Just <$> objectTypeNames)
    irMutationRoot <- maybeObjectTypeName
    irSubscriptionRoot <- maybeObjectTypeName
    pure $ IntrospectionResult {..}

instance (Arbitrary a) => Arbitrary (NamespacedField a) where
  arbitrary = oneof [NotNamespaced <$> arbitrary, Namespaced <$> arbitrary]
  shrink = namespacedField (fmap NotNamespaced . shrink) (fmap Namespaced . shrink)

instance Arbitrary QErrExtra where
  arbitrary =
    oneof
      [ ExtraExtensions <$> arbitrary,
        ExtraInternal <$> arbitrary
      ]

instance Arbitrary MetadataObjId where
  arbitrary =
    oneof
      -- This is not exhaustive, because it wasn't needed.
      [ pure $ MOSource SNDefault,
        MORemoteSchema . RemoteSchemaName <$> arbitrary
      ]

instance Arbitrary MetadataObject where
  arbitrary = MetadataObject <$> arbitrary <*> arbitrary

instance Arbitrary QErr where
  arbitrary = do
    -- This is not exhaustive, because it wasn't needed.
    --
    -- I just picked a few random error codes.
    let genCode =
          elements
            [ Error.AlreadyExists,
              Error.Conflict,
              Error.ConstraintError,
              Error.ConstraintViolation,
              Error.NotFound,
              Error.Unexpected
            ]
    QErr
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> genCode
      <*> arbitrary

instance Arbitrary Comment where
  arbitrary =
    oneof
      [ pure Automatic,
        Explicit <$> arbitrary
      ]

  shrink Automatic = []
  shrink (Explicit t) = Explicit <$> shrink t

instance Arbitrary CustomRootField where
  arbitrary = CustomRootField <$> arbitrary <*> arbitrary

instance Arbitrary TableCustomRootFields where
  arbitrary =
    ( TableCustomRootFields
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
    )
      `suchThat` allFieldNamesAreUnique
    where
      allFieldNamesAreUnique :: TableCustomRootFields -> Bool
      allFieldNamesAreUnique tcrf =
        let allNames = mapMaybe _crfName $ getAllCustomRootFields tcrf
            uniqueNames = HashSet.fromList allNames
         in length allNames == length uniqueNames

instance Arbitrary ColumnConfig where
  arbitrary =
    ColumnConfig
      <$> arbitrary
      <*> arbitrary

-- Generators for GraphQL Engine types

genRemoteSchemaInputValueDefinition ::
  [GraphQL.Name] -> Gen RemoteSchemaInputValueDefinition
genRemoteSchemaInputValueDefinition inputTypeNames =
  RemoteSchemaInputValueDefinition
    <$> genInputValueDefinition inputTypeNames
    <*> pure Nothing
