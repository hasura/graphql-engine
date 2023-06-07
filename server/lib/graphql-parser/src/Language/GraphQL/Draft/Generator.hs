{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Language.GraphQL.Draft.Generator
  ( -- * Generator
    Generator (..),
    generate,

    -- * Document
    genDocument,
    genExecutableDocument,

    -- * Identifiers
    genText,
    alpha_,
    alphaNum_,
    genGraphqlName,
    genName,
    genNullability,
    genType,
    genDescription,
    genValueWith,
    genEnumValue,
    genListValue,
    genObjectValue,
    genBlockText,
    genMinIndentedText,
    genIndentation,

    -- * Definitions
    genDefinition,
    genExecutableDefinition,
    genOperationDefinition,
    genTypedOperationDefinition,
    genVariableDefinition,
    genFragmentDefinition,
    genTypeSystemDefinition,
    genSchemaDefinition,
    genRootOperationTypeDefinition,
    genOperationType,
    genTypeDefinition,
    genScalarTypeDefinition,
    genObjectTypeDefinition,
    genInterfaceTypeDefinition,
    genUnionTypeDefinition,
    genEnumTypeDefinition,
    genInputObjectTypeDefinition,
    genInputValueDefinition,
    genEnumValueDefinition,
    genFieldDefinition,
    genFieldDefinitions,
    genDirectiveDefinition,
    genArgumentsDefinition,
    genDirectiveLocation,
    genExecutableDirectiveLocation,
    genTypeSystemDirectiveLocation,

    -- * Structure
    genSelectionSet,
    genSelection,
    genFragmentSpread,
    genInlineFragment,
    genField,
    genDirective,
    genDirectives,
    genArgument,

    -- * Helpers
    mkList,
    mkListNonEmpty,
  )
where

-------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO)
import Data.HashMap.Strict as HashMap
import Data.Scientific (fromFloatDigits)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Language.GraphQL.Draft.Syntax
import Prelude

-------------------------------------------------------------------------------

-- | *Generator*
class Generator a where
  genValue :: Gen (Value a)

instance Generator Void where
  genValue = genValueWith []

instance Generator Name where
  genValue = genValueWith [genName]

generate :: (MonadIO m) => Gen a -> m a
generate = Gen.sample

-------------------------------------------------------------------------------

-- Document

genDocument :: Gen Document
genDocument =
  Document <$> Gen.list (Range.linear 0 3) genDefinition

genExecutableDocument :: (Generator a) => Gen (ExecutableDocument a)
genExecutableDocument =
  ExecutableDocument <$> Gen.list (Range.linear 1 3) genExecutableDefinition

-------------------------------------------------------------------------------

-- Identifiers

genText :: Gen Text
genText = Gen.text (Range.linear 0 11) Gen.unicode

alpha_ :: Gen Char
alpha_ = Gen.choice [Gen.alpha, pure '_']

alphaNum_ :: Gen Char
alphaNum_ = Gen.choice [Gen.alphaNum, pure '_']

genGraphqlName :: Gen Text
genGraphqlName =
  Gen.text (Range.singleton 1) alpha_
    <> Gen.text (Range.linear 0 11) alphaNum_

{-# HLINT ignore "Use mkName" #-}
genName :: Gen Name
genName = unsafeMkName <$> genGraphqlName

genNullability :: Gen Nullability
genNullability = Nullability <$> Gen.bool

genType :: Gen GType
genType =
  Gen.recursive
    Gen.choice
    [TypeNamed <$> genNullability <*> genName]
    [TypeList <$> genNullability <*> genType]

genDescription :: Gen Description
genDescription = Description <$> Gen.choice [genText, genBlockText]

-------------------------------------------------------------------------------

-- Values

genValueWith :: [Gen a] -> Gen (Value a)
genValueWith varGens = Gen.recursive Gen.choice nonRecursive recursive
  where
    recursive =
      [ VList <$> genListValue (genValueWith varGens),
        VObject <$> genObjectValue (genValueWith varGens)
      ]
    -- TODO: use maxbound of int32/double or something?
    nonRecursive =
      [ pure VNull,
        VInt . fromIntegral <$> Gen.int32 (Range.linear 1 99999),
        VEnum <$> genEnumValue,
        VFloat . fromFloatDigits <$> Gen.double (Range.linearFrac 1.1 999999.99999),
        VString <$> Gen.choice [genText, genBlockText],
        VBoolean <$> Gen.bool
      ]
        <> [VVariable <$> var | var <- varGens]

genEnumValue :: Gen EnumValue
genEnumValue = EnumValue <$> genName

genListValue :: Gen (Value a) -> Gen [Value a]
genListValue = mkList

genObjectValue :: Gen (Value a) -> Gen (HashMap.HashMap Name (Value a))
genObjectValue genVal = HashMap.fromList <$> mkList genObjectField
  where
    genObjectField = (,) <$> genName <*> genVal

genBlockText :: Gen Text
genBlockText = T.unlines <$> Gen.list (Range.linear 0 20) line
  where
    line = do
      Gen.frequency
        [ (10, Gen.text (Range.linear 1 10) Gen.unicode),
          (10, return "\n"),
          (6, genIndentation),
          (5, genMinIndentedText 10),
          (4, return ""),
          (3, return " "),
          (6, return "\t"),
          (3, return "\""), -- "
          (3, return "\\") -- \
        ]

-- | Like `genText` but with random indentation in the start of the string according
-- to a minimum value.
genMinIndentedText :: Int -> Gen Text
genMinIndentedText min_ = do
  let minIndent = T.replicate min_ " "
  i <- genIndentation
  t <- genText
  return (minIndent <> i <> t)

genIndentation :: Gen Text
genIndentation = do
  Gen.text (Range.linear 0 100) (return ' ')

-------------------------------------------------------------------------------

-- Definitions

genDefinition :: Gen Definition
genDefinition =
  Gen.choice
    [ DefinitionExecutable <$> genExecutableDefinition,
      DefinitionTypeSystem <$> genTypeSystemDefinition
    ]

genExecutableDefinition :: (Generator a) => Gen (ExecutableDefinition a)
genExecutableDefinition =
  Gen.choice
    [ ExecutableDefinitionOperation <$> genOperationDefinition,
      ExecutableDefinitionFragment <$> genFragmentDefinition
    ]

genOperationDefinition :: (Generator a) => Gen (OperationDefinition FragmentSpread a)
genOperationDefinition =
  Gen.choice
    [ OperationDefinitionTyped <$> genTypedOperationDefinition,
      OperationDefinitionUnTyped <$> genSelectionSet
    ]

genTypedOperationDefinition :: (Generator a) => Gen (TypedOperationDefinition FragmentSpread a)
genTypedOperationDefinition =
  TypedOperationDefinition
    <$> genOperationType
    <*> Gen.maybe genName
    <*> mkList genVariableDefinition
    <*> genDirectives
    <*> genSelectionSet

genVariableDefinition :: Gen VariableDefinition
genVariableDefinition =
  VariableDefinition
    <$> genName
    <*> genType
    <*> Gen.maybe genValue

genFragmentDefinition :: Gen FragmentDefinition
genFragmentDefinition =
  FragmentDefinition
    <$> genName
    <*> genName
    <*> genDirectives
    <*> genSelectionSet

genTypeSystemDefinition :: Gen TypeSystemDefinition
genTypeSystemDefinition =
  Gen.choice
    [ TypeSystemDefinitionSchema <$> genSchemaDefinition,
      TypeSystemDefinitionType <$> genTypeDefinition
    ]

genSchemaDefinition :: Gen SchemaDefinition
genSchemaDefinition =
  SchemaDefinition
    <$> Gen.maybe genDirectives
    <*> mkList genRootOperationTypeDefinition

genRootOperationTypeDefinition :: Gen RootOperationTypeDefinition
genRootOperationTypeDefinition =
  RootOperationTypeDefinition
    <$> genOperationType
    <*> genName

genOperationType :: Gen OperationType
genOperationType =
  Gen.element
    [ OperationTypeQuery,
      OperationTypeMutation,
      OperationTypeSubscription
    ]

genTypeDefinition :: Gen (TypeDefinition () InputValueDefinition)
genTypeDefinition =
  Gen.choice
    [ TypeDefinitionScalar <$> genScalarTypeDefinition,
      TypeDefinitionObject <$> genObjectTypeDefinition,
      TypeDefinitionInterface <$> genInterfaceTypeDefinition,
      TypeDefinitionUnion <$> genUnionTypeDefinition,
      TypeDefinitionEnum <$> genEnumTypeDefinition,
      TypeDefinitionInputObject <$> genInputObjectTypeDefinition
    ]

genScalarTypeDefinition :: Gen ScalarTypeDefinition
genScalarTypeDefinition =
  ScalarTypeDefinition
    <$> Gen.maybe genDescription
    <*> genName
    <*> genDirectives

genObjectTypeDefinition :: Gen (ObjectTypeDefinition InputValueDefinition)
genObjectTypeDefinition =
  ObjectTypeDefinition
    <$> Gen.maybe genDescription
    <*> genName
    <*> mkList genName
    <*> genDirectives
    <*> genFieldDefinitions

genInterfaceTypeDefinition :: Gen (InterfaceTypeDefinition () InputValueDefinition)
genInterfaceTypeDefinition =
  InterfaceTypeDefinition
    <$> Gen.maybe genDescription
    <*> genName
    <*> genDirectives
    <*> genFieldDefinitions
    <*> pure ()

genUnionTypeDefinition :: Gen UnionTypeDefinition
genUnionTypeDefinition =
  UnionTypeDefinition
    <$> Gen.maybe genDescription
    <*> genName
    <*> genDirectives
    <*> mkList genName

genEnumTypeDefinition :: Gen EnumTypeDefinition
genEnumTypeDefinition =
  EnumTypeDefinition
    <$> Gen.maybe genDescription
    <*> genName
    <*> genDirectives
    <*> mkList genEnumValueDefinition

genInputObjectTypeDefinition :: Gen (InputObjectTypeDefinition InputValueDefinition)
genInputObjectTypeDefinition =
  InputObjectTypeDefinition
    <$> Gen.maybe genDescription
    <*> genName
    <*> genDirectives
    <*> mkList genInputValueDefinition

genInputValueDefinition :: Gen InputValueDefinition
genInputValueDefinition =
  InputValueDefinition
    <$> Gen.maybe genDescription
    <*> genName
    <*> genType
    <*> Gen.maybe genValue
    <*> genDirectives

genEnumValueDefinition :: Gen EnumValueDefinition
genEnumValueDefinition =
  EnumValueDefinition
    <$> Gen.maybe genDescription
    <*> genEnumValue
    <*> genDirectives

genFieldDefinition :: Gen (FieldDefinition InputValueDefinition)
genFieldDefinition =
  FieldDefinition
    <$> Gen.maybe genDescription
    <*> genName
    <*> mkList genInputValueDefinition
    <*> genType
    <*> genDirectives

genFieldDefinitions :: Gen [FieldDefinition InputValueDefinition]
genFieldDefinitions = mkList genFieldDefinition

genDirectiveDefinition :: Gen (DirectiveDefinition InputValueDefinition)
genDirectiveDefinition =
  DirectiveDefinition
    <$> Gen.maybe genDescription
    <*> genName
    <*> genArgumentsDefinition
    <*> Gen.list (Range.linear 1 10) genDirectiveLocation

genArgumentsDefinition :: Gen (ArgumentsDefinition InputValueDefinition)
genArgumentsDefinition = Gen.list (Range.linear 1 10) genInputValueDefinition

genDirectiveLocation :: Gen DirectiveLocation
genDirectiveLocation =
  Gen.choice
    [ DLExecutable <$> genExecutableDirectiveLocation,
      DLTypeSystem <$> genTypeSystemDirectiveLocation
    ]

genExecutableDirectiveLocation :: Gen ExecutableDirectiveLocation
genExecutableDirectiveLocation =
  Gen.element
    [ EDLQUERY,
      EDLMUTATION,
      EDLSUBSCRIPTION,
      EDLFIELD,
      EDLFRAGMENT_DEFINITION,
      EDLFRAGMENT_SPREAD,
      EDLINLINE_FRAGMENT
    ]

genTypeSystemDirectiveLocation :: Gen TypeSystemDirectiveLocation
genTypeSystemDirectiveLocation =
  Gen.element
    [ TSDLSCHEMA,
      TSDLSCALAR,
      TSDLOBJECT,
      TSDLFIELD_DEFINITION,
      TSDLARGUMENT_DEFINITION,
      TSDLINTERFACE,
      TSDLUNION,
      TSDLENUM,
      TSDLENUM_VALUE,
      TSDLINPUT_OBJECT,
      TSDLINPUT_FIELD_DEFINITION
    ]

-------------------------------------------------------------------------------

-- Structure

genSelectionSet :: (Generator a) => Gen (SelectionSet FragmentSpread a)
genSelectionSet = mkListNonEmpty genSelection

genSelection :: (Generator a) => Gen (Selection FragmentSpread a)
genSelection =
  Gen.recursive
    Gen.choice
    [ SelectionFragmentSpread <$> genFragmentSpread
    ]
    [ SelectionField <$> genField,
      SelectionInlineFragment <$> genInlineFragment
    ]

genFragmentSpread :: (Generator a) => Gen (FragmentSpread a)
genFragmentSpread =
  FragmentSpread
    <$> genName
    <*> genDirectives

genInlineFragment :: (Generator a) => Gen (InlineFragment FragmentSpread a)
genInlineFragment =
  InlineFragment
    <$> Gen.maybe genName
    <*> genDirectives
    <*> genSelectionSet

genField :: (Generator a) => Gen (Field FragmentSpread a)
genField =
  Field
    <$> Gen.maybe genName
    <*> genName
    <*> (HashMap.fromList <$> mkList genArgument)
    <*> genDirectives
    <*> genSelectionSet

genDirective :: (Generator a) => Gen (Directive a)
genDirective =
  Directive
    <$> genName
    <*> (HashMap.fromList <$> mkList genArgument)

genDirectives :: (Generator a) => Gen [Directive a]
genDirectives = mkList genDirective

genArgument :: (Generator a) => Gen (Name, Value a)
genArgument = (,) <$> genName <*> genValue

-------------------------------------------------------------------------------

-- Helpers

mkList :: Gen a -> Gen [a]
mkList = Gen.list $ Range.linear 0 11

mkListNonEmpty :: Gen a -> Gen [a]
mkListNonEmpty = Gen.list $ Range.linear 1 11
