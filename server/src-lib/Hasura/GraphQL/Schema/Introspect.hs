module Hasura.GraphQL.Schema.Introspect where

import           Hasura.Prelude
-- import qualified Hasura.RQL.Types

import qualified Language.GraphQL.Draft.Syntax       as G
import qualified Language.GraphQL.Draft.Printer      as GP
import qualified Language.GraphQL.Draft.Printer.Text as GPT
import qualified Data.Aeson                          as J
-- import qualified Data.HashMap.Strict           as Map
import qualified Data.HashMap.Strict.InsOrd          as OMap
import qualified Data.Vector                         as V

import qualified Hasura.GraphQL.Parser               as P

import           Hasura.GraphQL.Parser          (FieldParser, Kind (..), Parser)
import           Hasura.GraphQL.Parser.Class

-- TODO fix the type mismatch problems below
import Unsafe.Coerce

data Schema = Schema
  { sDescription :: Maybe G.Description
  -- TODO add support for input objects as well. NB 'Both does not suffice as
  -- this is the intersection of 'Input and 'Output, rather than its union.
  , sTypes :: [P.Type 'Output]
  , sQueryType :: P.Type 'Output
  , sMutationType :: Maybe (P.Type 'Output)
  , sSubscriptionType :: Maybe (P.Type 'Output)
  , sDirectives :: [G.Directive Void]
  }

-- | Generate an introspection parser.
schema
  :: forall n
   . MonadParse n
  => Schema
  -> FieldParser n J.Value
schema realSchema =
  let schemaSetParser = schemaSet realSchema
  in P.subselection_ $$(G.litName "__schema") Nothing schemaSetParser

selectionSetToJSON :: OMap.InsOrdHashMap G.Name J.Value -> J.Value
selectionSetToJSON = J.Object . OMap.toHashMap . OMap.mapKeys G.unName

takeType :: P.Type k -> P.NonNullableType k
takeType = P.discardNullability

{-
type __Type {
  kind: __TypeKind!
  name: String
  description: String

  # should be non-null for OBJECT and INTERFACE only, must be null for the others
  fields(includeDeprecated: Boolean = false): [__Field!]

  # should be non-null for OBJECT and INTERFACE only, must be null for the others
  interfaces: [__Type!]

  # should be non-null for INTERFACE and UNION only, always null for the others
  possibleTypes: [__Type!]

  # should be non-null for ENUM only, must be null for the others
  enumValues(includeDeprecated: Boolean = false): [__EnumValue!]

  # should be non-null for INPUT_OBJECT only, must be null for the others
  inputFields: [__InputValue!]

  # should be non-null for NON_NULL and LIST only, must be null for the others
  ofType: __Type
}
-}

typeField
  :: forall n k
   . MonadParse n
  => Parser 'Output n (P.Type k -> J.Value)
typeField =
  let
    -- TODO the kind printer code should be de-duplicated
    kind :: FieldParser n (P.Type k -> J.Value)
    kind = P.selection_ $$(G.litName "kind") Nothing typeKind $>
      \tp -> J.String case takeType tp of
        P.TList _ -> "LIST"
        P.TNamed (P.Definition _ _ _ P.TIScalar) -> "SCALAR"
        P.TNamed (P.Definition _ _ _ (P.TIEnum _)) -> "ENUM"
        P.TNamed (P.Definition _ _ _ (P.TIInputObject _)) -> "INPUT_OBJECT"
        P.TNamed (P.Definition _ _ _ (P.TIObject _)) -> "OBJECT"
    name :: FieldParser n (P.Type k -> J.Value)
    name = P.selection_ $$(G.litName "name") Nothing P.string $>
      \tp -> case takeType tp of
        P.TList _ -> J.Null
        P.TNamed def -> mkTypeName (P.dName def)
    description :: FieldParser n (P.Type k -> J.Value)
    description = P.selection_ $$(G.litName "description") Nothing P.string $>
      \tp -> case takeType tp of
        P.TList _ -> J.Null
        P.TNamed def -> case P.dDescription def of
          Nothing -> J.Null
          Just desc -> J.String (G.unDescription desc)
    fields :: FieldParser n (P.Type k -> J.Value)
    fields = do
      -- TODO includeDeprecated
      printer <- P.subselection_ $$(G.litName "fields") Nothing fieldField
      return $ \to -> case takeType to of
        P.TList _ -> J.Null
        P.TNamed def -> case P.dInfo def of
          P.TIObject fields' ->
            J.Array $ V.fromList $ fmap printer fields'
          _ -> J.Null
    interfaces :: FieldParser n (P.Type k -> J.Value)
    interfaces = P.subselection_ $$(G.litName "interfaces") Nothing typeField $>
      \tp -> case takeType tp of
        P.TNamed (P.Definition _ _ _ (P.TIObject _)) -> J.Array mempty
        _ -> J.Null
    possibleTypes :: FieldParser n (P.Type k -> J.Value)
    possibleTypes = P.subselection_ $$(G.litName "possibleTypes") Nothing typeField $>
      const J.Null
    enumValues :: FieldParser n (P.Type k -> J.Value)
    enumValues = do
      printer <- P.subselection_ $$(G.litName "enumValues") Nothing enumValue
      return $ \tp -> case takeType tp of
        P.TNamed (P.Definition _ _ _ (P.TIEnum vals)) -> J.Array $ V.fromList $ toList $ fmap printer vals
        _ -> J.Null
    inputFields :: FieldParser n (P.Type k -> J.Value)
    inputFields = P.subselection_ $$(G.litName "inputFields") Nothing inputValue $> const J.Null
    ofType :: FieldParser n (P.Type k -> J.Value)
    ofType = P.subselection_ $$(G.litName "ofType") Nothing typeField $> const J.Null
  in
    applyPrinter <$>
    P.selectionSet
      $$(G.litName "type")
      Nothing
      [ kind
      , name
      , description
      , fields
      , interfaces
      , possibleTypes
      , enumValues
      , inputFields
      , ofType
      ]

{-
type __InputValue {
  name: String!
  description: String
  type: __Type!
  defaultValue: String
}
-}
inputValue
  :: forall n
   . MonadParse n
  => Parser 'Output n (P.Definition P.InputFieldInfo -> J.Value)
inputValue =
  let
    name :: FieldParser n (P.Definition P.InputFieldInfo -> J.Value)
    name = P.selection_ $$(G.litName "name") Nothing P.string $>
      mkTypeName . P.dName
    description :: FieldParser n (P.Definition P.InputFieldInfo -> J.Value)
    description = P.selection_ $$(G.litName "description") Nothing P.string $> const J.Null
    typeF :: FieldParser n (P.Definition P.InputFieldInfo -> J.Value)
    typeF = do
      printer <- P.subselection_ $$(G.litName "type") Nothing (typeField @n @'Both)
      return $ \defInfo -> case P.dInfo defInfo of
        P.IFRequired tp -> printer $ P.NonNullable $ unsafeCoerce tp -- Typing/kinding mismatch, TODO
        P.IFOptional tp _ -> printer $ P.NonNullable $ unsafeCoerce tp -- ditto
    defaultValue :: FieldParser n (P.Definition P.InputFieldInfo -> J.Value)
    defaultValue = P.selection_ $$(G.litName "defaultValue") Nothing P.string $>
      \defInfo -> case P.dInfo defInfo of
        P.IFOptional _ (Just val) -> J.String $ GPT.render GP.value $ val
        _ -> J.Null
  in
    applyPrinter <$>
    P.selectionSet
      $$(G.litName "__inputvalue")
      Nothing
      [ name
      , description
      , typeF
      , defaultValue
      ]

{-
type __EnumValue {
  name: String!
  description: String
  isDeprecated: Boolean!
  deprecationReason: String
}
-}
enumValue
  :: forall n
   . MonadParse n
  => Parser 'Output n (P.Definition P.EnumValueInfo -> J.Value)
enumValue =
  let
    name :: FieldParser n (P.Definition P.EnumValueInfo -> J.Value)
    name = P.selection_ $$(G.litName "name") Nothing P.string $> mkTypeName . P.dName
    description :: FieldParser n (P.Definition P.EnumValueInfo -> J.Value)
    description = P.selection_ $$(G.litName "description") Nothing P.string $>
      maybe J.Null (J.String . G.unDescription) . P.dDescription
    -- TODO We don't seem to support enum value deprecation
    isDeprecated :: FieldParser n (P.Definition P.EnumValueInfo -> J.Value)
    isDeprecated = P.selection_ $$(G.litName "isDeprecated") Nothing P.string $> const (J.Bool False)
    deprecationReason :: FieldParser n (P.Definition P.EnumValueInfo -> J.Value)
    deprecationReason = P.selection_ $$(G.litName "deprecationReason") Nothing P.string $> const J.Null
  in
    applyPrinter <$>
    P.selectionSet
      $$(G.litName "_enumvalue")
      Nothing
      [ name
      , description
      , isDeprecated
      , deprecationReason
      ]

{-
enum __TypeKind {
  SCALAR
  OBJECT
  INTERFACE
  UNION
  ENUM
  INPUT_OBJECT
  LIST
  NON_NULL
}
-}
typeKind
  :: forall n k
   . MonadParse n
  => Parser 'Both n (P.Type k -> J.Value)
typeKind =
  P.enum $$(G.litName "__typekind") Nothing ((P.Definition $$(G.litName "SCALAR") Nothing Nothing P.EnumValueInfo,_todo):|[]) $>
  \tp -> J.String case takeType tp of
    P.TList _ -> "LIST"
    P.TNamed (P.Definition _ _ _ P.TIScalar) -> "SCALAR"
    P.TNamed (P.Definition _ _ _ (P.TIEnum _)) -> "ENUM"
    P.TNamed (P.Definition _ _ _ (P.TIInputObject _)) -> "INPUT_OBJECT"
    P.TNamed (P.Definition _ _ _ (P.TIObject _)) -> "OBJECT"

applyPrinter
  :: OMap.InsOrdHashMap G.Name (P.ParsedSelection (a -> J.Value))
  -> a
  -> J.Value
applyPrinter = flip (\x -> selectionSetToJSON . fmap (($ x) . P.handleTypename (const . mkTypeName)))

{-
type __Field {
  name: String!
  description: String
  args: [__InputValue!]!
  type: __Type!
  isDeprecated: Boolean!
  deprecationReason: String
}
-}
fieldField
  :: forall n
   . MonadParse n
  => Parser 'Output n (P.Definition P.FieldInfo -> J.Value)
fieldField =
  let
    name :: FieldParser n (P.Definition P.FieldInfo -> J.Value)
    name = P.selection_ $$(G.litName "name") Nothing P.string $>
      mkTypeName . P.dName
    description :: FieldParser n (P.Definition P.FieldInfo -> J.Value)
    description = P.selection_ $$(G.litName "description") Nothing P.string $> \defInfo ->
      case P.dDescription defInfo of
      Nothing -> J.Null
      Just desc -> J.String (G.unDescription desc)
    args :: FieldParser n (P.Definition P.FieldInfo -> J.Value)
    args = do
      printer <- P.subselection_ $$(G.litName "args") Nothing inputValue
      return \defInfo -> J.Array $ V.fromList $ map printer $ P.fArguments $ P.dInfo defInfo
    typeF :: FieldParser n (P.Definition P.FieldInfo -> J.Value)
    typeF = do
      printer <- P.subselection_ $$(G.litName "type") Nothing typeField
      return $ printer . (\case P.FieldInfo _ tp -> unsafeCoerce tp) . P.dInfo
    -- TODO We don't seem to track deprecation info
    isDeprecated :: FieldParser n (P.Definition P.FieldInfo -> J.Value)
    isDeprecated = P.selection_ $$(G.litName "isDeprecated") Nothing P.string $> const (J.Bool False)
    deprecationReason :: FieldParser n (P.Definition P.FieldInfo -> J.Value)
    deprecationReason = P.selection_ $$(G.litName "deprecationReason") Nothing P.string $> const J.Null
  in
    applyPrinter <$>
    P.selectionSet $$(G.litName "__field") Nothing
    [ name
    , description
    , args
    , typeF
    , isDeprecated
    , deprecationReason
    ]

-- uhh this is wrong
mkTypeName :: G.Name -> J.Value
mkTypeName = J.String . G.unName
{-
type __Directive {
  name: String!
  description: String
  locations: [__DirectiveLocation!]!
  args: [__InputValue!]!
  isRepeatable: Boolean!
}
-}

-- TODO actually output data
directiveSet
  :: forall n
   . MonadParse n
  => Parser 'Output n J.Value
directiveSet =
  let
    name :: FieldParser n J.Value
    name = J.Null <$ P.selection_ $$(G.litName "name") Nothing P.string
    description :: FieldParser n J.Value
    description = J.Null <$ P.selection_ $$(G.litName "description") Nothing P.string
    locations :: FieldParser n J.Value
    locations = J.Null <$ P.selection_ $$(G.litName "locations") Nothing P.string
    args :: FieldParser n J.Value
    args = J.Null <$ P.subselection_ $$(G.litName "args") Nothing inputValue
    isRepeatable :: FieldParser n J.Value
    isRepeatable = J.Null <$ P.selection_ $$(G.litName "isRepeatable") Nothing P.string
  in
    -- fmap (J.Object . OMap.toHashMap . (OMap.mapKeys G.unName) . fmap (P.handleTypename mkTypeName)) $
    J.Null <$ P.selectionSet $$(G.litName "__directive") Nothing
    [ name
    , description
    , locations
    , args
    , isRepeatable
    ]

{-
type __Schema {
  description: String
  types: [__Type!]!
  queryType: __Type!
  mutationType: __Type
  subscriptionType: __Type
  directives: [__Directive!]!
}
-}

schemaSet
  :: forall n
   . MonadParse n
  => Schema
  -> Parser 'Output n J.Value
schemaSet realSchema =
  let
    description :: FieldParser n J.Value
    description = P.selection_ $$(G.litName "description") Nothing P.string $>
      case sDescription realSchema of
        Nothing -> J.Null
        Just s -> J.String $ G.unDescription s

    types :: FieldParser n J.Value
    types = do
      printer <- P.subselection_ $$(G.litName "types") Nothing typeField
      return $ J.Array $ V.fromList $ map printer (sTypes realSchema)
    queryType :: FieldParser n J.Value
    queryType = do
      printer <- P.subselection_ $$(G.litName "queryType") Nothing typeField
      return $ printer (sQueryType realSchema)
    -- TODO implement
    mutationType :: FieldParser n J.Value
    mutationType = do
      printer <- P.subselection_ $$(G.litName "mutationType") Nothing typeField
      return $ case sMutationType realSchema of
        Nothing -> J.Null
        Just tp -> printer tp
    subscriptionType :: FieldParser n J.Value
    subscriptionType = do
      printer <- P.subselection_ $$(G.litName "subscriptionType") Nothing typeField
      return $ case sSubscriptionType realSchema of
        Nothing -> J.Null
        Just tp -> printer tp
    directives :: FieldParser n J.Value
    directives = J.Array mempty <$ P.subselection_ $$(G.litName "directives") Nothing directiveSet
  in
    selectionSetToJSON . fmap (P.handleTypename mkTypeName) <$>
    P.selectionSet
      $$(G.litName "__schema")
      Nothing
      [ description
      , types
      , queryType
      , mutationType
      , subscriptionType
      , directives
      ]
