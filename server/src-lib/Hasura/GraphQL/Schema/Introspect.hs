module Hasura.GraphQL.Schema.Introspect where

import           Hasura.Prelude
-- import qualified Hasura.RQL.Types

import qualified Language.GraphQL.Draft.Syntax       as G
import qualified Language.GraphQL.Draft.Printer      as GP
import qualified Language.GraphQL.Draft.Printer.Text as GPT
import qualified Data.Aeson                          as J
import qualified Data.HashMap.Strict                 as Map
import qualified Data.HashMap.Strict.InsOrd          as OMap
import qualified Data.Vector                         as V

import qualified Hasura.GraphQL.Parser               as P

import           Hasura.GraphQL.Parser          (FieldParser, Kind (..), Parser, Schema (..))
import           Hasura.GraphQL.Parser.Class

{-
Note [Basics of introspection schema generation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We generate the introspection schema from the existing schema for queries,
mutations and subscriptions.  In other words, we generate one @Parser@ from some
other @Parser@s.  In this way, we avoid having to remember what types we have to
expose through introspection explicitly, as we did in a previous version of
graphql-engine.

However the schema information is obtained, the @Schema@ type stores it.  From a
@Schema@ object we then produce one @FieldParser@ that reads a `__schema` field,
and one that reads a `__type` field.  The idea is that these parsers simply
output a JSON value directly, and so indeed the type of @schema@, for instance,
is @FieldParser n J.Value@.

The idea of "just output the JSON object directly" breaks down when we want to
output a list of things, however, such as in the `types` field of `__schema`.
In the case of `types`, the JSON object to be generated is influenced by the
underlying selection set, so that, for instance,

```
query {
  __schema {
    types {
      name
    }
  }
}
```

means that we only output the _name_ of every type in our schema.  One naive
approach one might consider here would be to have a parser

```
typeField :: P.Type k -> Parser n J.Value
```

that takes a type, and is able to produce a JSON value for it, and then to apply
this parser to all the types in our schema.

However, we only have *one* selection set to parse: so which of the parsers we
obtained should we use to parse it?  And what should we do in the theoretical
case that we have a schema without any types?  (The latter is actually not
possible since we always have `query_root`, but it illustrates the problem that
there is no canonical choice of type to use to parse the selection set.)
Additionally, this would allow us to get the JSON output for *one* type, rather
than for our list of types.  After all, @Parser n@ is *not* a @Monad@ (it's not
even an @Applicative@), so we don't have a map @(a -> Parser n b) -> [a] -> m
[b]@.

In order to resolve this conundrum, let's consider what the ordinary Postgres
schema generates for a query such as follows.

```
query {
  author {
    articles {
      title
    }
  }
}
```

Now the @Parser@ for an article's title does not directly give the desired
output data: indeed, there would be many different titles, rather than any
single one we can return.  Instead, it returns a value that can, after parsing,
be used to build an output, along the lines of:

```
articleTitle :: FieldParser n SQLArticleTitle
```

(This is a heavily simplified representation of reality.)

These values can be thought of as an intermediate representation that can then
be used to generate and run SQL that gives the desired JSON output at a later
stage.  In other words, in the above example, @SQLArticleTitle@ can be thought
of as a function @Article -> Title@ that, given an article, gives back its
title.

Such an approach could help us as well, as, from instructions on how to generate
a JSON return for a given `__Type`, surely we can later simply apply this
construction to all types desired.

However, we don't _need_ to build an intermediate AST to do this: we can simply
output the conversion functions directly.  So the type of @typeField@ is closer
to:

```
typeField :: Parser n (P.Type k -> J.Value)
```

This says that we are always able to parse a selection set for a `__Type`, and
once we do, we obtain a map, which we refer to as `printer` in this module,
which can output a JSON object for a given GraphQL type from our schema.

To use `typeField` as part of another selection set, we build up a corresponding
`FieldParser`, thus obtaining a printer, then apply this printer to all desired
types, and output the final JSON object as a J.Array of the printed results,
like so (again, heavily simplified):

```
    types :: FieldParser n J.Value
    types = do
      printer <- P.subselection_ $$(G.litName "types") Nothing typeField
      return $ J.Array $ map printer $ allSchemaTypes
```

Upon reading this you may be bewildered how we are able to use do notation for
@FieldParser@, which does not have a @Monad@ instance, or even an @Applicative@
instance.  It just so happens that, as long as we write our do blocks carefully,
so that we only use the functoriality of @FieldParser@, the simplification rules
of GHC kick in just in time to avoid any application of @(>>=)@ or @return@.
Arguably the above notation is prettier than equivalent code that explicitly
reduces this to applications of @fmap@.  If you, dear reader, feel like the do
notation adds more confusion than value, you should feel free to change this, as
there is no deeper meaning to the application of do notation than ease of
reading.
-}

-- | Generate a __type introspection parser
typeIntrospection
  :: forall n
   . MonadParse n
  => Schema
  -> FieldParser n J.Value
typeIntrospection fakeSchema = do
  let nameArg :: P.InputFieldsParser n G.Name
      nameArg = G.unsafeMkName <$> P.field $$(G.litName "name") Nothing P.string
  nameAndPrinter <- P.subselection $$(G.litName "__type") Nothing nameArg typeField
  return $ case Map.lookup (fst nameAndPrinter) (sTypes fakeSchema) of
    Nothing -> J.Null
    Just (P.Definition n u d (P.SomeTypeInfo i)) ->
      snd nameAndPrinter (SomeType (P.Nullable (P.TNamed (P.Definition n u d i))))

-- | Generate a __schema introspection parser.
schema
  :: forall n
   . MonadParse n
  => Schema
  -> FieldParser n J.Value
schema fakeSchema =
  let schemaSetParser = schemaSet fakeSchema
  in P.subselection_ $$(G.litName "__schema") Nothing schemaSetParser

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

data SomeType = forall k . SomeType (P.Type k)

typeField
  :: forall n
   . MonadParse n
  => Parser 'Output n (SomeType -> J.Value)
typeField =
  let
    includeDeprecated :: P.InputFieldsParser n Bool
    includeDeprecated =
      P.fieldWithDefault $$(G.litName "includeDeprecated") Nothing (G.VBoolean False) P.boolean
    kind :: FieldParser n (SomeType -> J.Value)
    kind = P.selection_ $$(G.litName "kind") Nothing typeKind $>
      \case SomeType tp ->
              case tp of
                P.NonNullable _ ->
                  J.String "NON_NULL"
                P.Nullable (P.TList _) ->
                  J.String "LIST"
                P.Nullable (P.TNamed (P.Definition _ _ _ P.TIScalar)) ->
                  J.String "SCALAR"
                P.Nullable (P.TNamed (P.Definition _ _ _ (P.TIEnum _))) ->
                  J.String "ENUM"
                P.Nullable (P.TNamed (P.Definition _ _ _ (P.TIInputObject _))) ->
                  J.String "INPUT_OBJECT"
                P.Nullable (P.TNamed (P.Definition _ _ _ (P.TIObject _))) ->
                  J.String "OBJECT"
    name :: FieldParser n (SomeType -> J.Value)
    name = P.selection_ $$(G.litName "name") Nothing P.string $>
      \case SomeType tp ->
              case P.discardNullability tp of
                P.TNamed (P.Definition name' _ _ _) ->
                  nameAsJSON name'
                _ -> J.Null
    description :: FieldParser n (SomeType -> J.Value)
    description = P.selection_ $$(G.litName "description") Nothing P.string $>
      \case SomeType tp ->
              case P.discardNullability tp of
                P.TNamed (P.Definition _ _ (Just desc) _) ->
                  J.String (G.unDescription desc)
                _ -> J.Null
    fields :: FieldParser n (SomeType -> J.Value)
    fields = do
      -- TODO includeDeprecated
      nameAndPrinter <- P.subselection $$(G.litName "fields") Nothing includeDeprecated fieldField
      return $
        \case SomeType tp ->
                case tp of
                  P.Nullable (P.TNamed (P.Definition _ _ _ (P.TIObject fields'))) ->
                    J.Array $ V.fromList $ fmap (snd nameAndPrinter) fields'
                  _ -> J.Null
    interfaces :: FieldParser n (SomeType -> J.Value)
    interfaces = P.subselection_ $$(G.litName "interfaces") Nothing typeField $>
      \case SomeType tp ->
              case tp of
                P.Nullable (P.TNamed (P.Definition _ _ _ (P.TIObject _))) ->
                  J.Array mempty
                _ -> J.Null
    possibleTypes :: FieldParser n (SomeType -> J.Value)
    possibleTypes = P.subselection_ $$(G.litName "possibleTypes") Nothing typeField $>
      const J.Null -- We don't support any interfaces or union types.
    enumValues :: FieldParser n (SomeType -> J.Value)
    enumValues = do
      nameAndPrinter <- P.subselection $$(G.litName "enumValues") Nothing includeDeprecated enumValue
      return $
        \case SomeType tp ->
                case tp of
                  P.Nullable (P.TNamed (P.Definition _ _ _ (P.TIEnum vals))) ->
                    J.Array $ V.fromList $ toList $ fmap (snd nameAndPrinter) vals
                  _ -> J.Null
    inputFields :: FieldParser n (SomeType -> J.Value)
    inputFields = do
      printer <- P.subselection_ $$(G.litName "inputFields") Nothing inputValue
      return $
        \case SomeType tp ->
                case tp of
                  P.Nullable (P.TNamed (P.Definition _ _ _ (P.TIInputObject fieldDefs))) ->
                    J.Array $ V.fromList $ map printer fieldDefs
                  _ -> J.Null
    ofType :: FieldParser n (SomeType -> J.Value)
    ofType = do
      printer <- P.subselection_ $$(G.litName "ofType") Nothing typeField
      return $ \case
        SomeType (P.NonNullable x) ->
          printer $ SomeType $ P.Nullable x
        SomeType (P.Nullable (P.TList x)) ->
          printer $ SomeType x
        _ -> J.Null
  in
    applyPrinter <$>
    P.selectionSet
      $$(G.litName "__Type")
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
      nameAsJSON . P.dName
    description :: FieldParser n (P.Definition P.InputFieldInfo -> J.Value)
    description = P.selection_ $$(G.litName "description") Nothing P.string $> const J.Null
    typeF :: FieldParser n (P.Definition P.InputFieldInfo -> J.Value)
    typeF = do
      printer <- P.subselection_ $$(G.litName "type") Nothing typeField
      return $ \defInfo -> case P.dInfo defInfo of
        P.IFRequired tp -> printer $ SomeType $ P.NonNullable tp
        P.IFOptional tp _ -> printer $ SomeType $ P.Nullable tp
    defaultValue :: FieldParser n (P.Definition P.InputFieldInfo -> J.Value)
    defaultValue = P.selection_ $$(G.litName "defaultValue") Nothing P.string $>
      \defInfo -> case P.dInfo defInfo of
        P.IFOptional _ (Just val) -> J.String $ GPT.render GP.value val
        _ -> J.Null
  in
    applyPrinter <$>
    P.selectionSet
      $$(G.litName "__InputValue")
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
    name = P.selection_ $$(G.litName "name") Nothing P.string $>
      nameAsJSON . P.dName
    description :: FieldParser n (P.Definition P.EnumValueInfo -> J.Value)
    description = P.selection_ $$(G.litName "description") Nothing P.string $>
      maybe J.Null (J.String . G.unDescription) . P.dDescription
    -- TODO We don't seem to support enum value deprecation
    isDeprecated :: FieldParser n (P.Definition P.EnumValueInfo -> J.Value)
    isDeprecated = P.selection_ $$(G.litName "isDeprecated") Nothing P.string $>
      const (J.Bool False)
    deprecationReason :: FieldParser n (P.Definition P.EnumValueInfo -> J.Value)
    deprecationReason = P.selection_ $$(G.litName "deprecationReason") Nothing P.string $>
      const J.Null
  in
    applyPrinter <$>
    P.selectionSet
      $$(G.litName "__EnumValue")
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
  :: forall n
   . MonadParse n
  => Parser 'Both n ()
typeKind = P.enum
  $$(G.litName "__TypeKind")
  Nothing
  ( (P.Definition $$(G.litName "SCALAR") Nothing Nothing P.EnumValueInfo, ()) :|
    [ (P.Definition $$(G.litName "OBJECT") Nothing Nothing P.EnumValueInfo, ())
    , (P.Definition $$(G.litName "INTERFACE") Nothing Nothing P.EnumValueInfo, ())
    , (P.Definition $$(G.litName "UNION") Nothing Nothing P.EnumValueInfo, ())
    , (P.Definition $$(G.litName "ENUM") Nothing Nothing P.EnumValueInfo, ())
    , (P.Definition $$(G.litName "INPUT_OBJECT") Nothing Nothing P.EnumValueInfo, ())
    , (P.Definition $$(G.litName "LIST") Nothing Nothing P.EnumValueInfo, ())
    , (P.Definition $$(G.litName "NON_NULL") Nothing Nothing P.EnumValueInfo, ())
    ])

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
      nameAsJSON . P.dName
    description :: FieldParser n (P.Definition P.FieldInfo -> J.Value)
    description = P.selection_ $$(G.litName "description") Nothing P.string $> \defInfo ->
      case P.dDescription defInfo of
      Nothing -> J.Null
      Just desc -> J.String (G.unDescription desc)
    args :: FieldParser n (P.Definition P.FieldInfo -> J.Value)
    args = do
      printer <- P.subselection_ $$(G.litName "args") Nothing inputValue
      return $ J.Array . V.fromList . map printer . P.fArguments . P.dInfo
    typeF :: FieldParser n (P.Definition P.FieldInfo -> J.Value)
    typeF = do
      printer <- P.subselection_ $$(G.litName "type") Nothing typeField
      return $ printer . (\case P.FieldInfo _ tp -> SomeType tp) . P.dInfo
    -- TODO We don't seem to track deprecation info
    isDeprecated :: FieldParser n (P.Definition P.FieldInfo -> J.Value)
    isDeprecated = P.selection_ $$(G.litName "isDeprecated") Nothing P.string $>
      const (J.Bool False)
    deprecationReason :: FieldParser n (P.Definition P.FieldInfo -> J.Value)
    deprecationReason = P.selection_ $$(G.litName "deprecationReason") Nothing P.string $>
      const J.Null
  in
    applyPrinter <$>
    P.selectionSet $$(G.litName "__Field") Nothing
    [ name
    , description
    , args
    , typeF
    , isDeprecated
    , deprecationReason
    ]

{-
type __Directive {
  name: String!
  description: String
  locations: [__DirectiveLocation!]!
  args: [__InputValue!]!
  isRepeatable: Boolean!
}
-}

-- TODO actually output data.  Directives should be collected and saved in the Schema datatype.
directiveSet
  :: forall n
   . MonadParse n
  => Parser 'Output n J.Value
directiveSet =
  let
    name :: FieldParser n J.Value
    name = P.selection_ $$(G.litName "name") Nothing P.string $>
      J.Null
    description :: FieldParser n J.Value
    description = P.selection_ $$(G.litName "description") Nothing P.string $>
      J.Null
    locations :: FieldParser n J.Value
    locations = P.selection_ $$(G.litName "locations") Nothing P.string $>
      J.Null
    args :: FieldParser n J.Value
    args = P.subselection_ $$(G.litName "args") Nothing inputValue $>
      J.Null
    isRepeatable :: FieldParser n J.Value
    isRepeatable = P.selection_ $$(G.litName "isRepeatable") Nothing P.string $>
      J.Null
  in
    J.Null <$ P.selectionSet $$(G.litName "__Directive") Nothing
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
schemaSet fakeSchema =
  let
    description :: FieldParser n J.Value
    description = P.selection_ $$(G.litName "description") Nothing P.string $>
      case sDescription fakeSchema of
        Nothing -> J.Null
        Just s -> J.String $ G.unDescription s
    types :: FieldParser n J.Value
    types = do
      printer <- P.subselection_ $$(G.litName "types") Nothing typeField
      return $ J.Array $ V.fromList $ map (printer . schemaTypeToSomeType) $
        Map.elems $ sTypes fakeSchema
        where
          schemaTypeToSomeType
            :: P.Definition P.SomeTypeInfo
            -> SomeType
          schemaTypeToSomeType (P.Definition n u d (P.SomeTypeInfo i)) =
            SomeType $ P.Nullable $ P.TNamed (P.Definition n u d i)
    queryType :: FieldParser n J.Value
    queryType = do
      printer <- P.subselection_ $$(G.litName "queryType") Nothing typeField
      return $ printer $ SomeType $ sQueryType fakeSchema
    mutationType :: FieldParser n J.Value
    mutationType = do
      printer <- P.subselection_ $$(G.litName "mutationType") Nothing typeField
      return $ case sMutationType fakeSchema of
        Nothing -> J.Null
        Just tp -> printer $ SomeType tp
    subscriptionType :: FieldParser n J.Value
    subscriptionType = do
      printer <- P.subselection_ $$(G.litName "subscriptionType") Nothing typeField
      return $ case sSubscriptionType fakeSchema of
        Nothing -> J.Null
        Just tp -> printer $ SomeType tp
    directives :: FieldParser n J.Value
    directives = J.Array mempty <$ P.subselection_ $$(G.litName "directives") Nothing directiveSet
  in
    selectionSetToJSON . fmap (P.handleTypename nameAsJSON) <$>
    P.selectionSet
      $$(G.litName "__Schema")
      Nothing
      [ description
      , types
      , queryType
      , mutationType
      , subscriptionType
      , directives
      ]

selectionSetToJSON
  :: OMap.InsOrdHashMap G.Name J.Value
  -> J.Value
selectionSetToJSON = J.Object . OMap.toHashMap . OMap.mapKeys G.unName

applyPrinter
  :: OMap.InsOrdHashMap G.Name (P.ParsedSelection (a -> J.Value))
  -> a
  -> J.Value
applyPrinter = flip (\x -> selectionSetToJSON . fmap (($ x) . P.handleTypename (const . nameAsJSON)))

nameAsJSON :: G.Name -> J.Value
nameAsJSON = J.String . G.unName
