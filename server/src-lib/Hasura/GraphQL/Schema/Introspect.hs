{-# LANGUAGE ApplicativeDo #-}

module Hasura.GraphQL.Schema.Introspect
  ( buildIntrospectionSchema,
    schema,
    typeIntrospection,
  )
where

import Data.Aeson.Ordered qualified as J
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Vector qualified as V
import Hasura.GraphQL.Parser.Class
import Hasura.GraphQL.Parser.Directives
import Hasura.GraphQL.Parser.Name qualified as GName
import Hasura.GraphQL.Schema.Parser as P
import Hasura.Prelude
import Language.GraphQL.Draft.Printer qualified as GP
import Language.GraphQL.Draft.Syntax qualified as G
import Text.Builder qualified as T

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
      printer <- P.subselection_ GName._types Nothing typeField
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

{- Note [What introspection exposes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
NB: By "introspection query", we mean a query making use of the __type or
__schema top-level fields.

It would be very convenient if we could simply build up our desired GraphQL
schema, without regard for introspection. Ideally, we would then extract the
data required for introspection from this complete GraphQL schema. There are,
however, some complications:

1. Of course, we _do_ need to include the introspection fields themselves into
   the query_root, so that we can deal with introspection queries
   appropriately. So we can't avoid thinking about introspection entirely while
   constructing the GraphQL schema.

2. The GraphQL specification says that although we must always expose __type and
   __schema fields as part of the query_root, they must not be visible fields of
   the query_root object type. See
   http://spec.graphql.org/June2018/#sec-Schema-Introspection

At this point, one might naively attempt to generate two GraphQL schemas:

- One without the __type and __schema fields, from which we generate the data
  required for responding to introspection queries.
- One with the __type and __schema fields, which is used to actually respond to
  queries.

However, this is also not GraphQL-compliant!

The problem here is that while __type and __schema are not visible fields of the
query root object, their *types*, __Type and __Schema respectively, *must be*
exposed through __type introspection, even though those types never appear as
(transitive) members of the query/mutation/subscription root fields.

So in order to gather the data required for introspection, we follow the
following recipe:

A. Collect type information from the introspection-free GraphQL schema

B. Collect type information from the introspection-only GraphQL schema

C. Stitch together the results of (A) and (B). In particular, the query_root
from (A) is used, and all types from (A) and (B) are used, except for the
query_root obtained in (B). -}

-- | Builds a @Schema@ from GraphQL types for the query_root, mutation_root and
-- subscription_root.
--
-- See Note [What introspection exposes]
buildIntrospectionSchema ::
  P.Type 'Output ->
  Maybe (P.Type 'Output) ->
  Maybe (P.Type 'Output) ->
  Either P.ConflictingDefinitions P.Schema
buildIntrospectionSchema queryRoot' mutationRoot' subscriptionRoot' = do
  let -- The only directives that we currently expose over introspection are our
      -- statically defined ones.  So, for instance, we don't correctly expose
      -- directives from remote schemas.
      directives :: [DirectiveInfo] = directivesInfo @P.Parse

  -- Collect type information of all fields
  --
  -- TODO: it may be worth looking at whether we can stop collecting
  -- introspection types monadically. They are independent of the user schema;
  -- the types here are always the same and specified by the GraphQL spec
  allTypes <-
    P.collectTypeDefinitions
      [ P.TypeDefinitionsWrapper queryRoot',
        P.TypeDefinitionsWrapper mutationRoot',
        P.TypeDefinitionsWrapper subscriptionRoot',
        P.TypeDefinitionsWrapper $ P.diArguments =<< directives,
        -- The __schema introspection field
        P.TypeDefinitionsWrapper $ fDefinition (schema @Parse),
        -- The __type introspection field
        P.TypeDefinitionsWrapper $ fDefinition (typeIntrospection @Parse)
      ]

  pure
    $ P.Schema
      { sDescription = Nothing,
        sTypes = allTypes,
        sQueryType = queryRoot',
        sMutationType = mutationRoot',
        sSubscriptionType = subscriptionRoot',
        sDirectives = directives
      }

-- | Generate a __type introspection parser
typeIntrospection ::
  forall n.
  (MonadParse n) =>
  FieldParser n (Schema -> J.Value)
{-# INLINE typeIntrospection #-}
typeIntrospection = do
  let nameArg :: P.InputFieldsParser n Text
      nameArg = P.field GName._name Nothing P.string
  ~(nameText, printer) <- P.subselection GName.___type Nothing nameArg typeField
  -- We pass around the GraphQL schema information under the name `partialSchema`,
  -- because the GraphQL spec forces us to expose a hybrid between the
  -- specification of valid queries (including introspection) and an
  -- introspection-free GraphQL schema.  See Note [What introspection exposes].
  pure $ \partialSchema -> fromMaybe J.Null $ do
    name <- G.mkName nameText
    P.SomeDefinitionTypeInfo def <- HashMap.lookup name $ sTypes partialSchema
    Just $ printer $ SomeType $ P.TNamed P.Nullable def

-- | Generate a __schema introspection parser.
schema ::
  forall n.
  (MonadParse n) =>
  FieldParser n (Schema -> J.Value)
{-# INLINE schema #-}
schema = P.subselection_ GName.___schema Nothing schemaSet

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

data SomeType = forall k. SomeType (P.Type k)

typeField ::
  forall n.
  (MonadParse n) =>
  Parser 'Output n (SomeType -> J.Value)
typeField =
  let includeDeprecated :: P.InputFieldsParser n Bool
      includeDeprecated =
        P.fieldWithDefault GName._includeDeprecated Nothing (G.VBoolean False) (P.nullable P.boolean)
          <&> fromMaybe False
      kind :: FieldParser n (SomeType -> J.Value)
      kind =
        P.selection_ GName._kind Nothing typeKind
          $> \case
            SomeType tp ->
              case tp of
                P.TList P.NonNullable _ ->
                  J.String "NON_NULL"
                P.TNamed P.NonNullable _ ->
                  J.String "NON_NULL"
                P.TList P.Nullable _ ->
                  J.String "LIST"
                P.TNamed P.Nullable (P.Definition _ _ _ _ P.TIScalar) ->
                  J.String "SCALAR"
                P.TNamed P.Nullable (P.Definition _ _ _ _ (P.TIEnum _)) ->
                  J.String "ENUM"
                P.TNamed P.Nullable (P.Definition _ _ _ _ (P.TIInputObject _)) ->
                  J.String "INPUT_OBJECT"
                P.TNamed P.Nullable (P.Definition _ _ _ _ (P.TIObject _)) ->
                  J.String "OBJECT"
                P.TNamed P.Nullable (P.Definition _ _ _ _ (P.TIInterface _)) ->
                  J.String "INTERFACE"
                P.TNamed P.Nullable (P.Definition _ _ _ _ (P.TIUnion _)) ->
                  J.String "UNION"
      name :: FieldParser n (SomeType -> J.Value)
      name =
        P.selection_ GName._name Nothing P.string
          $> \case
            SomeType tp ->
              case tp of
                P.TNamed P.Nullable (P.Definition name' _ _ _ _) ->
                  nameAsJSON name'
                _ -> J.Null
      description :: FieldParser n (SomeType -> J.Value)
      description =
        P.selection_ GName._description Nothing P.string
          $> \case
            SomeType (P.TNamed _ (P.Definition _ (Just desc) _ _ _)) ->
              J.String (G.unDescription desc)
            _ -> J.Null
      fields :: FieldParser n (SomeType -> J.Value)
      fields = do
        -- TODO handle the value of includeDeprecated
        ~(_includeDeprecated, printer) <- P.subselection GName._fields Nothing includeDeprecated fieldField
        return
          $ \case
            SomeType tp ->
              case tp of
                P.TNamed P.Nullable (P.Definition _ _ _ _ (P.TIObject (P.ObjectInfo fields' _interfaces'))) ->
                  J.Array $ V.fromList $ printer <$> fields'
                P.TNamed P.Nullable (P.Definition _ _ _ _ (P.TIInterface (P.InterfaceInfo fields' _objects'))) ->
                  J.Array $ V.fromList $ printer <$> fields'
                _ -> J.Null
      interfaces :: FieldParser n (SomeType -> J.Value)
      interfaces = do
        printer <- P.subselection_ GName._interfaces Nothing typeField
        return
          $ \case
            SomeType tp ->
              case tp of
                P.TNamed P.Nullable (P.Definition _ _ _ _ (P.TIObject (P.ObjectInfo _fields' interfaces'))) ->
                  J.Array $ V.fromList $ printer . SomeType . P.TNamed P.Nullable . fmap P.TIInterface <$> interfaces'
                _ -> J.Null
      possibleTypes :: FieldParser n (SomeType -> J.Value)
      possibleTypes = do
        printer <- P.subselection_ GName._possibleTypes Nothing typeField
        return
          $ \case
            SomeType tp ->
              case tp of
                P.TNamed P.Nullable (P.Definition _ _ _ _ (P.TIInterface (P.InterfaceInfo _fields' objects'))) ->
                  J.Array $ V.fromList $ printer . SomeType . P.TNamed P.Nullable . fmap P.TIObject <$> objects'
                P.TNamed P.Nullable (P.Definition _ _ _ _ (P.TIUnion (P.UnionInfo objects'))) ->
                  J.Array $ V.fromList $ printer . SomeType . P.TNamed P.Nullable . fmap P.TIObject <$> objects'
                _ -> J.Null
      enumValues :: FieldParser n (SomeType -> J.Value)
      enumValues = do
        -- TODO handle the value of includeDeprecated
        ~(_includeDeprecated, printer) <- P.subselection GName._enumValues Nothing includeDeprecated enumValue
        return
          $ \case
            SomeType tp ->
              case tp of
                P.TNamed P.Nullable (P.Definition _ _ _ _ (P.TIEnum vals)) ->
                  J.Array $ V.fromList $ fmap printer $ toList vals
                _ -> J.Null
      inputFields :: FieldParser n (SomeType -> J.Value)
      inputFields = do
        printer <- P.subselection_ GName._inputFields Nothing inputValue
        return
          $ \case
            SomeType tp ->
              case tp of
                P.TNamed P.Nullable (P.Definition _ _ _ _ (P.TIInputObject (P.InputObjectInfo fieldDefs))) ->
                  J.Array $ V.fromList $ map printer fieldDefs
                _ -> J.Null
      -- ofType peels modalities off of types
      ofType :: FieldParser n (SomeType -> J.Value)
      ofType = do
        printer <- P.subselection_ GName._ofType Nothing typeField
        return $ \case
          -- kind = "NON_NULL": !a -> a
          SomeType (P.TNamed P.NonNullable x) ->
            printer $ SomeType $ P.TNamed P.Nullable x
          -- kind = "NON_NULL": ![a] -> [a], and ![!a] -> [!a]
          SomeType (P.TList P.NonNullable x) ->
            printer $ SomeType $ P.TList P.Nullable x
          -- kind = "LIST": [a] -> a
          SomeType (P.TList P.Nullable x) ->
            printer $ SomeType x
          _ -> J.Null
   in applyPrinter
        <$> P.selectionSet
          GName.___Type
          Nothing
          [ kind,
            name,
            description,
            fields,
            interfaces,
            possibleTypes,
            enumValues,
            inputFields,
            ofType
          ]

{-
type __InputValue {
  name: String!
  description: String
  type: __Type!
  defaultValue: String
}
-}
inputValue ::
  forall n.
  (MonadParse n) =>
  Parser 'Output n (P.Definition P.InputFieldInfo -> J.Value)
inputValue =
  let name :: FieldParser n (P.Definition P.InputFieldInfo -> J.Value)
      name =
        P.selection_ GName._name Nothing P.string
          $> nameAsJSON
          . P.dName
      description :: FieldParser n (P.Definition P.InputFieldInfo -> J.Value)
      description =
        P.selection_ GName._description Nothing P.string
          $> maybe J.Null (J.String . G.unDescription)
          . P.dDescription
      typeF :: FieldParser n (P.Definition P.InputFieldInfo -> J.Value)
      typeF = do
        printer <- P.subselection_ GName._type Nothing typeField
        return $ \defInfo -> case P.dInfo defInfo of
          P.InputFieldInfo tp _ -> printer $ SomeType tp
      defaultValue :: FieldParser n (P.Definition P.InputFieldInfo -> J.Value)
      defaultValue =
        P.selection_ GName._defaultValue Nothing P.string
          $> \defInfo -> case P.dInfo defInfo of
            P.InputFieldInfo _ (Just val) -> J.String $ T.run $ GP.value val
            _ -> J.Null
   in applyPrinter
        <$> P.selectionSet
          GName.___InputValue
          Nothing
          [ name,
            description,
            typeF,
            defaultValue
          ]

{-
type __EnumValue {
  name: String!
  description: String
  isDeprecated: Boolean!
  deprecationReason: String
}
-}
enumValue ::
  forall n.
  (MonadParse n) =>
  Parser 'Output n (P.Definition P.EnumValueInfo -> J.Value)
enumValue =
  let name :: FieldParser n (P.Definition P.EnumValueInfo -> J.Value)
      name =
        P.selection_ GName._name Nothing P.string
          $> nameAsJSON
          . P.dName
      description :: FieldParser n (P.Definition P.EnumValueInfo -> J.Value)
      description =
        P.selection_ GName._description Nothing P.string
          $> maybe J.Null (J.String . G.unDescription)
          . P.dDescription
      -- TODO We don't seem to support enum value deprecation
      isDeprecated :: FieldParser n (P.Definition P.EnumValueInfo -> J.Value)
      isDeprecated =
        P.selection_ GName._isDeprecated Nothing P.string
          $> const (J.Bool False)
      deprecationReason :: FieldParser n (P.Definition P.EnumValueInfo -> J.Value)
      deprecationReason =
        P.selection_ GName._deprecationReason Nothing P.string
          $> const J.Null
   in applyPrinter
        <$> P.selectionSet
          GName.___EnumValue
          Nothing
          [ name,
            description,
            isDeprecated,
            deprecationReason
          ]

{-
enum __TypeKind {
  ENUM
  INPUT_OBJECT
  INTERFACE
  LIST
  NON_NULL
  OBJECT
  SCALAR
  UNION
}
-}
typeKind ::
  forall n.
  (MonadParse n) =>
  Parser 'Both n ()
typeKind =
  P.enum
    GName.___TypeKind
    Nothing
    ( NE.fromList
        [ mkDefinition GName._ENUM,
          mkDefinition GName._INPUT_OBJECT,
          mkDefinition GName._INTERFACE,
          mkDefinition GName._LIST,
          mkDefinition GName._NON_NULL,
          mkDefinition GName._OBJECT,
          mkDefinition GName._SCALAR,
          mkDefinition GName._UNION
        ]
    )
  where
    mkDefinition name = (P.Definition name Nothing Nothing [] P.EnumValueInfo, ())

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
fieldField ::
  forall n.
  (MonadParse n) =>
  Parser 'Output n (P.Definition P.FieldInfo -> J.Value)
fieldField =
  let name :: FieldParser n (P.Definition P.FieldInfo -> J.Value)
      name =
        P.selection_ GName._name Nothing P.string
          $> nameAsJSON
          . P.dName
      description :: FieldParser n (P.Definition P.FieldInfo -> J.Value)
      description =
        P.selection_ GName._description Nothing P.string $> \defInfo ->
          case P.dDescription defInfo of
            Nothing -> J.Null
            Just desc -> J.String (G.unDescription desc)
      args :: FieldParser n (P.Definition P.FieldInfo -> J.Value)
      args = do
        printer <- P.subselection_ GName._args Nothing inputValue
        return $ J.Array . V.fromList . map printer . sortOn P.dName . P.fArguments . P.dInfo
      typeF :: FieldParser n (P.Definition P.FieldInfo -> J.Value)
      typeF = do
        printer <- P.subselection_ GName._type Nothing typeField
        return $ printer . (\case P.FieldInfo _ tp -> SomeType tp) . P.dInfo
      -- TODO We don't seem to track deprecation info
      isDeprecated :: FieldParser n (P.Definition P.FieldInfo -> J.Value)
      isDeprecated =
        P.selection_ GName._isDeprecated Nothing P.string
          $> const (J.Bool False)
      deprecationReason :: FieldParser n (P.Definition P.FieldInfo -> J.Value)
      deprecationReason =
        P.selection_ GName._deprecationReason Nothing P.string
          $> const J.Null
   in applyPrinter
        <$> P.selectionSet
          GName.___Field
          Nothing
          [ name,
            description,
            args,
            typeF,
            isDeprecated,
            deprecationReason
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

directiveSet ::
  forall n.
  (MonadParse n) =>
  Parser 'Output n (P.DirectiveInfo -> J.Value)
directiveSet =
  let name :: FieldParser n (P.DirectiveInfo -> J.Value)
      name =
        P.selection_ GName._name Nothing P.string
          $> (J.toOrdered . P.diName)
      description :: FieldParser n (P.DirectiveInfo -> J.Value)
      description =
        P.selection_ GName._description Nothing P.string
          $> (J.toOrdered . P.diDescription)
      locations :: FieldParser n (P.DirectiveInfo -> J.Value)
      locations =
        P.selection_ GName._locations Nothing P.string
          $> (J.toOrdered . map showDirLoc . P.diLocations)
      args :: FieldParser n (P.DirectiveInfo -> J.Value)
      args = do
        printer <- P.subselection_ GName._args Nothing inputValue
        pure $ J.array . map printer . P.diArguments
      isRepeatable :: FieldParser n (P.DirectiveInfo -> J.Value)
      isRepeatable =
        P.selection_ GName._isRepeatable Nothing P.string
          $> const J.Null
   in applyPrinter
        <$> P.selectionSet
          GName.___Directive
          Nothing
          [ name,
            description,
            locations,
            args,
            isRepeatable
          ]
  where
    showDirLoc :: G.DirectiveLocation -> Text
    showDirLoc = \case
      G.DLExecutable edl -> T.pack $ drop 3 $ show edl
      G.DLTypeSystem tsdl -> T.pack $ drop 4 $ show tsdl

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
schemaSet ::
  forall n.
  (MonadParse n) =>
  Parser 'Output n (Schema -> J.Value)
{-# INLINE schemaSet #-}
schemaSet =
  let description :: FieldParser n (Schema -> J.Value)
      description =
        P.selection_ GName._description Nothing P.string
          $> \partialSchema -> case sDescription partialSchema of
            Nothing -> J.Null
            Just s -> J.String $ G.unDescription s
      types :: FieldParser n (Schema -> J.Value)
      types = do
        printer <- P.subselection_ GName._types Nothing typeField
        return
          $ \partialSchema ->
            J.Array
              $ V.fromList
              $ map (printer . schemaTypeToSomeType)
              $ sortOn P.getName
              $ HashMap.elems
              $ sTypes partialSchema
        where
          schemaTypeToSomeType :: P.SomeDefinitionTypeInfo -> SomeType
          schemaTypeToSomeType (P.SomeDefinitionTypeInfo def) =
            SomeType $ P.TNamed P.Nullable def
      queryType :: FieldParser n (Schema -> J.Value)
      queryType = do
        printer <- P.subselection_ GName._queryType Nothing typeField
        return $ \partialSchema -> printer $ SomeType $ sQueryType partialSchema
      mutationType :: FieldParser n (Schema -> J.Value)
      mutationType = do
        printer <- P.subselection_ GName._mutationType Nothing typeField
        return $ \partialSchema -> case sMutationType partialSchema of
          Nothing -> J.Null
          Just tp -> printer $ SomeType tp
      subscriptionType :: FieldParser n (Schema -> J.Value)
      subscriptionType = do
        printer <- P.subselection_ GName._subscriptionType Nothing typeField
        return $ \partialSchema -> case sSubscriptionType partialSchema of
          Nothing -> J.Null
          Just tp -> printer $ SomeType tp
      directives :: FieldParser n (Schema -> J.Value)
      directives = do
        printer <- P.subselection_ GName._directives Nothing directiveSet
        return $ \partialSchema -> J.array $ map printer $ sDirectives partialSchema
   in applyPrinter
        <$> P.selectionSet
          GName.___Schema
          Nothing
          [ description,
            types,
            queryType,
            mutationType,
            subscriptionType,
            directives
          ]

selectionSetToJSON ::
  InsOrdHashMap.InsOrdHashMap G.Name J.Value ->
  J.Value
selectionSetToJSON = J.object . map (first G.unName) . InsOrdHashMap.toList

applyPrinter ::
  InsOrdHashMap.InsOrdHashMap G.Name (P.ParsedSelection (a -> J.Value)) ->
  a ->
  J.Value
applyPrinter = flip (\x -> selectionSetToJSON . fmap (($ x) . P.handleTypename (const . nameAsJSON)))

nameAsJSON :: (P.HasName a) => a -> J.Value
nameAsJSON = J.String . G.unName . P.getName
