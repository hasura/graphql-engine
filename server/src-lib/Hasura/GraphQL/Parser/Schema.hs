{-# LANGUAGE PatternSynonyms #-}

-- | Types for representing a GraphQL schema.
module Hasura.GraphQL.Parser.Schema
  ( -- * Kinds
    Kind (..),
    (:<:) (..),
    type (<:) (..),

    -- * Types
    MkTypename (..),
    mkTypename,
    withTypenameCustomization,
    Nullability (..),
    Type (..),
    TypeInfo (TIScalar, TIEnum, TIInputObject, TIObject, TIInterface, TIUnion),
    getTypeInfo,
    SomeDefinitionTypeInfo (..),
    eqType,
    eqTypeInfo,
    typeNullability,
    nullableType,
    nonNullableType,
    toGraphQLType,
    getObjectInfo,
    getInterfaceInfo,
    EnumValueInfo (..),
    InputFieldInfo (..),
    FieldInfo (..),
    InputObjectInfo (InputObjectInfo),
    ObjectInfo (ObjectInfo, oiFields, oiImplements),
    InterfaceInfo (InterfaceInfo, iiFields, iiPossibleTypes),
    UnionInfo (UnionInfo, uiPossibleTypes),

    -- * Definitions
    Definition (..),

    -- * Schemas
    Schema (..),
    ConflictingDefinitions (..),
    HasTypeDefinitions (..),
    TypeDefinitionsWrapper (..),
    collectTypeDefinitions,

    -- * Miscellany
    HasName (..),
    InputValue (..),
    Variable (..),
    VariableInfo (..),
    DirectiveInfo (..),
  )
where

import Control.Lens
import Data.Aeson qualified as J
import Data.Functor.Classes
import Data.Has
import Data.HashMap.Strict.Extended qualified as Map
import Data.Hashable (Hashable (..))
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Monoid
import Data.Text qualified as T
import Data.Text.Extended
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Language.GraphQL.Draft.Syntax
  ( Description (..),
    DirectiveLocation (..),
    GType (..),
    Name (..),
    Value (..),
  )
import Language.GraphQL.Draft.Syntax qualified as G
import Language.Haskell.TH.Lift qualified as TH

class HasName a where
  getName :: a -> Name

instance HasName Name where
  getName = id

-- | Type name customization
newtype MkTypename = MkTypename {runMkTypename :: Name -> Name}
  deriving (Semigroup, Monoid) via (Endo Name)

-- | Inject a new @MkTypename@ customization function into the environment.
-- This can be used by schema-building code (with @MonadBuildSchema@ constraint) to ensure
-- the correct type name customizations are applied.
withTypenameCustomization :: forall m r a. (MonadReader r m, Has MkTypename r) => MkTypename -> m a -> m a
withTypenameCustomization = local . set hasLens

-- | Apply the type name customization function from the current environment.
mkTypename :: (MonadReader r m, Has MkTypename r) => Name -> m Name
mkTypename name =
  ($ name) . runMkTypename <$> asks getter

-- | GraphQL types are divided into two classes: input types and output types.
-- The GraphQL spec does not use the word “kind” to describe these classes, but
-- it’s an apt term.
--
-- Some GraphQL types can be used at either kind, so we also include the 'Both'
-- kind, the superkind of both 'Input' and 'Output'. The '<:' class provides
-- kind subsumption constraints.
--
-- For more details, see <http://spec.graphql.org/June2018/#sec-Input-and-Output-Types>.
data Kind
  = -- | see Note [The 'Both kind]
    Both
  | Input
  | Output

{- Note [The 'Both kind]
~~~~~~~~~~~~~~~~~~~~~~~~
As described in the Haddock comments for Kind and <:, we use Kind to index
various types, such as Type and Parser. We use this to enforce various
correctness constraints mandated by the GraphQL spec; for example, we don’t
allow input object fields to have output types and we don’t allow output object
fields to have input types.

But scalars and enums can be used as input types *or* output types. A natural
encoding of that in Haskell would be to make constructors for those types
polymorphic, like this:

    data Kind = Input | Output

    data TypeInfo k where
      TIScalar      :: TypeInfo k           -- \ Polymorphic!
      TIEnum        :: ... -> TypeInfo k    -- /
      TIInputObject :: ... -> TypeInfo 'Input
      TIObject      :: ... -> TypeInfo 'Output

Naturally, this would give the `scalar` parser constructor a similarly
polymorphic type:

    scalar
      :: MonadParse m
      => Name
      -> Maybe Description
      -> ScalarRepresentation a
      -> Parser k m a             -- Polymorphic!

But if we actually try that, we run into problems. The trouble is that we want
to use the Kind to influence several different things:

  * As mentioned above, we use it to ensure that the types we generate are
    well-kinded according to the GraphQL spec rules.

  * We use it to determine what a Parser consumes as input. Parsers for input
    types parse GraphQL input values, but Parsers for output types parse
    selection sets. (See Note [The meaning of Parser 'Output] in
    Hasura.GraphQL.Parser.Internal.Parser for an explanation of why.)

  * We use it to know when to expect a sub-selection set for a field of an
    output object (see Note [The delicate balance of GraphQL kinds]).

These many uses of Kind cause some trouble for a polymorphic representation. For
example, consider our `scalar` parser constructor above---if we were to
instantiate it at kind 'Output, we’d receive a `Parser 'Output`, which we would
then expect to be able to apply to a selection set. But that doesn’t make any
sense, since scalar fields don’t have selection sets!

Another issue with this representation has to do with effectful parser
constructors (such as constructors that can throw errors). These have types like

    mkFooParser :: MonadSchema n m => Blah -> m (Parser k n Foo)

where the parser construction is itself monadic. This causes some annoyance,
since even if mkFooParser returns a Parser of a polymorphic kind, code like this
will not typecheck:

    (fooParser :: forall k. Parser k n Foo) <- mkFooParser blah

The issue is that we have to instantiate k to a particular type to be able to
call mkFooParser. If we want to use the result at both kinds, we’d have to call
mkFooParser twice:

    (fooInputParser :: Parser 'Input n Foo) <- mkFooParser blah
    (fooOutputParser :: Parser 'Output n Foo) <- mkFooParser blah

Other situations encounter similar difficulties, and they are not easy to
resolve without impredicative polymorphism (which GHC does not support).

To avoid this problem, we don’t use polymorphic kinds, but instead introduce a
form of kind subsumption. Types that can be used as both input and output types
are explicitly given the kind 'Both. This allows us to get the best of both
worlds:

  * We use the <: typeclass to accept 'Both in most places where we expect
    either input or output types.

  * We can treat 'Both specially to avoid requiring `scalar` to supply a
    selection set parser (see Note [The delicate balance of GraphQL kinds] for
    further explanation).

  * Because we avoid the polymorphism, we don’t run into the aforementioned
    issue with monadic parser constructors.

All of this is subtle and somewhat complicated, but unfortunately there isn’t
much of a way around that: GraphQL is subtle and complicated. Our use of an
explicit 'Both kind isn’t the only way to encode these things, but it’s the
particular set of compromises we’ve chosen to accept.

Note [The delicate balance of GraphQL kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As discussed in Note [The 'Both kind], we use GraphQL kinds to distinguish
several different things. One of them is which output types take sub-selection
sets. For example, scalars don’t accept sub-selection sets, so if we have a
schema like

    type Query {
      users: [User!]!
    }

    type User {
      id: Int!
    }

then the following query is illegal:

    query {
      users {
        id {
          blah
        }
      }
    }

The id field has a scalar type, so it should not take a sub-selection set. This
is actually something we care about distinguishing at the type level, because it
affects the type of the `selection` parser combinator. Suppose we have a
`Parser 'Output m UserQuery` for the User type. When we parse a field with that
type, we expect to receive a UserQuery as a result, unsurprisingly. But what if
we parse an output field using the `int` parser, which has this type:

    int :: MonadParse m => Parser 'Both m Int32

If we follow the same logic as for the User parser above, we’d expect to receive
an Int32 as a result... but that doesn’t make any sense, since the Int32
corresponds to the result *we* are suppose to produce as a result of executing
the query, not something user-specified.

One way to solve this would be to associate every Parser with two result types:
one when given an input object, and one when given a selection set. Then our
parsers could be given these types, instead:

    user :: MonadParse m => Parser 'Output m Void UserQuery
    int :: MonadParse m => Parser 'Both m Int32 ()

But if you work through this, you’ll find that *all* parsers will either have
Void or () for at least one of their input result types or their output result
types, depending on their kind:

  * All 'Input parsers must have Void for their output result type, since they
    aren’t allowed to be used in output contexts at all.

  * All 'Output parsers must have Void for their input result type, since they
    aren’t allowed to be used in input contexts at all.

  * That just leaves 'Both. The only types of kind 'Both are scalars and enums,
    neither of which accept a sub-selection set. Their output result type would
    therefore be (), since they are allowed to appear in output contexts, but
    they don’t return any results.

The end result of this is that we clutter all our types with Voids and ()s, with
little actual benefit.

If you really think about it, the fact that the no types of kind 'Both accept a
sub-selection set is really something of a coincidence. In theory, one could
imagine a future version of the GraphQL spec adding a type that can be used as
both an input type or an output type, but accepts a sub-selection set. If that
ever happens, we’ll have to tweak our encoding, but for now, we can take
advantage of this happy coincidence and make the kinds serve double duty:

  * We can make `ParserInput 'Both` identical to `ParserInput 'Input`, since
    all parsers of kind 'Both only parse input values.

  * We can require types of kind 'Both in `selection`, which does not expect a
    sub-selection set, and types of kind 'Output in `subselection`, which does.

Relying on this coincidence might seem a little gross, and perhaps it is
somewhat. But it’s enormously convenient: not doing this would make some types
significantly more complicated, since we would have to thread around more
information at the type level and we couldn’t make as many simplifying
assumptions. So until GraphQL adds a type that violates these assumptions, we
are happy to take advantage of this coincidence. -}

-- | Evidence for '<:'.
data k1 :<: k2 where
  KRefl :: k :<: k
  KBoth :: k :<: 'Both

-- | 'Kind' subsumption. The GraphQL kind hierarchy is extremely simple:
--
-- >     Both
-- >     /  \
-- > Input  Output
--
-- Various functions in this module use '<:' to allow 'Both' to be used in
-- places where 'Input' or 'Output' would otherwise be expected.
class k1 <: k2 where
  subKind :: k1 :<: k2

instance k1 ~ k2 => k1 <: k2 where
  subKind = KRefl

instance {-# OVERLAPPING #-} k <: 'Both where
  subKind = KBoth

data Nullability = Nullable | NonNullable
  deriving (Eq)

isNullable :: Nullability -> Bool
isNullable Nullable = True
isNullable NonNullable = False

data Type k
  = TNamed Nullability (Definition (TypeInfo k))
  | TList Nullability (Type k)

instance Eq (Type k) where
  (==) = eqType

-- | Like '==', but can compare 'Type's of different kinds.
eqType :: Type k1 -> Type k2 -> Bool
eqType (TNamed n a) (TNamed n' b) = n == n' && liftEq eqTypeInfo a b
eqType (TList n a) (TList n' b) = n == n' && eqType a b
eqType _ _ = False

instance HasName (Type k) where
  getName (TNamed _ def) = getName def
  getName (TList _ t) = getName t

typeNullability :: Type k -> Nullability
typeNullability (TNamed n _) = n
typeNullability (TList n _) = n

nullableType :: Type k -> Type k
nullableType (TNamed _ def) = TNamed Nullable def
nullableType (TList _ t) = TList Nullable t

nonNullableType :: Type k -> Type k
nonNullableType (TNamed _ def) = TNamed NonNullable def
nonNullableType (TList _ t) = TList NonNullable t

toGraphQLType :: Type k -> GType
toGraphQLType (TNamed n typeInfo) = TypeNamed (G.Nullability (isNullable n)) $ getName typeInfo
toGraphQLType (TList n typeInfo) = TypeList (G.Nullability (isNullable n)) $ toGraphQLType typeInfo

{- Note [The interfaces story]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GraphQL interfaces are not conceptually complicated, but they pose some
non-obvious challenges for our implementation. First, familiarize yourself with
GraphQL interfaces themselves:

  * https://graphql.org/learn/schema/#interfaces
  * http://spec.graphql.org/June2018/#sec-Interfaces
  * http://spec.graphql.org/June2018/#sec-Objects

The most logical repesentation of object and interface types is to have objects
reference the interfaces they implement, but not the other way around. After
all, that’s how it works in the GraphQL language: when you declare an interface,
you just specify its fields, and you specify which interfaces each object type
implements as part of their declarations.

However, this representation is actually not very useful for us. We /also/ need
the interfaces to reference the objects that implement them---forming a circular
structure---for two reasons:

  1. Most directly, we need this information for introspection queries.
     Introspection queries for object types return the set of interfaces they
     implement <http://spec.graphql.org/June2018/#sec-Object>, and introspection
     queries for interfaces return the set of object types that implement them
     <http://spec.graphql.org/June2018/#sec-Interface>.

  2. Less obviously, it’s more natural to specify the relationships “backwards”
     like this when building the schema using the parser combinator language.

     From the parser’s point of view, each implementation of an interface
     corresponds to a distinct parsing possibility. For example, when we
     generate a Relay schema, the type of the `node` root field is an interface,
     and each table is a type that implements it:

         type query_root {
           node(id: ID!): Node
           ...
         }

         interface Node {
           id: ID!
         }

         type author implements Node {
           id: ID!
           name: String!
           ...
         }

         type article implements Node {
           id: ID!
           title: String!
           body: String!
           ...
         }

     A query will use fragments on the Node type to access table-specific fields:

         query get_article_info($article_id: ID!) {
           node(id: $article_id) {
             ... on article {
               title
               body
             }
           }
         }

     The query parser needs to know which types implement the interface (and
     how to parse their selection sets) so that it can parse the fragments.

This presents some complications, since we need to build this information in a
circular fashion. Currently, we do this in a very naïve way:

  * We require selectionSetObject to specify the interfaces it implements /and/
    require selectionSetInterface to specify the objects that implement it.

  * We take advantage of our existing memoization mechanism to do the knot-tying
    for us (see Note [Tying the knot] in Hasura.GraphQL.Parser.Class).

You may notice that this makes it possible for the definitions to be
inconsistent: we could construct an interface parser that parses some object
type, but forget to specify that the object type implements the interface. This
inconsistency is currently completely unchecked, which is quite unfortunate. It
also means we don’t support remote schema-defined object types that implement
interfaces we generate, since we don’t know anything about those types when we
construct the interface.

Since we don’t make very much use of interface types at the time of this
writing, this isn’t much of a problem in practice. But if that changes, it would
be worth implementing a more sophisticated solution that can gather up all the
different sources of information and make sure they’re consistent. -}

-- | Invariant: the list is sorted by 'dName'
data InputObjectInfo = InputObjectInfo__ ~[Definition InputFieldInfo]

-- Public interface enforcing invariants
pattern InputObjectInfo :: [Definition InputFieldInfo] -> InputObjectInfo
pattern InputObjectInfo xs <-
  InputObjectInfo__ xs
  where
    InputObjectInfo xs = InputObjectInfo__ (List.sortOn dName xs)

{-# COMPLETE InputObjectInfo #-}

-- Note that we can't check for equality of the fields since there may be
-- circularity. So we rather check for equality of names.
instance Eq InputObjectInfo where
  InputObjectInfo fields1 == InputObjectInfo fields2 =
    eqByName fields1 fields2

-- | Invariant: the lists are sorted by 'dName', maintained via pattern synonyms
data ObjectInfo = ObjectInfo__
  { -- | The fields that this object has.  This consists of the fields of the
    -- interfaces that it implements, as well as any additional fields.
    _oiFields :: ~[Definition FieldInfo],
    -- | The interfaces that this object implements (inheriting all their
    -- fields). See Note [The interfaces story] for more details.
    _oiImplements :: ~[Definition InterfaceInfo]
  }

-- Public interface enforcing invariants
pattern ObjectInfo :: [Definition FieldInfo] -> [Definition InterfaceInfo] -> ObjectInfo
pattern ObjectInfo {oiFields, oiImplements} <-
  ObjectInfo__ oiFields oiImplements
  where
    ObjectInfo xs ys = ObjectInfo__ (List.sortOn dName xs) (List.sortOn dName ys)

{-# COMPLETE ObjectInfo #-}

-- | Note that we can't check for equality of the fields and the interfaces since
-- there may be circularity. So we rather check for equality of names.
--
-- This is dodgy... the equality logic here should I think correspond to the
-- logic in @typeField@ and its neighbors in "Hasura.GraphQL.Schema.Introspect",
-- in terms of how much we recurse.
instance Eq ObjectInfo where
  ObjectInfo fields1 interfaces1 == ObjectInfo fields2 interfaces2 =
    eqByName fields1 fields2 && eqByName interfaces1 interfaces2

-- | Type information for a GraphQL interface; see Note [The interfaces story]
-- for more details.
--
-- Note: in the current working draft of the GraphQL specification (> June
-- 2018), interfaces may implement other interfaces, but we currently don't
-- support this.
--
-- Invariant: the lists are sorted by 'dName', maintained via pattern synonyms
data InterfaceInfo = InterfaceInfo__
  { -- | Fields declared by this interface. Every object implementing this
    -- interface must include those fields.
    _iiFields :: ~[Definition FieldInfo],
    -- | Objects that implement this interface. See Note [The interfaces story]
    -- for why we include that information here.
    _iiPossibleTypes :: ~[Definition ObjectInfo]
  }

-- Public interface enforcing invariants
pattern InterfaceInfo :: [Definition FieldInfo] -> [Definition ObjectInfo] -> InterfaceInfo
pattern InterfaceInfo {iiFields, iiPossibleTypes} <-
  InterfaceInfo__ iiFields iiPossibleTypes
  where
    InterfaceInfo xs ys = InterfaceInfo__ (List.sortOn dName xs) (List.sortOn dName ys)

{-# COMPLETE InterfaceInfo #-}

-- Note that we can't check for equality of the fields and the interfaces since
-- there may be circularity. So we rather check for equality of names.
instance Eq InterfaceInfo where
  InterfaceInfo fields1 objects1 == InterfaceInfo fields2 objects2 =
    eqByName fields1 fields2 && eqByName objects1 objects2

-- | Invariant: the list is sorted by 'dName'
data UnionInfo = UnionInfo__
  { -- | The member object types of this union.
    _uiPossibleTypes :: ~[Definition ObjectInfo]
  }

-- Public interface enforcing invariants
pattern UnionInfo :: [Definition ObjectInfo] -> UnionInfo
pattern UnionInfo {uiPossibleTypes} <-
  UnionInfo__ uiPossibleTypes
  where
    UnionInfo xs = UnionInfo__ (List.sortOn dName xs)

{-# COMPLETE UnionInfo #-}

data TypeInfo k where
  TIScalar :: TypeInfo 'Both
  -- | Invariant: the NonEmpty is sorted by 'dName'
  TIEnum__ :: NonEmpty (Definition EnumValueInfo) -> TypeInfo 'Both
  TIInputObject :: InputObjectInfo -> TypeInfo 'Input
  TIObject :: ObjectInfo -> TypeInfo 'Output
  TIInterface :: InterfaceInfo -> TypeInfo 'Output
  TIUnion :: UnionInfo -> TypeInfo 'Output

-- Public interface enforcing invariants
pattern TIEnum :: forall (k :: Kind). () => (k ~ 'Both) => NonEmpty (Definition EnumValueInfo) -> TypeInfo k
pattern TIEnum xs <-
  TIEnum__ xs
  where
    TIEnum xs = TIEnum__ (NE.sortWith dName xs)

{-# COMPLETE TIScalar, TIEnum, TIInputObject, TIObject, TIInterface, TIUnion #-}

instance Eq (TypeInfo k) where
  (==) = eqTypeInfo

-- | Like '==', but can compare 'TypeInfo's of different kinds.
eqTypeInfo :: TypeInfo k1 -> TypeInfo k2 -> Bool
eqTypeInfo TIScalar TIScalar = True
eqTypeInfo (TIEnum values1) (TIEnum values2) = values1 == values2
-- NB the case for input objects currently has quadratic complexity, which is
-- probably avoidable. HashSets should be able to get this down to
-- O(n*log(n)). But this requires writing some Hashable instances by hand
-- because we use some existential types and GADTs.
eqTypeInfo (TIInputObject ioi1) (TIInputObject ioi2) = ioi1 == ioi2
eqTypeInfo (TIObject oi1) (TIObject oi2) = oi1 == oi2
eqTypeInfo (TIInterface ii1) (TIInterface ii2) = ii1 == ii2
eqTypeInfo (TIUnion (UnionInfo objects1)) (TIUnion (UnionInfo objects2)) =
  eqByName objects1 objects2
eqTypeInfo _ _ = False

getTypeInfo :: Type k -> Definition (TypeInfo k)
getTypeInfo (TNamed _ d) = d
getTypeInfo (TList _ t) = getTypeInfo t

getObjectInfo :: Type k -> Maybe (Definition ObjectInfo)
getObjectInfo t = case getTypeInfo t of
  d@Definition {dInfo = TIObject oi} -> Just d {dInfo = oi}
  _ -> Nothing

getInterfaceInfo :: Type k -> Maybe (Definition InterfaceInfo)
getInterfaceInfo t = case getTypeInfo t of
  d@Definition {dInfo = TIInterface ii} -> Just d {dInfo = ii}
  _ -> Nothing

data SomeDefinitionTypeInfo = forall k. SomeDefinitionTypeInfo (Definition (TypeInfo k))

instance HasName SomeDefinitionTypeInfo where
  getName (SomeDefinitionTypeInfo (Definition n _ _)) = n

instance Eq SomeDefinitionTypeInfo where
  -- Same as instance Eq Definition
  SomeDefinitionTypeInfo (Definition name1 _ ti1)
    == SomeDefinitionTypeInfo (Definition name2 _ ti2) =
      name1 == name2 && eqTypeInfo ti1 ti2

data Definition a = Definition
  { dName :: Name,
    dDescription :: Maybe Description,
    -- | Lazy to allow mutually-recursive type definitions.
    dInfo :: ~a
  }
  deriving (Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (Definition a) where
  hashWithSalt salt Definition {..} =
    salt `hashWithSalt` dName `hashWithSalt` dInfo

instance Eq a => Eq (Definition a) where
  (==) = eq1

instance Eq1 Definition where
  liftEq
    eq
    (Definition name1 _ info1)
    (Definition name2 _ info2) =
      name1 == name2 && eq info1 info2

instance HasName (Definition a) where
  getName = dName

-- | equivalent to, but faster than...
--
-- > map dName x == map dName y
eqByName :: [Definition a] -> [Definition a] -> Bool
eqByName = liftEq ((==) `on` dName)

-- | Enum values have no extra information except for the information common to
-- all definitions, so this is just a placeholder for use as @'Definition'
-- 'EnumValueInfo'@.
data EnumValueInfo = EnumValueInfo
  deriving (Eq, Generic)

instance Hashable EnumValueInfo

data InputFieldInfo
  = -- | An input field with a type and possibly a default value. If a default
    -- value is provided, it should be a valid value for the type.
    --
    -- Note that a default value of 'VNull' is subtly different from having no
    -- default value at all. If no default value is provided (i.e. 'Nothing'),
    -- the GraphQL specification allows distinguishing provided @null@ values
    -- from values left completely absent; see Note [The value of omitted
    -- fields] in Hasura.GraphQL.Parser.Internal.Parser.
    forall k. ('Input <: k) => InputFieldInfo (Type k) (Maybe (Value Void))

instance Eq InputFieldInfo where
  InputFieldInfo t1 v1 == InputFieldInfo t2 v2 = eqType t1 t2 && v1 == v2

data FieldInfo = forall k.
  ('Output <: k) =>
  FieldInfo
  { fArguments :: [Definition InputFieldInfo],
    fType :: Type k
  }

instance Eq FieldInfo where
  FieldInfo args1 t1 == FieldInfo args2 t2 = args1 == args2 && eqType t1 t2

{- Note [Parsing variable values]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GraphQL includes its own tiny language for input values, which is similar to
JSON but not quite the same---GraphQL input values can be enum values, and there
are restrictions on the names of input object keys. Despite these differences,
variables’ values are passed as JSON, so we actually need to be able to parse
values expressed in both languages.

It’s tempting to contain this complexity by simply converting the JSON values to
GraphQL input values up front, and for booleans, numbers, arrays, and most
objects, this conversion is viable. But JSON strings pose a problem, since they
are used to represent both GraphQL strings and GraphQL enums. For example,
consider a query like this:

    enum FooBar {
      FOO
      BAR
    }

    query some_query($a: String, $b: FooBar) {
      ...
    }

We might receive an accompany variables payload like this:

    {
      "a": "FOO",
      "b": "FOO"
    }

To properly convert these JSON values to GraphQL, we’d need to use the type
information to guide the parsing. Since $a has type String, its value should be
parsed as the GraphQL string "FOO", while $b has type FooBar, so its value
should be parsed as the GraphQL enum value FOO.

We could do this type-directed parsing, but there are some advantages to being
lazier. For one, we can use JSON values directly when used as a column value of
type json or jsonb, rather than converting them to GraphQL and back; which, in
turn, solves another problem with JSON objects: JSON object keys are arbitrary
strings, while GraphQL input object keys are GraphQL names, and therefore
restricted: not all JSON objects can be represented by a GraphQL input object.

Arguably such columns should really be represented as strings containing encoded
JSON, not GraphQL lists/objects, but the decision to treat them otherwise is
old, and it would be backwards-incompatible to change now. We can also avoid
needing to interpret the values of variables for types outside our control
(i.e. those from a remote schema), which can be useful in the case of custom
scalars or extensions of the GraphQL protocol.

So instead we use the InputValue type to represent that an input value might be
a GraphQL literal value or a JSON value from the variables payload. This means
each input parser constructor needs to be able to parse both GraphQL values and
JSON values, but fortunately, the duplication of logic is minimal. -}

-- | See Note [Parsing variable values].
data InputValue v
  = GraphQLValue (Value v)
  | JSONValue J.Value
  deriving (Show, Eq, Functor, Generic, Ord, TH.Lift)

instance (Hashable v) => Hashable (InputValue v)

instance (Cacheable v) => Cacheable (InputValue v)

data Variable = Variable
  { vInfo :: VariableInfo,
    vType :: GType,
    -- | Note: if the variable was null or was not provided and the field has a
    -- non-null default value, this field contains the default value, not 'VNull'.
    vValue :: InputValue Void
  }
  deriving (Show, Eq, Generic, Ord, TH.Lift)

instance Hashable Variable

instance Cacheable Variable

data VariableInfo
  = VIRequired Name
  | -- | Unlike fields (see 'InputFieldInfo'), nullable variables with no
    -- default value are indistinguishable from variables with a default value
    -- of null, so we don’t distinguish those cases here.
    VIOptional Name (Value Void)
  deriving (Show, Eq, Generic, Ord, TH.Lift)

instance Hashable VariableInfo

instance Cacheable VariableInfo

instance HasName Variable where
  getName = getName . vInfo

instance HasName VariableInfo where
  getName (VIRequired name) = name
  getName (VIOptional name _) = name

-- -----------------------------------------------------------------------------
-- support for introspection queries

-- | This type represents the directives information to be served over GraphQL introspection
data DirectiveInfo = DirectiveInfo
  { diName :: !Name,
    diDescription :: !(Maybe Description),
    diArguments :: ![Definition InputFieldInfo],
    diLocations :: ![DirectiveLocation]
  }

-- | This type contains all the information needed to efficiently serve GraphQL
-- introspection queries. It corresponds to the GraphQL @__Schema@ type defined
-- in <§ 4.5 Schema Introspection http://spec.graphql.org/June2018/#sec-Introspection>.
-- See also Note [Basics of introspection schema generation].
data Schema = Schema
  { sDescription :: Maybe Description,
    sTypes :: HashMap Name SomeDefinitionTypeInfo,
    sQueryType :: Type 'Output,
    sMutationType :: Maybe (Type 'Output),
    sSubscriptionType :: Maybe (Type 'Output),
    sDirectives :: [DirectiveInfo]
  }

data TypeDefinitionsWrapper where
  TypeDefinitionsWrapper :: HasTypeDefinitions a => a -> TypeDefinitionsWrapper

{-
Note [Collecting types from the GraphQL schema]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A `Parser` object consists of two things:

- a function that is used to process (part of) an incoming query, and
- a piece of GraphQL type information.

The main reason that the GraphQL type information is included is so that we can
generate responses for the introspection fields `__type` and `__schema`.  In
particular, this requires us to have a complete list of all types being used in
our schema.

When we build our schema, we therefore finish by making a full walk over the
entirety of the schema, collecting the GraphQL types encountered in a `HashMap`,
allowing us to look up GraphQL types by name.  At this point we might figure out
that a single name is used to represent two GraphQL types that are materially
distinct.  For instance, the name `author` might be used as both a GraphQL
object, representing a database table, and as a scalar, e.g. as a string name.
It also prevents us from having both

```
type author {
  id : int
  name : string
}
```

and

```
type author {
  id : int
  name : string
  email : string
}
```

in the schema, as the latter has an additional field and is thus distinct from
the former, even though it has the same name.

In fact, for HGE internally, such name clashes are not problematic.  We would
merely end up exposing illegal introspection results.  But in order to produce
introspection results, we have to explore the GraphQL schema anyway, to collect
all types.  We use this opportunity to help our users figure out whether there
are any name clashes, and if so what caused them.  So we do some work to track
where in the schema various GraphQL type names were encountered.  This type
collision information is stored in `ConflictingDefinitions`.

A typical way in which conflicting type definitions occur in practice is if one
version of HGE adds a different version of HGE as a remote schema, particularly
when support for database features was added in the meantime.  For instance, if
we'd add a new operator to boolean expressions, e.g. `XOR`, then this would end
up adding an additional field to every `<column>_bool_exp` object in our schema,
which clashes with the old `<column>_bool_exp` that's part of the remote schema.
This is not a bug of the "conflicting type definitions" logic but a limitation
of the design of HGE, which would be resolved by e.g. having namespaces for
different data sources.
-}

-- | Recursively collects all type definitions accessible from the given value,
-- attempting to detect any conflicting defintions that may have made it this
-- far (See 'ConflictingDefinitions' for details).
collectTypeDefinitions ::
  HasTypeDefinitions a =>
  a ->
  Either ConflictingDefinitions (HashMap Name SomeDefinitionTypeInfo)
collectTypeDefinitions x =
  fmap (fmap fst) $
    runExcept $
      flip execStateT Map.empty $
        flip runReaderT (TypeOriginStack []) $
          runTypeAccumulation $
            accumulateTypeDefinitions x

-- | A path through 'Definition', accumulated in 'accumulateTypeDefinitions'
-- only to power 'ConflictingDefinitions' in the error case.
newtype TypeOriginStack = TypeOriginStack [Name]

-- Add the current field name to the origin stack
typeOriginRecurse :: Name -> TypeOriginStack -> TypeOriginStack
typeOriginRecurse field (TypeOriginStack origins) = TypeOriginStack (field : origins)

-- This is kind of a hack to make sure that the query root name is part of the origin stack
typeRootRecurse :: Name -> TypeOriginStack -> TypeOriginStack
typeRootRecurse rootName (TypeOriginStack []) = (TypeOriginStack [rootName])
typeRootRecurse _ x = x

instance ToTxt TypeOriginStack where
  toTxt (TypeOriginStack fields) = T.intercalate "." $ toTxt <$> reverse fields

-- | NOTE: it's not clear exactly where we'd get conflicting definitions at the
-- point 'collectTypeDefinitions' is called, but conflicting names from
-- different data sources is apparently one place (TODO some tests that
-- excercise this).
--
-- ALSO NOTE: it's difficult to see in isolation how or if this check is
-- correct since 'Definition' is cyclic and has no accomodations for observable
-- sharing (formerly it had Uniques; see commit history and discussion in
-- #3685). The check relies on dodgy Eq instances for the types that make up
-- the Definition graph (see e.g. @instance Eq ObjectInfo@).
--
-- See Note [Collecting types from the GraphQL schema]
data ConflictingDefinitions
  = -- | Type collection has found at least two types with the same name.
    ConflictingDefinitions
      (SomeDefinitionTypeInfo, TypeOriginStack)
      (SomeDefinitionTypeInfo, NonEmpty TypeOriginStack)

-- | Although the majority of graphql-engine is written in terms of abstract
-- mtl-style effect monads, we figured out that this particular codepath is
-- quite hot, and that mtl has a measurable negative effect for accumulating
-- types from the schema, both in profiling and in benchmarking.  Using an
-- explicit transformers-style effect stack seems to overall memory usage by
-- about 3-7%.
newtype TypeAccumulation a = TypeAccumulation
  { runTypeAccumulation ::
      ReaderT
        TypeOriginStack
        ( StateT
            (HashMap Name (SomeDefinitionTypeInfo, NonEmpty TypeOriginStack))
            (ExceptT ConflictingDefinitions Identity)
        )
        a
  }
  deriving (Functor, Applicative, Monad)
  deriving (MonadReader TypeOriginStack)
  deriving (MonadState (HashMap Name (SomeDefinitionTypeInfo, NonEmpty TypeOriginStack)))
  deriving (MonadError ConflictingDefinitions)

class HasTypeDefinitions a where
  -- | Recursively accumulates all type definitions accessible from the given
  -- value. This is done statefully to avoid infinite loops arising from
  -- recursive type definitions; see Note [Tying the knot] in Hasura.GraphQL.Parser.Class.
  accumulateTypeDefinitions ::
    a -> TypeAccumulation ()

instance HasTypeDefinitions (Definition (TypeInfo k)) where
  accumulateTypeDefinitions new@Definition {..} = do
    -- This is the important case! We actually have a type definition, so we
    -- need to add it to the state.
    definitions <- get
    stack <- ask
    let someNew = SomeDefinitionTypeInfo new
    case Map.lookup dName definitions of
      Nothing -> do
        put $! Map.insert dName (someNew, pure stack) definitions
        -- This type definition might reference other type definitions, so we
        -- still need to recur.
        local (typeRootRecurse dName) $ accumulateTypeDefinitions dInfo
      Just (someOld, origins)
        -- It’s important we /don’t/ recur if we’ve already seen this definition
        -- before to avoid infinite loops; see Note [Tying the knot] in Hasura.GraphQL.Parser.Class.
        -- (NOTE: I tried making `origins` an STRef and doing a mutable update
        -- here but the performance was about the same)
        | someOld == someNew -> put $! Map.insert dName (someOld, stack `NE.cons` origins) definitions
        | otherwise -> throwError $ ConflictingDefinitions (someNew, stack) (someOld, origins)

instance HasTypeDefinitions a => HasTypeDefinitions [a] where
  accumulateTypeDefinitions = traverse_ accumulateTypeDefinitions

instance HasTypeDefinitions a => HasTypeDefinitions (Maybe a) where
  accumulateTypeDefinitions = traverse_ accumulateTypeDefinitions

instance HasTypeDefinitions TypeDefinitionsWrapper where
  accumulateTypeDefinitions (TypeDefinitionsWrapper x) = accumulateTypeDefinitions x

instance HasTypeDefinitions (Type k) where
  accumulateTypeDefinitions = \case
    TNamed _ t -> accumulateTypeDefinitions t
    TList _ t -> accumulateTypeDefinitions t

instance HasTypeDefinitions (TypeInfo k) where
  accumulateTypeDefinitions = \case
    TIScalar -> pure ()
    TIEnum _ -> pure ()
    TIInputObject (InputObjectInfo fields) -> accumulateTypeDefinitions fields
    TIObject (ObjectInfo fields interfaces) ->
      accumulateTypeDefinitions fields >> accumulateTypeDefinitions interfaces
    TIInterface (InterfaceInfo fields objects) ->
      accumulateTypeDefinitions fields
        >> accumulateTypeDefinitions objects
    TIUnion (UnionInfo objects) -> accumulateTypeDefinitions objects

instance HasTypeDefinitions (Definition InputObjectInfo) where
  accumulateTypeDefinitions = accumulateTypeDefinitions . fmap TIInputObject

instance HasTypeDefinitions (Definition InputFieldInfo) where
  accumulateTypeDefinitions Definition {..} =
    local (typeOriginRecurse dName) $ accumulateTypeDefinitions dInfo

instance HasTypeDefinitions InputFieldInfo where
  accumulateTypeDefinitions (InputFieldInfo t _) =
    accumulateTypeDefinitions t

instance HasTypeDefinitions (Definition FieldInfo) where
  accumulateTypeDefinitions Definition {..} =
    local (typeOriginRecurse dName) $ accumulateTypeDefinitions dInfo

instance HasTypeDefinitions FieldInfo where
  accumulateTypeDefinitions (FieldInfo args t) = do
    accumulateTypeDefinitions args
    accumulateTypeDefinitions t

instance HasTypeDefinitions (Definition ObjectInfo) where
  accumulateTypeDefinitions d@Definition {..} =
    local (typeOriginRecurse dName) $ accumulateTypeDefinitions (fmap TIObject d)

instance HasTypeDefinitions (Definition InterfaceInfo) where
  accumulateTypeDefinitions d@Definition {..} =
    local (typeOriginRecurse dName) $ accumulateTypeDefinitions (fmap TIInterface d)

instance HasTypeDefinitions (Definition UnionInfo) where
  accumulateTypeDefinitions d@Definition {..} =
    local (typeOriginRecurse dName) $ accumulateTypeDefinitions (fmap TIUnion d)

{- PERFORMANCE NOTE/TODO:

Since Definition's are cyclic I spent a little time trying to optimize the
== in accumulateTypeDefinitions into a nearly-noop using pointer
equality, but could not get it to trigger unless I called it on the unlifted
ByteArray# within dName, but at that point what was a pretty small theoretical
benefit disappeared for whatever reason (plus wasn't strictly safe at that
point). (Note, to have any luck calling on Definitions directly we would need
to fix the reallocation of Definitions via @fmap TI...@ in
accumulateTypeDefinitions as well)

The TODO-flavored thing here is to investigate whether we might not have as
much sharing here as we assume. We can use ghc-debug to inspect the object in
the heap.

We might also then rewrite accumulateTypeDefinitions to return non-cyclic type
definition segmants corresponding to the equality logic here (see "dodgy"
equality comments), and even consider trying to do some kind of global
interning of these across roles (though I think that would only be an
very incremental improvement...)

-- | See e.g. https://github.com/haskell/containers/blob/master/containers/src/Utils/Containers/Internal/PtrEquality.hs
--
-- If this returns True then the arguments are equal (for any sane definition of equality)
-- if this returns False nothing can be determined. The caller must ensure
-- referential transparency is preserved...
unsafeHetPtrEq :: a -> b -> Bool
unsafeHetPtrEq !x !y = isTrue# (unsafeCoerce (reallyUnsafePtrEquality# :: x -> x -> Int#) x y)
{-# INLINE unsafeHetPtrEq #-}
infix 4 `unsafeHetPtrEq` -- just like (==)

-- | Equivalent to @(==)@ but potentially faster in cases where the arguments
-- might be pointer-identical.
fastEq :: (Eq a)=> a -> a -> Bool
fastEq !x !y =
  -- See e.g. https://github.com/haskell/containers/blob/master/containers/src/Utils/Containers/Internal/PtrEquality.hs
  isTrue# (reallyUnsafePtrEquality# x y) || x == y

infix 4 `fastEq` -- just like (==)
-}
