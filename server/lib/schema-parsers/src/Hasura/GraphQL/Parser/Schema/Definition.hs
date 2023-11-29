{-# LANGUAGE PatternSynonyms #-}

-- | Types for representing a GraphQL schema.
module Hasura.GraphQL.Parser.Schema.Definition
  ( -- * Kinds
    Kind (..),
    (:<:) (..),
    type (<:) (..),

    -- * Types
    Nullability (..),
    Type (..),
    onTypeDef,
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

    -- * Miscellany
    DirectiveInfo (..),
  )
where

import Data.Function (on)
import Data.Functor.Classes
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable (..))
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Void (Void)
import GHC.Generics (Generic)
import Hasura.GraphQL.Parser.Names
import Language.GraphQL.Draft.Syntax
  ( Description (..),
    DirectiveLocation (..),
    GType (..),
    Name (..),
    Value (..),
  )
import Language.GraphQL.Draft.Syntax qualified as G

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
      TIInputObject :: ... -> TypeInfo origin 'Input
      TIObject      :: ... -> TypeInfo origin 'Output

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

    mkFooParser :: (MonadMemoize m, MonadParse n) => Blah -> m (Parser k n Foo)

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

instance (k1 ~ k2) => k1 <: k2 where
  subKind = KRefl

instance {-# OVERLAPPING #-} k <: 'Both where
  subKind = KBoth

data Nullability = Nullable | NonNullable
  deriving (Eq)

isNullable :: Nullability -> Bool
isNullable Nullable = True
isNullable NonNullable = False

data Type origin k
  = TNamed Nullability (Definition origin (TypeInfo origin k))
  | TList Nullability (Type origin k)

instance Eq (Type origin k) where
  (==) = eqType

-- | Adjust the 'Definition' underlying a 'Type'
onTypeDef :: (forall a. Definition origin a -> Definition origin a) -> Type origin k -> Type origin k
onTypeDef f (TNamed nul def) = TNamed nul (f def)
onTypeDef f (TList nul typ) = TList nul (onTypeDef f typ)

-- | Like '==', but can compare 'Type's of different kinds.
eqType :: Type origin k1 -> Type origin k2 -> Bool
eqType (TNamed n a) (TNamed n' b) = n == n' && liftEq eqTypeInfo a b
eqType (TList n a) (TList n' b) = n == n' && eqType a b
eqType _ _ = False

instance HasName (Type origin k) where
  getName (TNamed _ def) = getName def
  getName (TList _ t) = getName t

typeNullability :: Type origin k -> Nullability
typeNullability (TNamed n _) = n
typeNullability (TList n _) = n

nullableType :: Type origin k -> Type origin k
nullableType (TNamed _ def) = TNamed Nullable def
nullableType (TList _ t) = TList Nullable t

nonNullableType :: Type origin k -> Type origin k
nonNullableType (TNamed _ def) = TNamed NonNullable def
nonNullableType (TList _ t) = TList NonNullable t

toGraphQLType :: Type origin k -> GType
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
data InputObjectInfo origin = InputObjectInfo__ ~[Definition origin (InputFieldInfo origin)]

-- Public interface enforcing invariants
pattern InputObjectInfo :: [Definition origin (InputFieldInfo origin)] -> InputObjectInfo origin
pattern InputObjectInfo xs <-
  InputObjectInfo__ xs
  where
    InputObjectInfo xs = InputObjectInfo__ (List.sortOn dName xs)

{-# COMPLETE InputObjectInfo #-}

-- Note that we can't check for equality of the fields since there may be
-- circularity. So we rather check for equality of names.
instance Eq (InputObjectInfo origin) where
  InputObjectInfo fields1 == InputObjectInfo fields2 =
    eqByName fields1 fields2

-- | Invariant: the lists are sorted by 'dName', maintained via pattern synonyms
data ObjectInfo origin = ObjectInfo__
  { -- | The fields that this object has.  This consists of the fields of the
    -- interfaces that it implements, as well as any additional fields.
    _oiFields :: ~[Definition origin (FieldInfo origin)],
    -- | The interfaces that this object implements (inheriting all their
    -- fields). See Note [The interfaces story] for more details.
    _oiImplements :: ~[Definition origin (InterfaceInfo origin)]
  }

-- Public interface enforcing invariants
pattern ObjectInfo :: [Definition origin (FieldInfo origin)] -> [Definition origin (InterfaceInfo origin)] -> ObjectInfo origin
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
instance Eq (ObjectInfo origin) where
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
data InterfaceInfo origin = InterfaceInfo__
  { -- | Fields declared by this interface. Every object implementing this
    -- interface must include those fields.
    _iiFields :: ~[Definition origin (FieldInfo origin)],
    -- | Objects that implement this interface. See Note [The interfaces story]
    -- for why we include that information here.
    _iiPossibleTypes :: ~[Definition origin (ObjectInfo origin)]
  }

-- Public interface enforcing invariants
pattern InterfaceInfo :: [Definition origin (FieldInfo origin)] -> [Definition origin (ObjectInfo origin)] -> InterfaceInfo origin
pattern InterfaceInfo {iiFields, iiPossibleTypes} <-
  InterfaceInfo__ iiFields iiPossibleTypes
  where
    InterfaceInfo xs ys = InterfaceInfo__ (List.sortOn dName xs) (List.sortOn dName ys)

{-# COMPLETE InterfaceInfo #-}

-- Note that we can't check for equality of the fields and the interfaces since
-- there may be circularity. So we rather check for equality of names.
instance Eq (InterfaceInfo origin) where
  InterfaceInfo fields1 objects1 == InterfaceInfo fields2 objects2 =
    eqByName fields1 fields2 && eqByName objects1 objects2

-- | Invariant: the list is sorted by 'dName'
data UnionInfo origin = UnionInfo__
  { -- | The member object types of this union.
    _uiPossibleTypes :: ~[Definition origin (ObjectInfo origin)]
  }

-- Public interface enforcing invariants
pattern UnionInfo :: [Definition origin (ObjectInfo origin)] -> UnionInfo origin
pattern UnionInfo {uiPossibleTypes} <-
  UnionInfo__ uiPossibleTypes
  where
    UnionInfo xs = UnionInfo__ (List.sortOn dName xs)

{-# COMPLETE UnionInfo #-}

data TypeInfo origin k where
  TIScalar :: TypeInfo origin 'Both
  -- | Invariant: the NonEmpty is sorted by 'dName'
  TIEnum__ :: NonEmpty (Definition origin EnumValueInfo) -> TypeInfo origin 'Both
  TIInputObject :: InputObjectInfo origin -> TypeInfo origin 'Input
  TIObject :: ObjectInfo origin -> TypeInfo origin 'Output
  TIInterface :: InterfaceInfo origin -> TypeInfo origin 'Output
  TIUnion :: UnionInfo origin -> TypeInfo origin 'Output

-- Public interface enforcing invariants
pattern TIEnum :: forall origin (k :: Kind). () => (k ~ 'Both) => NonEmpty (Definition origin EnumValueInfo) -> TypeInfo origin k
pattern TIEnum xs <-
  TIEnum__ xs
  where
    TIEnum xs = TIEnum__ (NE.sortWith dName xs)

{-# COMPLETE TIScalar, TIEnum, TIInputObject, TIObject, TIInterface, TIUnion #-}

instance Eq (TypeInfo origin k) where
  (==) = eqTypeInfo

-- | Like '==', but can compare 'TypeInfo's of different kinds.
eqTypeInfo :: TypeInfo origin k1 -> TypeInfo origin k2 -> Bool
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

getTypeInfo :: Type origin k -> Definition origin (TypeInfo origin k)
getTypeInfo (TNamed _ d) = d
getTypeInfo (TList _ t) = getTypeInfo t

getObjectInfo :: Type origin k -> Maybe (Definition origin (ObjectInfo origin))
getObjectInfo t = case getTypeInfo t of
  d@Definition {dInfo = TIObject oi} -> Just d {dInfo = oi}
  _ -> Nothing

getInterfaceInfo :: Type origin k -> Maybe (Definition origin (InterfaceInfo origin))
getInterfaceInfo t = case getTypeInfo t of
  d@Definition {dInfo = TIInterface ii} -> Just d {dInfo = ii}
  _ -> Nothing

data SomeDefinitionTypeInfo origin = forall k. SomeDefinitionTypeInfo (Definition origin (TypeInfo origin k))

instance HasName (SomeDefinitionTypeInfo origin) where
  getName (SomeDefinitionTypeInfo (Definition n _ _ _ _)) = n

instance Eq (SomeDefinitionTypeInfo origin) where
  -- Same as instance Eq Definition
  SomeDefinitionTypeInfo (Definition name1 _ _ _ ti1)
    == SomeDefinitionTypeInfo (Definition name2 _ _ _ ti2) =
      name1 == name2 && eqTypeInfo ti1 ti2

data Definition origin a = Definition
  { dName :: Name,
    dDescription :: Maybe Description,
    -- | What piece of metadata was this fragment of GraphQL type information
    -- from?  See also 'Hasura.GraphQL.Schema.Parser'.
    --
    -- 'Nothing' can represent a couple of scenarios:
    -- 1. This is a native part of the GraphQL spec, e.g. the '__Type'
    --    introspection type
    -- 2. This is a native part of HGE, e.g. our scalar types and Relay-related
    --    types
    -- 3. We don't have a clear origin, because
    --    a. Semantically there is no clear origin because it arose from the
    --       combination of several things
    --    b. We generated this 'Definition' in a context where origin
    --       information was no longer in scope
    --
    -- Maybe, at some point, it makes sense to represent the above options more
    -- accurately in the type of 'dOrigin'.
    dOrigin :: Maybe origin,
    -- | The directives for this object.
    dDirectives :: [G.Directive Void],
    -- | Lazy to allow mutually-recursive type definitions.
    dInfo :: ~a
  }
  deriving (Functor, Foldable, Traversable, Generic)

instance (Hashable a) => Hashable (Definition origin a) where
  hashWithSalt salt Definition {..} =
    salt `hashWithSalt` dName `hashWithSalt` dInfo

instance (Eq a) => Eq (Definition origin a) where
  (==) = eq1

instance Eq1 (Definition origin) where
  liftEq
    eq
    (Definition name1 _ _ _ info1)
    (Definition name2 _ _ _ info2) =
      name1 == name2 && eq info1 info2

instance HasName (Definition origin a) where
  getName = dName

-- | equivalent to, but faster than...
--
-- > map dName x == map dName y
eqByName :: [Definition origin a] -> [Definition origin a] -> Bool
eqByName = liftEq ((==) `on` dName)

-- | Enum values have no extra information except for the information common to
-- all definitions, so this is just a placeholder for use as @'Definition'
-- 'EnumValueInfo'@.
data EnumValueInfo = EnumValueInfo
  deriving (Eq, Generic)

instance Hashable EnumValueInfo

data InputFieldInfo origin
  = -- | An input field with a type and possibly a default value. If a default
    -- value is provided, it should be a valid value for the type.
    --
    -- Note that a default value of 'VNull' is subtly different from having no
    -- default value at all. If no default value is provided (i.e. 'Nothing'),
    -- the GraphQL specification allows distinguishing provided @null@ values
    -- from values left completely absent; see Note [The value of omitted
    -- fields] in Hasura.GraphQL.Parser.Internal.Parser.
    forall k. ('Input <: k) => InputFieldInfo (Type origin k) (Maybe (Value Void))

instance Eq (InputFieldInfo origin) where
  InputFieldInfo t1 v1 == InputFieldInfo t2 v2 = eqType t1 t2 && v1 == v2

data FieldInfo origin = forall k.
  ('Output <: k) =>
  FieldInfo
  { fArguments :: [Definition origin (InputFieldInfo origin)],
    fType :: Type origin k
  }

instance Eq (FieldInfo origin) where
  FieldInfo args1 t1 == FieldInfo args2 t2 = args1 == args2 && eqType t1 t2

-- -----------------------------------------------------------------------------
-- support for introspection queries

-- | This type represents the directives information to be served over GraphQL introspection
data DirectiveInfo origin = DirectiveInfo
  { diName :: !Name,
    diDescription :: !(Maybe Description),
    diArguments :: ![Definition origin (InputFieldInfo origin)],
    diLocations :: ![DirectiveLocation]
  }

-- | This type contains all the information needed to efficiently serve GraphQL
-- introspection queries. It corresponds to the GraphQL @__Schema@ type defined
-- in <§ 4.5 Schema Introspection http://spec.graphql.org/June2018/#sec-Introspection>.
-- See also Note [Basics of introspection schema generation].
data Schema origin = Schema
  { sDescription :: Maybe Description,
    sTypes :: HashMap Name (SomeDefinitionTypeInfo origin),
    sQueryType :: Type origin 'Output,
    sMutationType :: Maybe (Type origin 'Output),
    sSubscriptionType :: Maybe (Type origin 'Output),
    sDirectives :: [DirectiveInfo origin]
  }
