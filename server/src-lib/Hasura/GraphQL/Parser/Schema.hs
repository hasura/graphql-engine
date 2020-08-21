{-# LANGUAGE StrictData #-}

-- | Types for representing a GraphQL schema.
module Hasura.GraphQL.Parser.Schema (
  -- * Kinds
    Kind(..)
  , (:<:)(..)
  , type (<:)(..)

  -- * Types
  , Type(..)
  , NonNullableType(..)
  , TypeInfo(..)
  , SomeTypeInfo(..)
  , eqType
  , eqNonNullableType
  , eqTypeInfo
  , discardNullability
  , nullableType
  , nonNullableType
  , toGraphQLType
  , getObjectInfo
  , getInterfaceInfo

  , EnumValueInfo(..)
  , InputFieldInfo(..)
  , FieldInfo(..)
  , InputObjectInfo(..)
  , ObjectInfo(..)
  , InterfaceInfo(..)
  , UnionInfo(..)

  -- * Definitions
  , Definition(..)
  , mkDefinition
  , addDefinitionUnique
  , HasDefinition(..)

  -- * Schemas
  , Schema(..)
  , ConflictingDefinitions(..)
  , HasTypeDefinitions(..)
  , collectTypeDefinitions

  -- * Miscellany
  , HasName(..)
  , InputValue(..)
  , Variable(..)
  , VariableInfo(..)
  , DirectiveInfo(..)
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                    as J
import qualified Data.HashMap.Strict.Extended  as Map
import qualified Data.HashSet                  as Set
import           Data.Hashable                 ( Hashable (..) )

import           Control.Lens.Extended
import           Control.Monad.Unique
import           Data.Functor.Classes
import           Language.GraphQL.Draft.Syntax ( Description (..), Name (..)
                                               , Value (..), Nullability(..)
                                               , GType (..), DirectiveLocation(..)
                                               )

class HasName a where
  getName :: a -> Name
instance HasName Name where
  getName = id

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
  = Both -- ^ see Note [The 'Both kind]
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

data Type k
  = NonNullable (NonNullableType k)
  | Nullable (NonNullableType k)

instance Eq (Type k) where
  (==) = eqType

-- | Like '==', but can compare 'Type's of different kinds.
eqType :: Type k1 -> Type k2 -> Bool
eqType (NonNullable a) (NonNullable b) = eqNonNullableType a b
eqType (Nullable    a) (Nullable    b) = eqNonNullableType a b
eqType _               _               = False

instance HasName (Type k) where
  getName = getName . discardNullability

instance HasDefinition (Type k) (TypeInfo k) where
  definitionLens f (NonNullable t) = NonNullable <$> definitionLens f t
  definitionLens f (Nullable t)    = Nullable <$> definitionLens f t

discardNullability :: Type k -> NonNullableType k
discardNullability (NonNullable t) = t
discardNullability (Nullable t)    = t

nullableType :: Type k -> Type k
nullableType = Nullable . discardNullability

nonNullableType :: Type k -> Type k
nonNullableType = NonNullable . discardNullability

data NonNullableType k
  = TNamed (Definition (TypeInfo k))
  | TList (Type k)

instance Eq (NonNullableType k) where
  (==) = eqNonNullableType

toGraphQLType :: Type k -> GType
toGraphQLType = \case
  NonNullable t -> translateWith False t
  Nullable    t -> translateWith True  t
  where
    translateWith nullability = \case
      TNamed typeInfo -> TypeNamed (Nullability nullability) $ getName typeInfo
      TList  typeInfo -> TypeList  (Nullability nullability) $ toGraphQLType typeInfo


-- | Like '==', but can compare 'NonNullableType's of different kinds.
eqNonNullableType :: NonNullableType k1 -> NonNullableType k2 -> Bool
eqNonNullableType (TNamed a) (TNamed b) = liftEq eqTypeInfo a b
eqNonNullableType (TList  a) (TList  b) = eqType a b
eqNonNullableType _          _          = False

instance HasName (NonNullableType k) where
  getName (TNamed definition) = getName definition
  getName (TList t)           = getName t

instance HasDefinition (NonNullableType k) (TypeInfo k) where
  definitionLens f (TNamed definition) = TNamed <$> f definition
  definitionLens f (TList t)           = TList <$> definitionLens f t

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

data InputObjectInfo = InputObjectInfo ~[Definition InputFieldInfo]
-- Note that we can't check for equality of the fields since there may be
-- circularity. So we rather check for equality of names.
instance Eq InputObjectInfo where
  InputObjectInfo fields1 == InputObjectInfo fields2
    =  Set.fromList (fmap dName fields1)     == Set.fromList (fmap dName fields2)

data ObjectInfo = ObjectInfo
  { oiFields :: ~[Definition FieldInfo]
    -- ^ The fields that this object has.  This consists of the fields of the
    -- interfaces that it implements, as well as any additional fields.
  , oiImplements :: ~[Definition InterfaceInfo]
    -- ^ The interfaces that this object implements (inheriting all their
    -- fields). See Note [The interfaces story] for more details.
  }
-- Note that we can't check for equality of the fields and the interfaces since
-- there may be circularity. So we rather check for equality of names.
instance Eq ObjectInfo where
  ObjectInfo fields1 interfaces1 == ObjectInfo fields2 interfaces2
    =  Set.fromList (fmap dName fields1    ) == Set.fromList (fmap dName fields2    )
    && Set.fromList (fmap dName interfaces1) == Set.fromList (fmap dName interfaces2)

-- | Type information for a GraphQL interface; see Note [The interfaces story]
-- for more details.
--
-- Note: in the current working draft of the GraphQL specification (> June
-- 2018), interfaces may implement other interfaces, but we currently don't
-- support this.
data InterfaceInfo = InterfaceInfo
  { iiFields :: ~[Definition FieldInfo]
    -- ^ Fields declared by this interface. Every object implementing this
    -- interface must include those fields.
  , iiPossibleTypes :: ~[Definition ObjectInfo]
    -- ^ Objects that implement this interface. See Note [The interfaces story]
    -- for why we include that information here.
  }
-- Note that we can't check for equality of the fields and the interfaces since
-- there may be circularity. So we rather check for equality of names.
instance Eq InterfaceInfo where
  InterfaceInfo fields1 objects1 == InterfaceInfo fields2 objects2
    =  Set.fromList (fmap dName fields1    ) == Set.fromList (fmap dName fields2    )
    && Set.fromList (fmap dName objects1   ) == Set.fromList (fmap dName objects2   )

data UnionInfo = UnionInfo
  { uiPossibleTypes :: ~[Definition ObjectInfo]
    -- ^ The member object types of this union.
  }

data TypeInfo k where
  TIScalar :: TypeInfo 'Both
  TIEnum :: NonEmpty (Definition EnumValueInfo) -> TypeInfo 'Both
  TIInputObject :: InputObjectInfo -> TypeInfo 'Input
  TIObject :: ObjectInfo -> TypeInfo 'Output
  TIInterface :: InterfaceInfo -> TypeInfo 'Output
  TIUnion :: UnionInfo -> TypeInfo 'Output

instance Eq (TypeInfo k) where
  (==) = eqTypeInfo

-- | Like '==', but can compare 'TypeInfo's of different kinds.
eqTypeInfo :: TypeInfo k1 -> TypeInfo k2 -> Bool
eqTypeInfo  TIScalar                       TIScalar            = True
eqTypeInfo (TIEnum values1)               (TIEnum values2)
  = Set.fromList (toList values1) == Set.fromList (toList values2)
-- NB the case for input objects currently has quadratic complexity, which is
-- probably avoidable. HashSets should be able to get this down to
-- O(n*log(n)). But this requires writing some Hashable instances by hand
-- because we use some existential types and GADTs.
eqTypeInfo (TIInputObject ioi1)           (TIInputObject ioi2) = ioi1 == ioi2
eqTypeInfo (TIObject oi1)                 (TIObject oi2)       = oi1 == oi2
eqTypeInfo (TIInterface ii1)              (TIInterface ii2)    = ii1 == ii2
eqTypeInfo (TIUnion (UnionInfo objects1)) (TIUnion (UnionInfo objects2))
  = Set.fromList (fmap dName objects1) == Set.fromList (fmap dName objects2)
eqTypeInfo _                              _                    = False

getObjectInfo :: Type k -> Maybe (Definition ObjectInfo)
getObjectInfo = traverse getTI . (^.definitionLens)
  where
    getTI :: TypeInfo k -> Maybe ObjectInfo
    getTI (TIObject oi) = Just oi
    getTI _ = Nothing

getInterfaceInfo :: Type 'Output -> Maybe (Definition InterfaceInfo)
getInterfaceInfo = traverse getTI . (^.definitionLens)
  where
    getTI :: TypeInfo 'Output -> Maybe InterfaceInfo
    getTI (TIInterface ii) = Just ii
    getTI _ = Nothing

data SomeTypeInfo = forall k. SomeTypeInfo (TypeInfo k)

instance Eq SomeTypeInfo where
  SomeTypeInfo a == SomeTypeInfo b = eqTypeInfo a b

data Definition a = Definition
  { dName        :: Name
  , dUnique      :: Maybe Unique
  -- ^ A unique identifier used to break cycles in mutually-recursive type
  -- definitions. If two 'Definition's have the same 'Unique', they can be
  -- assumed to be identical. Note that the inverse is /not/ true: two
  -- definitions with different 'Unique's might still be otherwise identical.
  --
  -- Also see Note [Tying the knot] in Hasura.GraphQL.Parser.Class.
  , dDescription :: Maybe Description
  , dInfo        :: ~a
  -- ^ Lazy to allow mutually-recursive type definitions.
  } deriving (Functor, Foldable, Traversable, Generic)
instance Hashable a => Hashable (Definition a) where
  hashWithSalt salt Definition{..} =
    salt `hashWithSalt` dName `hashWithSalt` dInfo

mkDefinition :: Name -> Maybe Description -> a -> Definition a
mkDefinition name description info = Definition name Nothing description info

instance Eq a => Eq (Definition a) where
  (==) = eq1

instance Eq1 Definition where
  liftEq eq (Definition name1 maybeUnique1 _ info1)
            (Definition name2 maybeUnique2 _ info2)
    | Just unique1 <- maybeUnique1
    , Just unique2 <- maybeUnique2
    , unique1 == unique2
    = True
    | otherwise
    = name1 == name2 && eq info1 info2

instance HasName (Definition a) where
  getName = dName

class HasDefinition s a | s -> a where
  definitionLens :: Lens' s (Definition a)
instance HasDefinition (Definition a) a where
  definitionLens = id

-- | Adds a 'Unique' to a 'Definition' that does not yet have one. If the
-- definition already has a 'Unique', the existing 'Unique' is kept.
addDefinitionUnique :: HasDefinition s a => Unique -> s -> s
addDefinitionUnique unique = over definitionLens \definition ->
  definition { dUnique = dUnique definition <|> Just unique }

-- | Enum values have no extra information except for the information common to
-- all definitions, so this is just a placeholder for use as @'Definition'
-- 'EnumValueInfo'@.
data EnumValueInfo = EnumValueInfo
  deriving (Eq, Generic)
instance Hashable EnumValueInfo

data InputFieldInfo
  -- | A required field with a non-nullable type.
  = forall k. ('Input <: k) => IFRequired (NonNullableType k)
  -- | An optional input field with a nullable type and possibly a default
  -- value. If a default value is provided, it should be a valid value for the
  -- type.
  --
  -- Note that a default value of 'VNull' is subtly different from having no
  -- default value at all. If no default value is provided, the GraphQL
  -- specification allows distinguishing provided @null@ values from values left
  -- completely absent; see Note [Optional fields and nullability] in
  -- Hasura.GraphQL.Parser.Internal.Parser.
  | forall k. ('Input <: k) => IFOptional (Type k) (Maybe (Value Void))

instance Eq InputFieldInfo where
  IFRequired t1    == IFRequired t2    = eqNonNullableType t1 t2
  IFOptional t1 v1 == IFOptional t2 v2 = eqType t1 t2 && v1 == v2
  _                == _                = False

data FieldInfo = forall k. ('Output <: k) => FieldInfo
  { fArguments :: [Definition InputFieldInfo]
  , fType      :: Type k
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
  deriving (Show, Eq, Functor)

data Variable = Variable
  { vInfo  :: VariableInfo
  , vType  :: GType
  , vValue :: InputValue Void
  -- ^ Note: if the variable was null or was not provided and the field has a
  -- non-null default value, this field contains the default value, not 'VNull'.
  } deriving (Show,Eq)


data VariableInfo
  = VIRequired Name
  -- | Unlike fields (see 'IFOptional'), nullable variables with no default
  -- value are indistinguishable from variables with a default value of null, so
  -- we don’t distinguish those cases here.
  | VIOptional Name (Value Void)
  deriving (Show,Eq)

instance HasName Variable where
  getName = getName . vInfo
instance HasName VariableInfo where
  getName (VIRequired name)   = name
  getName (VIOptional name _) = name

-- -----------------------------------------------------------------------------
-- support for introspection queries

-- | This type represents the directives information to be served over GraphQL introspection
data DirectiveInfo = DirectiveInfo
  { diName        :: !Name
  , diDescription :: !(Maybe Description)
  , diArguments   :: ![Definition InputFieldInfo]
  , diLocations   :: ![DirectiveLocation]
  }

-- | This type contains all the information needed to efficiently serve GraphQL
-- introspection queries. It corresponds to the GraphQL @__Schema@ type defined
-- in <§ 4.5 Schema Introspection http://spec.graphql.org/June2018/#sec-Introspection>.
data Schema = Schema
  { sDescription      :: Maybe Description
  , sTypes            :: HashMap Name (Definition SomeTypeInfo)
  , sQueryType        :: Type 'Output
  , sMutationType     :: Maybe (Type 'Output)
  , sSubscriptionType :: Maybe (Type 'Output)
  , sDirectives       :: [DirectiveInfo]
  }

-- | Recursively collects all type definitions accessible from the given value.
collectTypeDefinitions
  :: (HasTypeDefinitions a, MonadError ConflictingDefinitions m)
  => a -> m (HashMap Name (Definition SomeTypeInfo))
collectTypeDefinitions = flip execStateT Map.empty . accumulateTypeDefinitions

data ConflictingDefinitions
  = ConflictingDefinitions (Definition SomeTypeInfo) (Definition SomeTypeInfo)

class HasTypeDefinitions a where
  -- | Recursively accumulates all type definitions accessible from the given
  -- value. This is done statefully to avoid infinite loops arising from
  -- recursive type definitions; see Note [Tying the knot] in Hasura.GraphQL.Parser.Class.
  accumulateTypeDefinitions
    :: ( MonadError ConflictingDefinitions m
       , MonadState (HashMap Name (Definition SomeTypeInfo)) m )
    => a -> m ()

instance HasTypeDefinitions (Definition (TypeInfo k)) where
  accumulateTypeDefinitions definition = do
    -- This is the important case! We actually have a type definition, so we
    -- need to add it to the state.
    definitions <- get
    let new = SomeTypeInfo <$> definition
    case Map.lookup (dName new) definitions of
      Nothing -> do
        put $! Map.insert (dName new) new definitions
        -- This type definition might reference other type definitions, so we
        -- still need to recur.
        accumulateTypeDefinitions (dInfo definition)
      Just old
        -- It’s important we /don’t/ recur if we’ve already seen this definition
        -- before to avoid infinite loops; see Note [Tying the knot] in Hasura.GraphQL.Parser.Class.
        | old == new -> pure ()
        | otherwise  -> throwError $ ConflictingDefinitions old new

instance HasTypeDefinitions a => HasTypeDefinitions [a] where
  accumulateTypeDefinitions = traverse_ accumulateTypeDefinitions

instance HasTypeDefinitions (Type k) where
  accumulateTypeDefinitions = \case
    NonNullable t -> accumulateTypeDefinitions t
    Nullable    t -> accumulateTypeDefinitions t

instance HasTypeDefinitions (NonNullableType k) where
  accumulateTypeDefinitions = \case
    TNamed d -> accumulateTypeDefinitions d
    TList  t -> accumulateTypeDefinitions t

instance HasTypeDefinitions (TypeInfo k) where
  accumulateTypeDefinitions = \case
    TIScalar                                              -> pure ()
    TIEnum _                                              -> pure ()
    TIInputObject (InputObjectInfo fields)                -> accumulateTypeDefinitions fields
    TIObject (ObjectInfo fields interfaces)               ->
      accumulateTypeDefinitions fields >> accumulateTypeDefinitions interfaces
    TIInterface (InterfaceInfo fields objects) ->
         accumulateTypeDefinitions fields
      >> accumulateTypeDefinitions objects
    TIUnion (UnionInfo objects)                           -> accumulateTypeDefinitions objects

instance HasTypeDefinitions (Definition InputObjectInfo) where
  accumulateTypeDefinitions = accumulateTypeDefinitions . fmap TIInputObject

instance HasTypeDefinitions (Definition InputFieldInfo) where
  accumulateTypeDefinitions = accumulateTypeDefinitions . dInfo

instance HasTypeDefinitions InputFieldInfo where
  accumulateTypeDefinitions = \case
    IFRequired t   -> accumulateTypeDefinitions t
    IFOptional t _ -> accumulateTypeDefinitions t

instance HasTypeDefinitions (Definition FieldInfo) where
  accumulateTypeDefinitions = accumulateTypeDefinitions . dInfo

instance HasTypeDefinitions FieldInfo where
  accumulateTypeDefinitions (FieldInfo args t) = do
    accumulateTypeDefinitions args
    accumulateTypeDefinitions t

instance HasTypeDefinitions (Definition ObjectInfo) where
  accumulateTypeDefinitions = accumulateTypeDefinitions . fmap TIObject

instance HasTypeDefinitions (Definition InterfaceInfo) where
  accumulateTypeDefinitions = accumulateTypeDefinitions . fmap TIInterface

instance HasTypeDefinitions (Definition UnionInfo) where
  accumulateTypeDefinitions = accumulateTypeDefinitions . fmap TIUnion
