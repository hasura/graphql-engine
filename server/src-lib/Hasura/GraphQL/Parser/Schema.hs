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
  , getObjType

  , EnumValueInfo(..)
  , InputFieldInfo(..)
  , FieldInfo(..)

  -- * Definitions
  , Definition(..)
  , mkDefinition
  , addDefinitionUnique
  , HasDefinition(..)

  -- * Schemas
  , Schema(..)
  , ConflictingDefinitions(..)
  , collectTypeDefinitions

  -- * Miscellany
  , HasName(..)
  , Variable(..)
  , VariableInfo(..)
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict.Extended  as Map

import           Control.Lens.Extended
import           Control.Monad.Unique
import           Data.Functor.Classes
import           Language.GraphQL.Draft.Syntax (Description (..), Name (..), Value (..), Directive(..))

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

data NonNullableType k
  = TNamed (Definition (TypeInfo k))
  | TList (Type k)

instance Eq (NonNullableType k) where
  (==) = eqNonNullableType

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

data TypeInfo k where
  TIScalar :: TypeInfo 'Both
  TIEnum :: NonEmpty (Definition EnumValueInfo) -> TypeInfo 'Both
  TIInputObject :: [Definition InputFieldInfo] -> TypeInfo 'Input
  TIObject :: [Definition FieldInfo] -> TypeInfo 'Output

instance Eq (TypeInfo k) where
  (==) = eqTypeInfo

-- | Like '==', but can compare 'TypeInfo's of different kinds.
eqTypeInfo :: TypeInfo k1 -> TypeInfo k2 -> Bool
eqTypeInfo TIScalar                TIScalar                = True
eqTypeInfo (TIEnum values1)        (TIEnum values2)        = values1 == values2
eqTypeInfo (TIInputObject fields1) (TIInputObject fields2) = fields1 == fields2
eqTypeInfo (TIObject fields1)      (TIObject fields2)      = fields1 == fields2
eqTypeInfo _                       _                       = False

getObjType :: SomeTypeInfo -> Maybe [Definition FieldInfo]
getObjType (SomeTypeInfo (TIObject obj)) = Just obj
getObjType _ = Nothing

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
  } deriving (Functor)

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
  deriving (Eq)

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
  -- completely absent; see <http://spec.graphql.org/June2018/#CoerceArgumentValues()>.
  | forall k. ('Input <: k) => IFOptional (NonNullableType k) (Maybe (Value Void))

instance Eq InputFieldInfo where
  IFRequired t1    == IFRequired t2    = eqNonNullableType t1 t2
  IFOptional t1 v1 == IFOptional t2 v2 = eqNonNullableType t1 t2 && v1 == v2
  _                == _                = False

data FieldInfo = forall k. ('Output <: k) => FieldInfo
  { fArguments :: [Definition InputFieldInfo]
  , fType      :: Type k
  }

instance Eq FieldInfo where
  FieldInfo args1 t1 == FieldInfo args2 t2 = args1 == args2 && eqType t1 t2

data Variable = Variable
  { vInfo  :: VariableInfo
  , vValue :: Value Void
  -- ^ Note: if the variable was null or was not provided and the field has a
  -- non-null default value, this field contains the default value, not 'VNull'.
  } deriving (Eq)

data VariableInfo
  = VIRequired Name
  -- | Unlike fields (see 'IFOptional'), nullable variables with no default
  -- value are indistinguishable from variables with a default value of null, so
  -- we don’t distinguish those cases here.
  | VIOptional Name (Value Void)
  deriving (Eq)

instance HasName Variable where
  getName = getName . vInfo
instance HasName VariableInfo where
  getName (VIRequired name)   = name
  getName (VIOptional name _) = name

-- -----------------------------------------------------------------------------
-- support for introspection queries

-- | This type contains all the information needed to efficiently serve GraphQL
-- introspection queries. It corresponds to the GraphQL @__Schema@ type defined
-- in <§ 4.5 Schema Introspection http://spec.graphql.org/June2018/#sec-Introspection>.
data Schema = Schema
  { sDescription      :: Maybe Description
  , sTypes            :: HashMap Name (Definition SomeTypeInfo)
  , sQueryType        :: Type 'Output
  , sMutationType     :: Maybe (Type 'Output)
  , sSubscriptionType :: Maybe (Type 'Output)
  , sDirectives       :: [Directive Void]
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
    TIScalar             -> pure ()
    TIEnum _             -> pure ()
    TIInputObject fields -> accumulateTypeDefinitions fields
    TIObject fields      -> accumulateTypeDefinitions fields

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
