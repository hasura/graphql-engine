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

  , EnumValueInfo(..)
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
import           Data.Void
import           Language.GraphQL.Draft.Syntax (Description (..), Name (..), Value (..))

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
data Kind = Both | Input | Output

{- Note [The delicate balance of GraphQL kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The way we track kinds is rather delicate, and it succeeds many failed attempts
at encoding the same ideas. We rely on a happy coincidence to keep the types as
simple as possible: whether or not a field has a sub-selection set is knowable
from its kind alone!

  * Normal (non-input) object fields cannot have types of kind Input, so we can
    ignore that case.

  * The only types of kind Both are scalars and enums, neither of which accept a
    sub-selection set.

  * The remaining types, which we give kind Output, are objects, interfaces, and
    unions. We don’t currently support the latter two, so we only have to deal
    with objects, which always take sub-selection sets.

This allows us to conveniently re-use `Parser`s for types of kind Both to
represent output types for selection set fields (while still disallowing types
of kind Input). This trick avoids the need to track that information separately,
significantly simplifying the types! But if that happy coincidence ever ceases
to hold, we’d have to rethink things. -}

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
  TIInputObject :: [Definition (FieldInfo 'Input)] -> TypeInfo 'Input
  TIObject :: [Definition (FieldInfo 'Output)] -> TypeInfo 'Output

instance Eq (TypeInfo k) where
  (==) = eqTypeInfo

-- | Like '==', but can compare 'TypeInfo's of different kinds.
eqTypeInfo :: TypeInfo k1 -> TypeInfo k2 -> Bool
eqTypeInfo TIScalar                TIScalar                = True
eqTypeInfo (TIEnum values1)        (TIEnum values2)        = values1 == values2
eqTypeInfo (TIInputObject fields1) (TIInputObject fields2) = fields1 == fields2
eqTypeInfo (TIObject fields1)      (TIObject fields2)      = fields1 == fields2
eqTypeInfo _                       _                       = False

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

data family FieldInfo (k :: Kind)

data instance FieldInfo 'Input
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

instance Eq (FieldInfo 'Input) where
  IFRequired t1    == IFRequired t2    = eqNonNullableType t1 t2
  IFOptional t1 v1 == IFOptional t2 v2 = eqNonNullableType t1 t2 && v1 == v2
  _                == _                = False

data instance FieldInfo 'Output = forall k. ('Output <: k) => FieldInfo
  { fArguments :: [Definition (FieldInfo 'Input)]
  , fType :: Type k
  }

instance Eq (FieldInfo 'Output) where
  FieldInfo args1 t1 == FieldInfo args2 t2 = args1 == args2 && eqType t1 t2

data Variable = Variable
  { vInfo  :: VariableInfo
  , vValue :: Value Void
  -- ^ Note: if the variable was null or was not provided and the field has a
  -- non-null default value, this field contains the default value, not 'VNull'.
  }

data VariableInfo
  = VIRequired Name
  -- | Unlike fields (see 'IFOptional'), nullable variables with no default
  -- value are indistinguishable from variables with a default value of null, so
  -- we don’t distinguish those cases here.
  | VIOptional Name (Value Void)

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
  { sTypes            :: HashMap Name (Definition SomeTypeInfo)
  , sQueryType        :: Definition (TypeInfo 'Output)
  , sMutationType     :: Definition (TypeInfo 'Output)
  , sSubscriptionType :: Definition (TypeInfo 'Output)
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

instance HasTypeDefinitions (FieldInfo k)
      => HasTypeDefinitions (Definition (FieldInfo k)) where
  accumulateTypeDefinitions = accumulateTypeDefinitions . dInfo

instance HasTypeDefinitions (FieldInfo 'Input) where
  accumulateTypeDefinitions = \case
    IFRequired t   -> accumulateTypeDefinitions t
    IFOptional t _ -> accumulateTypeDefinitions t

instance HasTypeDefinitions (FieldInfo 'Output) where
  accumulateTypeDefinitions (FieldInfo args t) = do
    accumulateTypeDefinitions args
    accumulateTypeDefinitions t
