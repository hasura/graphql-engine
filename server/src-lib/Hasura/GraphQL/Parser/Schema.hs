{-# LANGUAGE StrictData #-}

-- | Types for representing a GraphQL schema.
module Hasura.GraphQL.Parser.Schema where

import           Hasura.Prelude

import           Control.Lens.Extended
import           Control.Monad.Unique
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
  }

mkDefinition :: Name -> Maybe Description -> a -> Definition a
mkDefinition name description info = Definition name Nothing description info

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

data family FieldInfo (k :: Kind)

data instance FieldInfo 'Input
  -- | A required field with a non-nullable type.
  = forall k. ('Input <: k) => IFRequired (NonNullableType k)
  -- | An optional input field with a nullable type and possibly a default
  -- value. If a default value is provided, it should be a valid value for the
  -- type.
  --
  -- Note that a default value of 'TNull' is subtly different from having no
  -- default value at all. If no default value is provided, the GraphQL
  -- specification allows distinguishing provided @null@ values from values left
  -- completely absent; see <http://spec.graphql.org/June2018/#CoerceArgumentValues()>.
  | forall k. ('Input <: k) => IFOptional (NonNullableType k) (Maybe (Value Void))

data instance FieldInfo 'Output = forall k. ('Output <: k) => FieldInfo
  { fArguments :: [Definition (FieldInfo 'Input)]
  , fType :: Type k
  }

data Variable = Variable
  { vDefinition :: Definition (FieldInfo 'Input)
  , vValue      :: Value Void
  -- ^ Note: if the variable was null or was not provided and the field has a
  -- non-null default value, this field contains the default value, not 'VNull'.
  }

instance HasName Variable where
  getName = getName . vDefinition
