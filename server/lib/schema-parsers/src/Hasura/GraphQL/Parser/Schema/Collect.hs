module Hasura.GraphQL.Parser.Schema.Collect
  ( ConflictingDefinitions (..),
    HasTypeDefinitions (..),
    TypeDefinitionsWrapper (..),
    collectTypeDefinitions,
  )
where

import Control.Lens
import Control.Monad.Except (ExceptT, MonadError (..), runExcept)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.State.Strict (MonadState (..), StateT, execStateT)
import Data.Foldable (traverse_)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Hasura.Base.ErrorMessage (toErrorMessage)
import Hasura.Base.ToErrorValue
import Hasura.GraphQL.Parser.Names
import Hasura.GraphQL.Parser.Schema.Convert
import Hasura.GraphQL.Parser.Schema.Definition
import Language.GraphQL.Draft.Printer qualified as G
import Language.GraphQL.Draft.Syntax
  ( Name (..),
  )

data TypeDefinitionsWrapper origin where
  TypeDefinitionsWrapper :: (HasTypeDefinitions origin a) => a -> TypeDefinitionsWrapper origin

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
  (HasTypeDefinitions origin a) =>
  a ->
  Either (ConflictingDefinitions origin) (HashMap Name (SomeDefinitionTypeInfo origin))
collectTypeDefinitions x =
  fmap (fmap fst) $
    runExcept $
      flip execStateT HashMap.empty $
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

instance ToErrorValue TypeOriginStack where
  toErrorValue (TypeOriginStack fields) = toErrorMessage . T.intercalate "." . map unName . reverse $ fields

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
data ConflictingDefinitions origin
  = -- | Type collection has found at least two types with the same name.
    ConflictingDefinitions
      (SomeDefinitionTypeInfo origin, TypeOriginStack)
      (SomeDefinitionTypeInfo origin, NonEmpty TypeOriginStack)

instance ToErrorValue (ConflictingDefinitions origin) where
  toErrorValue (ConflictingDefinitions (type1, origin1) (type2, origins)) =
    "Found conflicting definitions for GraphQL type "
      <> toErrorValue (getName type1)
      <> ".  The definition at "
      <> toErrorValue origin1
      <> " differs from the definitions at "
      <> toErrorValue origins
      <> "."
      <> "\nFormer has definition:\n"
      <> toErrorMessage (G.typeDefinitionP (bimap (const ()) id (convertType type1)))
      <> "\nLatter has definition:\n"
      <> toErrorMessage (G.typeDefinitionP (bimap (const ()) id (convertType type2)))

-- | Although the majority of graphql-engine is written in terms of abstract
-- mtl-style effect monads, we figured out that this particular codepath is
-- quite hot, and that mtl has a measurable negative effect for accumulating
-- types from the schema, both in profiling and in benchmarking.  Using an
-- explicit transformers-style effect stack seems to overall memory usage by
-- about 3-7%.
newtype TypeAccumulation origin a = TypeAccumulation
  { runTypeAccumulation ::
      ReaderT
        TypeOriginStack
        ( StateT
            (HashMap Name (SomeDefinitionTypeInfo origin, NonEmpty TypeOriginStack))
            (ExceptT (ConflictingDefinitions origin) Identity)
        )
        a
  }
  deriving (Functor, Applicative, Monad)
  deriving (MonadReader TypeOriginStack)
  deriving (MonadState (HashMap Name (SomeDefinitionTypeInfo origin, NonEmpty TypeOriginStack)))
  deriving (MonadError (ConflictingDefinitions origin))

class HasTypeDefinitions origin a where
  -- | Recursively accumulates all type definitions accessible from the given
  -- value. This is done statefully to avoid infinite loops arising from
  -- recursive type definitions; see Note [Tying the knot] in Hasura.GraphQL.Parser.Class.
  accumulateTypeDefinitions ::
    a -> TypeAccumulation origin ()

instance HasTypeDefinitions origin (Definition origin (TypeInfo origin k)) where
  accumulateTypeDefinitions new@Definition {..} = do
    -- This is the important case! We actually have a type definition, so we
    -- need to add it to the state.
    definitions <- get
    stack <- ask
    let someNew = SomeDefinitionTypeInfo new
    case HashMap.lookup dName definitions of
      Nothing -> do
        put $! HashMap.insert dName (someNew, pure stack) definitions
        -- This type definition might reference other type definitions, so we
        -- still need to recur.
        local (typeRootRecurse dName) $ accumulateTypeDefinitions dInfo
      Just (someOld, origins)
        -- It’s important we /don’t/ recur if we’ve already seen this definition
        -- before to avoid infinite loops; see Note [Tying the knot] in Hasura.GraphQL.Parser.Class.
        -- (NOTE: I tried making `origins` an STRef and doing a mutable update
        -- here but the performance was about the same)
        | someOld == someNew -> put $! HashMap.insert dName (someOld, stack `NE.cons` origins) definitions
        | otherwise -> throwError $ ConflictingDefinitions (someNew, stack) (someOld, origins)

instance (HasTypeDefinitions origin a) => HasTypeDefinitions origin [a] where
  accumulateTypeDefinitions = traverse_ accumulateTypeDefinitions

instance (HasTypeDefinitions origin a) => HasTypeDefinitions origin (Maybe a) where
  accumulateTypeDefinitions = traverse_ accumulateTypeDefinitions

instance HasTypeDefinitions origin (TypeDefinitionsWrapper origin) where
  accumulateTypeDefinitions (TypeDefinitionsWrapper x) = accumulateTypeDefinitions x

instance HasTypeDefinitions origin (Type origin k) where
  accumulateTypeDefinitions = \case
    TNamed _ t -> accumulateTypeDefinitions t
    TList _ t -> accumulateTypeDefinitions t

instance HasTypeDefinitions origin (TypeInfo origin k) where
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

instance HasTypeDefinitions origin (Definition origin (InputObjectInfo origin)) where
  accumulateTypeDefinitions = accumulateTypeDefinitions . fmap TIInputObject

instance HasTypeDefinitions origin (Definition origin (InputFieldInfo origin)) where
  accumulateTypeDefinitions Definition {..} =
    local (typeOriginRecurse dName) $ accumulateTypeDefinitions dInfo

instance HasTypeDefinitions origin (InputFieldInfo origin) where
  accumulateTypeDefinitions (InputFieldInfo t _) =
    accumulateTypeDefinitions t

instance HasTypeDefinitions origin (Definition origin (FieldInfo origin)) where
  accumulateTypeDefinitions Definition {..} =
    local (typeOriginRecurse dName) $ accumulateTypeDefinitions dInfo

instance HasTypeDefinitions origin (FieldInfo origin) where
  accumulateTypeDefinitions (FieldInfo args t) = do
    accumulateTypeDefinitions args
    accumulateTypeDefinitions t

instance HasTypeDefinitions origin (Definition origin (ObjectInfo origin)) where
  accumulateTypeDefinitions d@Definition {..} =
    local (typeOriginRecurse dName) $ accumulateTypeDefinitions (fmap TIObject d)

instance HasTypeDefinitions origin (Definition origin (InterfaceInfo origin)) where
  accumulateTypeDefinitions d@Definition {..} =
    local (typeOriginRecurse dName) $ accumulateTypeDefinitions (fmap TIInterface d)

instance HasTypeDefinitions origin (Definition origin (UnionInfo origin)) where
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
