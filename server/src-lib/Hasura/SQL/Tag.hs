module Hasura.SQL.Tag
  ( BackendTag(..)
  , HasTag(..)
  , reify
  ) where

import           Hasura.Prelude

import           Data.GADT.Compare   (GCompare (..), GEq (..), GOrdering (GEQ, GGT, GLT))
import           Language.Haskell.TH hiding (reify)
import           Type.Reflection     (type (:~:) (Refl))
import           Unsafe.Coerce       (unsafeCoerce)

import           Hasura.SQL.Backend
import           Hasura.SQL.TH


-- | A singleton-like GADT that associates a tag to each backend.
-- It is generated with Template Haskell for each 'Backend'. Its
-- declaration results in the following type:
--
--   data BackendTag (b :: BackendType) where
--     PostgresVanillaTag :: BackendTag ('Postgres 'Vanilla)
--     PostgresCitusTag   :: BackendTag ('Postgres 'Citus)
--     MSSQLTag           :: BackendTag 'MSSQL
--     ...
$(let name = mkName "BackendTag" in
  backendData
  -- the name of the type
  name
  -- the type variable
  [KindedTV (mkName "b") $ ConT ''BackendType]
  -- the constructor for each backend
  (\b -> pure $ GadtC
    -- the name of the constructor (FooTag)
    [getBackendTagName b]
    -- no type argument
    []
    -- the resulting type (BackendTag 'Foo)
    (AppT (ConT name) (getBackendTypeValue b))
  )
  -- deriving clauses
  []
 )


-- | This class describes how to get a tag for a given type.
-- We use it in AnyBackend: `case backendTag @b of`...
class HasTag (b :: BackendType) where
  backendTag :: BackendTag b

-- | This generates the instance of HasTag for every backend.
$(concat <$> forEachBackend \b -> do
  -- the name of the tag: FooTag
  let tagName      = pure $ ConE $ getBackendTagName b
  -- the promoted version of b: 'Foo
  let promotedName = pure $ getBackendTypeValue b
  -- the instance:
  --  instance HasTag 'Foo          where backendTag = FooTag
  [d| instance HasTag $promotedName where backendTag = $tagName |]
 )


-- | How to convert back from a tag to a runtime value. This function
-- is generated with Template Haskell for each 'Backend'. The case
-- switch looks like this:
--
--   PostgresVanillaTag -> Postgres Vanilla
--   PostgresCitusTag   -> Postgres Citus
--   MSSQLTag           -> MSSQL
--   ...
reify :: BackendTag b -> BackendType
reify t = $(backendCase
  -- the expression on which we do the case switch
  [| t |]
  -- the pattern for a given backend: just its tag, no argument
  (\b -> pure $ ConP (getBackendTagName b) [])
  -- the body for a given backend: the backend constructor itself
  (\b -> pure $ getBackendValue b)
  -- no default case: every constructor should be handled
  Nothing
  )


-- We need those instances to be able to use a @BackendTag@ as a key in a
-- dependent map. Using @BackendType@ as a data kind, makes it difficult to use
-- @Typeable@, hence the reliance on `unsafeCoerce`.
instance GEq BackendTag where
  geq b1 b2
    | reify b1 == reify b2 = unsafeCoerce $ Just Refl
    | otherwise            = Nothing

instance GCompare BackendTag where
  gcompare b1 b2 = case compare (reify b1) (reify b2) of
    EQ -> unsafeCoerce GEQ
    LT -> GLT
    GT -> GGT
