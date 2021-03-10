module Hasura.SQL.Backend where

import           Hasura.Prelude

import           Data.GADT.Compare
import           Type.Reflection
import           Unsafe.Coerce


-- | An enum that represents each backend we support.
data BackendType = Postgres | MSSQL
  deriving (Show, Eq, Ord, Bounded, Enum)


-- | A singleton-like GADT that associates a tag to each backend.
-- It must contain one tag per backend in @BackendType@.
data BackendTag (b :: BackendType) where
  PostgresTag :: BackendTag 'Postgres
  MSSQLTag    :: BackendTag 'MSSQL


-- | How to convert back from a tag to a runtime value.
reify :: BackendTag b -> BackendType
reify = \case
  PostgresTag -> Postgres
  MSSQLTag    -> MSSQL


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

{- Note [Recovering Existentially Quantified Type Information]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See Note [Existentially Quantified Types] for a brief introduction to existential
quantification.

In our codebase, we have a significant amount of types indexed by 'BackendType',
and a few existential wrappers on top of them. One such example can be found in
Hasura.RQL.Types.Source: the 'SourceInfo b' type is existentially quantified as
'BackendSourceInfo'.

The idea is that we want to have a heterogenous list (or map) of 'BackendSourceInfo'
values. However, we also want to be able to recover the information about the
specific source.

One way would be to have multiple constructors on 'BackendSourceInfo':

    data BackendSourceInfo where
        PostgresSourceInfo :: SourceInfo 'Postgres -> BackendSourceInfo
        MssqlSourceInfo    :: SourceInfo 'MSSQL    -> BackendSourceInfo
        -- etc

However, this would mean that we need to change a significant amount of code
when we add a new backend: we need to change all the wrappers and add a new
constructor, as well as all the pattern matches on them.

Instead, we use existential quantification which makes it such that we don't
have to add constructors when adding backends. The problem then is that we
might want to make decisions depending on the backend, but this information
is erased.

    data BackendSourceInfo =
        forall b. Backend b => BackendSourceInfo (SourceInfo b)

In order to circumvent this problem, we use a trick similar to the one found
in the 'singletons' library.

The trick involves creating a tag type, like BackendTag, which is isomorphic
to the backend type (generally, we want as many elements in this tag type
as different types we need to differentiate from the existential).

Using this type, we can add the following method to the `Backend` class:

    backendTag :: BackendTag 'b

and implement it in each backend's instance. Now we can recover the type:

    f :: BackendSourceInfo -> String
    f (BackendSourceInfo (so :: SourceInfo b)) =
        case backendTag @b of
            PostgresTag -> "Got Postgres!"
            MSSQLTag    -> "Got MSSQL!"

But that's not all! In the context of each case branch, the type of 'so'
is actually `so :: SourceInfo 'Postgres`, and `so :: SourceInfo 'MSSQL`
respectively, so we can call backend-specific functions!

IMPORTANT: Please note that this function cannot be written:

    impossible :: forall b. BackendSourceInfo -> SourceInfo b

The problem here is that 'BackendSourceInfo' contains one specific instance
of a 'SourceInfo', but the function 'impossible' advertises that a caller
may pick any backend.

However, this function may be written:

    tryCastPostgres :: BackendSourceInfo -> Maybe (SourceInfo 'Postgres)
    tryCastPostgres (BackendSourceInfo (so :: SourceInfo b)) =
        case backendTag @b of
            PostgresTag -> Just so
            MSSQLTag    -> Nothing -}
