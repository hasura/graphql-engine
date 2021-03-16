module Hasura.SQL.Backend
  ( BackendType(..)
  , BackendTag(..)
  , reify
  , backendName
  , supportedBackends
  ) where

import           Hasura.Prelude

import           Data.GADT.Compare (GCompare (..), GEq (..), GOrdering (GEQ, GGT, GLT))
import           Type.Reflection   (type (:~:) (Refl))
import           Unsafe.Coerce     (unsafeCoerce)


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

backendName :: BackendTag b -> Text
backendName = \case
  PostgresTag -> "postgres"
  MSSQLTag    -> "mssql"

supportedBackends :: [Text]
supportedBackends = tshow <$> [minBound @BackendType .. maxBound]

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

