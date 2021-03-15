{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE Arrows               #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.SQL.AnyBackend
  ( AnyBackend
  , mkAnyBackend
  , dispatchAnyBackend
  , dispatchAnyBackend'
  , dispatchAnyBackendArrow
  , unpackAnyBackend
  , composeAnyBackend
  , runBackend
  ) where

import           Hasura.Prelude

import qualified Data.Text                as T
import qualified Data.Text.Extended       as T

import           Control.Arrow.Extended   (ArrowChoice)
import           Data.Aeson               (FromJSON (..), ToJSON (..), Value (..), withObject,
                                           (.:?))
import           Data.Hashable            (Hashable (hashWithSalt))
import           Data.Kind                (Constraint, Type)
import           Test.QuickCheck          (oneof)

import           Hasura.RQL.Types.Backend
import           Hasura.SQL.Backend


-- | This type is essentially an unlabeled box for types indexed by BackendType.
-- Given some type defined as 'data T (b :: BackendType) = ...', we can define
-- 'AnyBackend T' without mentioning any 'BackendType'.
--
-- This is useful for having generic containers of potentially different types
-- of T.
data AnyBackend (i :: BackendType -> Type)
  = PostgresValue (i 'Postgres)
  | MSSQLValue    (i 'MSSQL)

instance
  ( Arbitrary (i 'Postgres)
  , Arbitrary (i 'MSSQL)
  ) => Arbitrary (AnyBackend i) where
  arbitrary = oneof
      [ PostgresValue <$> arbitrary
      , MSSQLValue    <$> arbitrary
      ]

mkAnyBackend
  :: forall
      (b :: BackendType)
      (i :: BackendType -> Type)
   . Backend b
  => i b
  -> AnyBackend i
mkAnyBackend =
  case (backendTag @b) of
    PostgresTag -> PostgresValue
    MSSQLTag    -> MSSQLValue

runBackend
  :: forall
      (i  :: BackendType -> Type)
      (r  :: Type)
   . AnyBackend i
  -> (forall (b :: BackendType). i b -> r)
  -> r
runBackend b f =
  case b of
    PostgresValue pg -> f pg
    MSSQLValue    ms -> f ms

-- | Dispatch an existential using an universally quantified function while
-- also resolving a different constraint.
-- Use this to dispatch Backend* instances.
-- This is essentially a wrapper around 'runAnyBackend f . repackAnyBackend @c'.
dispatchAnyBackend
  :: forall
      (c  :: BackendType -> Constraint)
      (i  :: BackendType -> Type)
      (r  :: Type)
   . c 'Postgres
  => c 'MSSQL
  => AnyBackend i
  -> (forall (b :: BackendType). c b => i b -> r)
  -> r
dispatchAnyBackend e f =
  case e of
    PostgresValue pg -> f pg
    MSSQLValue    ms -> f ms

-- | Unlike 'dispatchAnyBackend', the expected constraint has a different kind.
-- Use for classes like 'Show', 'ToJSON', etc.
dispatchAnyBackend'
  :: forall
      (c  :: Type -> Constraint)
      (i  :: BackendType -> Type)
      (r  :: Type)
   . c (i 'Postgres)
  => c (i 'MSSQL)
  => AnyBackend i
  -> (forall (b :: BackendType). c (i b) => i b -> r)
  -> r
dispatchAnyBackend' e f =
  case e of
    PostgresValue pg -> f pg
    MSSQLValue    ms -> f ms

-- | Sometimes we need to run operations on two backends of the same type.
-- If the backends don't contain the same type, the given 'r' value is returned.
-- Otherwise, the function is called with the two wrapped values.
composeAnyBackend
  :: forall
      (c :: BackendType -> Constraint)
      (i :: BackendType -> Type)
      (r :: Type)
   . c 'Postgres
  => c 'MSSQL
  => (forall (b :: BackendType). c b => i b -> i b -> r)
  -> AnyBackend i
  -> AnyBackend i
  -> r
  -> r
composeAnyBackend f e1 e2 owise =
  case (e1, e2) of
    (PostgresValue p1, PostgresValue p2) -> f p1 p2
    (MSSQLValue    m1, MSSQLValue    m2) -> f m1 m2
    _                                    -> owise

-- | Dispatch variant for use with arrow syntax. The universally quantified
-- dispatch function is an arrow instead.
-- TODO: How do we only "target" the 'i b' and ditch the tuple details (x y z).
dispatchAnyBackendArrow
  :: forall
       (c :: BackendType -> Constraint)
       (i :: BackendType -> Type)
       (r :: Type)
       (arr :: Type -> Type -> Type)
       x y z
   . ArrowChoice arr
  => c 'Postgres
  => c 'MSSQL
  => (forall b. c b => arr (x, y, i b, z) r)
  -> arr (x, y, AnyBackend i, z) r
dispatchAnyBackendArrow arrow = proc (x, y, exists, z) -> do
  case exists of
    PostgresValue pg -> arrow @'Postgres -< (x, y, pg, z)
    MSSQLValue    ms -> arrow @'MSSQL    -< (x, y, ms, z)

-- | Try to guess the type of an existential. 'Just' means you were right.
unpackAnyBackend
  :: forall
      (b :: BackendType)
      (i :: BackendType -> Type)
   . Backend b
  => AnyBackend i
  -> Maybe (i b)
unpackAnyBackend exists =
  case (backendTag @b, exists) of
    (PostgresTag, PostgresValue pg) -> Just pg
    (MSSQLTag   , MSSQLValue    ms) -> Just ms
    _                               -> Nothing

instance (ToJSON (i 'Postgres), ToJSON (i 'MSSQL)) => ToJSON (AnyBackend i) where
  toJSON e = dispatchAnyBackend' @ToJSON e toJSON

instance (Show (i 'Postgres), Show (i 'MSSQL)) => Show (AnyBackend i) where
  show e = dispatchAnyBackend' @Show e show

instance (Eq (i 'Postgres), Eq (i 'MSSQL)) => Eq (AnyBackend i) where
  e1 == e2 =
    case (e1, e2) of
      (PostgresValue p1, PostgresValue p2) -> p1 == p2
      (MSSQLValue    m1, MSSQLValue    m2) -> m1 == m2
      _                                    -> False

instance (FromJSON (i 'Postgres), FromJSON (i 'MSSQL)) => FromJSON (AnyBackend i) where
  parseJSON = withObject "Object" $ \o -> do
    backendKind :: Text <- fromMaybe "postgres" <$> o .:? "kind"
    -- TODO: Make backendKind a concrete type or re-use `BackendType`
    case backendKind of
      "postgres" -> PostgresValue <$> parseJSON (Object o)
      "mssql"    -> MSSQLValue    <$> parseJSON (Object o)
      _          -> fail
                      $ "expected one of: "
                      <> T.unpack (T.commaSeparated (T.toLower <$> supportedBackends))

instance (Hashable (i 'Postgres), Hashable (i 'MSSQL)) => Hashable (AnyBackend i) where
  hashWithSalt salt e = dispatchAnyBackend' @Hashable e (hashWithSalt salt)
