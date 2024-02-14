{-# LANGUAGE Arrows #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Dispatch over backends.
--
-- = Creating and consuming 'AnyBackend'
--
-- Creating a new value of type 'AnyBackend' is done via 'mkAnyBackend'.
--
-- Consuming a value of type 'AnyBackend' is done via either 'runAnyBackend' or
-- any of the dispatch functions ('dispatchAnyBackend', 'dispatchAnyBackend'',
-- 'dispatchAnyBackend''').
--
-- For implementation details, or when trying to understand this module, start
-- from 'AnyBackend'.
--
-- = Backend Architecture
--
-- Our multiple backend architecture uses type classes and associated types
-- in order to share code, such as parsing graphql queries, building
-- schemas and metadata, while still accounting for the differences between
-- backends.
--
-- Each backend implements the @Backend@ type class from "Hasura.RQL.Types.Backend"
-- as well as instances for other classes such as @BackendSchema@ from
-- "Hasura.GraphQL.Schema.Backend", and define the associated types and
-- functions, such as @ScalarType@ and @parseScalarValue@, which fit the backend.
--
-- Whenever one of these associated types (@ScalarType@, @Column@, etc.) are
-- used, we need to either push the 'BackendType' to our caller (and making our
-- type @BackendType -> Type@), or use 'AnyBackend' (and allow our type to be
-- 'Type'). This is particularly useful when we need to store a container of
-- any backend.
--
-- In order to actually program abstractly using type classes, we need the
-- type class instances to be available for us to use. This module is a trick
-- to enumerate all supported backends and their respective instances to convince
-- GHC that they can be used.
--
-- = Example usage
--
-- As an example of using this module, consider wanting to write a function
-- that calculates metrics for each source. For example, we want to count
-- the number of tables each source has.
--
-- The @SchemaCache@ (defined in "Hasura.RQL.Types.SchemaCache") holds a hash map
-- from each source to their information.
-- The source information is parameterized by the 'BackendType' and is hidden
-- using an existential type inside 'AnyBackend'. It essentially looks like this:
--
-- > data SourceInfo b = ...
-- >
-- > type SourceCache = HashMap SourceName (AnyBackend SourceInfo)
--
-- Our metrics calculation function cares which backend it receives, but only
-- for its type class instances so it can call the relevant functions:
--
-- > telemetryForSource :: forall (b :: BackendType). SourceInfo b -> TelemetryPayload
--
-- In order to apply this function to all backends and return the telemetry payload for each,
-- we need to map over the hash map and dispatch the function over the relevant backend.
-- we can do this with 'runBackend':
--
-- > telemetries =
-- >   map
-- >     (`runBackend` telemetryForSource)
-- >     (scSources schemaCache)
--
-- If we want to be able to extract some information about the backend type
-- inside @telemetryForSource@, we can do this using 'backendTag':
--
-- > let telemetryForSource :: forall (b :: BackendType). HasTag b => SourceInfo b -> TelemetryPayload
-- >     telemetryForSource =
-- >       let dbKind = reify (backendTag @b)
--
-- Note that we needed to add the 'HasTag' constraint, which now means we can't use 'runBackend'
-- because our function has the wrong type (it has an extra constraint).
-- Instead, we can use 'dispatchAnyBackend' which allows us to have one constraint:
--
-- > telemetries =
-- >   fmap
-- >     (\sourceinfo -> (Any.dispatchAnyBackend @HasTag) sourceinfo telemetryForSource)
-- >     (scSources schemaCache)
--
-- Note that we had to add the constraint name as a type application, and we had
-- to explicitly add a lambda instead of using 'flip'.
module Hasura.SQL.AnyBackend
  ( AnyBackend,
    SatisfiesForAllBackends,
    liftTag,
    lowerTag,
    mkAnyBackend,
    mapBackend,
    traverseBackend,
    dispatchAnyBackend,
    dispatchAnyBackend',
    dispatchAnyBackend'',
    dispatchAnyBackendArrow,
    dispatchAnyBackendWithTwoConstraints,
    mergeAnyBackend,
    unpackAnyBackend,
    composeAnyBackend,
    runBackend,
    parseAnyBackendFromJSON,
    anyBackendCodec,
    debugAnyBackendToJSON,
    backendSourceKindFromText,
    parseBackendSourceKindFromJSON,
  )
where

import Autodocodec (HasCodec (codec), JSONCodec, dimapCodec)
import Control.Applicative
import Control.Arrow.Extended (ArrowChoice)
import Control.Lens (preview, _Right)
import Data.Aeson
import Data.Aeson.Extended
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types (Parser)
import Data.Kind (Constraint, Type)
import Hasura.Prelude
import Hasura.RQL.Types.BackendTag
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.DataConnector (mkDataConnectorName)
import Language.GraphQL.Draft.Syntax qualified as GQL

--------------------------------------------------------------------------------

-- * Types and constraints

-- | Allows storing types of kind @BackendType -> Type@ heterogenously.
--
-- Adding a new constructor to 'BackendType' will automatically create a new
-- constructor here.
--
-- Given some type defined as @data T (b :: BackendType) = ...@, we can define
-- @AnyBackend T@ without mentioning any 'BackendType'.
--
-- This is useful for having generic containers of potentially different types
-- of T. For instance, @SourceCache@ is defined as a
-- @HashMap SourceName (AnyBackend SourceInfo)@.
data AnyBackend (i :: BackendType -> Type)
  = PostgresVanillaValue (i ('Postgres 'Vanilla))
  | PostgresCitusValue (i ('Postgres 'Citus))
  | PostgresCockroachValue (i ('Postgres 'Cockroach))
  | MSSQLValue (i 'MSSQL)
  | BigQueryValue (i 'BigQuery)
  | DataConnectorValue (i 'DataConnector)
  deriving (Generic)

-- | Generates a constraint for all backends.
type AllBackendsSatisfy (c :: BackendType -> Constraint) =
  ( c ('Postgres 'Vanilla),
    c ('Postgres 'Citus),
    c ('Postgres 'Cockroach),
    c 'MSSQL,
    c 'BigQuery,
    c 'DataConnector
  )

-- | Generates a constraint for a generic type over all backends.
type SatisfiesForAllBackends
  (i :: BackendType -> Type)
  (c :: Type -> Constraint) =
  ( c (i ('Postgres 'Vanilla)),
    c (i ('Postgres 'Citus)),
    c (i ('Postgres 'Cockroach)),
    c (i 'MSSQL),
    c (i 'BigQuery),
    c (i 'DataConnector)
  )

--------------------------------------------------------------------------------

-- * Functions on AnyBackend

-- | How to obtain a tag from a runtime value.
liftTag :: BackendType -> AnyBackend BackendTag
liftTag (Postgres Vanilla) = PostgresVanillaValue PostgresVanillaTag
liftTag (Postgres Citus) = PostgresCitusValue PostgresCitusTag
liftTag (Postgres Cockroach) = PostgresCockroachValue PostgresCockroachTag
liftTag MSSQL = MSSQLValue MSSQLTag
liftTag BigQuery = BigQueryValue BigQueryTag
liftTag DataConnector = DataConnectorValue DataConnectorTag

-- | Obtain a @BackendType@ from a runtime value.
lowerTag :: AnyBackend i -> BackendType
lowerTag (PostgresVanillaValue _) = Postgres Vanilla
lowerTag (PostgresCitusValue _) = Postgres Citus
lowerTag (PostgresCockroachValue _) = Postgres Cockroach
lowerTag (MSSQLValue _) = MSSQL
lowerTag (BigQueryValue _) = BigQuery
lowerTag (DataConnectorValue _) = DataConnector

-- | Transforms an @AnyBackend i@ into an @AnyBackend j@.
mapBackend ::
  forall
    (i :: BackendType -> Type)
    (j :: BackendType -> Type).
  AnyBackend i ->
  (forall b. i b -> j b) ->
  AnyBackend j
mapBackend e f = case e of
  PostgresVanillaValue x -> PostgresVanillaValue (f x)
  PostgresCitusValue x -> PostgresCitusValue (f x)
  PostgresCockroachValue x -> PostgresCockroachValue (f x)
  MSSQLValue x -> MSSQLValue (f x)
  BigQueryValue x -> BigQueryValue (f x)
  DataConnectorValue x -> DataConnectorValue (f x)

-- | Traverse an @AnyBackend i@ into an @f (AnyBackend j)@.
traverseBackend ::
  forall
    (c :: BackendType -> Constraint)
    (i :: BackendType -> Type)
    (j :: BackendType -> Type)
    f.
  (AllBackendsSatisfy c, Functor f) =>
  AnyBackend i ->
  (forall b. (c b) => i b -> f (j b)) ->
  f (AnyBackend j)
traverseBackend e f = case e of
  PostgresVanillaValue x -> PostgresVanillaValue <$> f x
  PostgresCitusValue x -> PostgresCitusValue <$> f x
  PostgresCockroachValue x -> PostgresCockroachValue <$> f x
  MSSQLValue x -> MSSQLValue <$> f x
  BigQueryValue x -> BigQueryValue <$> f x
  DataConnectorValue x -> DataConnectorValue <$> f x

-- | Creates a new @AnyBackend i@ for a given backend @b@ by wrapping the given @i b@.
mkAnyBackend ::
  forall
    (b :: BackendType)
    (i :: BackendType -> Type).
  (HasTag b) =>
  i b ->
  AnyBackend i
mkAnyBackend x = case backendTag @b of
  PostgresVanillaTag -> PostgresVanillaValue x
  PostgresCitusTag -> PostgresCitusValue x
  PostgresCockroachTag -> PostgresCockroachValue x
  MSSQLTag -> MSSQLValue x
  BigQueryTag -> BigQueryValue x
  DataConnectorTag -> DataConnectorValue x

-- | Dispatch a function to the value inside the @AnyBackend@, that does not
-- require bringing into scope a new class constraint.
runBackend ::
  forall
    (i :: BackendType -> Type)
    (r :: Type).
  AnyBackend i ->
  (forall (b :: BackendType). i b -> r) ->
  r
runBackend b f = case b of
  PostgresVanillaValue x -> f x
  PostgresCitusValue x -> f x
  PostgresCockroachValue x -> f x
  MSSQLValue x -> f x
  BigQueryValue x -> f x
  DataConnectorValue x -> f x

-- | Dispatch an existential using an universally quantified function while
-- also resolving a different constraint.
-- Use this to dispatch Backend* instances.
-- This is essentially a wrapper around @runAnyBackend f . repackAnyBackend \@c@.
dispatchAnyBackend ::
  forall
    (c :: BackendType -> Constraint)
    (i :: BackendType -> Type)
    (r :: Type).
  (AllBackendsSatisfy c) =>
  AnyBackend i ->
  (forall (b :: BackendType). (c b) => i b -> r) ->
  r
dispatchAnyBackend e f = case e of
  PostgresVanillaValue x -> f x
  PostgresCitusValue x -> f x
  PostgresCockroachValue x -> f x
  MSSQLValue x -> f x
  BigQueryValue x -> f x
  DataConnectorValue x -> f x

dispatchAnyBackendWithTwoConstraints ::
  forall
    (c1 :: BackendType -> Constraint)
    (c2 :: BackendType -> Constraint)
    (i :: BackendType -> Type)
    (r :: Type).
  (AllBackendsSatisfy c1) =>
  (AllBackendsSatisfy c2) =>
  AnyBackend i ->
  (forall (b :: BackendType). (c1 b) => (c2 b) => i b -> r) ->
  r
dispatchAnyBackendWithTwoConstraints e f = case e of
  PostgresVanillaValue x -> f x
  PostgresCitusValue x -> f x
  PostgresCockroachValue x -> f x
  MSSQLValue x -> f x
  BigQueryValue x -> f x
  DataConnectorValue x -> f x

-- | Unlike 'dispatchAnyBackend', the expected constraint has a different kind.
-- Use for classes like 'Show', 'ToJSON', etc.
dispatchAnyBackend' ::
  forall
    (c :: Type -> Constraint)
    (i :: BackendType -> Type)
    (r :: Type).
  (i `SatisfiesForAllBackends` c) =>
  AnyBackend i ->
  (forall (b :: BackendType). (c (i b)) => i b -> r) ->
  r
dispatchAnyBackend' e f = case e of
  PostgresVanillaValue x -> f x
  PostgresCitusValue x -> f x
  PostgresCockroachValue x -> f x
  MSSQLValue x -> f x
  BigQueryValue x -> f x
  DataConnectorValue x -> f x

-- | This allows you to apply a constraint to the Backend instances (c2)
-- as well as a constraint on the higher-kinded @i b@ type (c1)
dispatchAnyBackend'' ::
  forall
    (c1 :: Type -> Constraint)
    (c2 :: BackendType -> Constraint)
    (i :: BackendType -> Type)
    (r :: Type).
  (i `SatisfiesForAllBackends` c1) =>
  (AllBackendsSatisfy c2) =>
  AnyBackend i ->
  (forall (b :: BackendType). (c2 b) => (c1 (i b)) => i b -> r) ->
  r
dispatchAnyBackend'' e f = case e of
  PostgresVanillaValue x -> f x
  PostgresCitusValue x -> f x
  PostgresCockroachValue x -> f x
  MSSQLValue x -> f x
  BigQueryValue x -> f x
  DataConnectorValue x -> f x

-- | Sometimes we need to run operations on two backends of the same type.
-- If the backends don't contain the same type, the given @r@ value is returned.
-- Otherwise, the function is called with the two wrapped values.
composeAnyBackend ::
  forall
    (c :: BackendType -> Constraint)
    (i :: BackendType -> Type)
    (r :: Type).
  (AllBackendsSatisfy c) =>
  (forall (b :: BackendType). (c b) => i b -> i b -> r) ->
  AnyBackend i ->
  AnyBackend i ->
  r ->
  r
composeAnyBackend f e1 e2 owise = case (e1, e2) of
  (PostgresVanillaValue x, PostgresVanillaValue y) -> f x y
  (PostgresCitusValue x, PostgresCitusValue y) -> f x y
  (PostgresCockroachValue x, PostgresCockroachValue y) -> f x y
  (MSSQLValue x, MSSQLValue y) -> f x y
  (BigQueryValue x, BigQueryValue y) -> f x y
  (DataConnectorValue x, DataConnectorValue y) -> f x y
  (value1, value2) ->
    if mapBackend value1 (Const . const ()) == mapBackend value2 (Const . const ())
      then error "Programming error: missing case in composeAnyBackend"
      else owise

-- | Merge two matching backends, falling back on a default.
mergeAnyBackend ::
  forall
    (c :: Type -> Constraint)
    (i :: BackendType -> Type).
  (i `SatisfiesForAllBackends` c) =>
  (forall (b :: BackendType). (c (i b)) => i b -> i b -> i b) ->
  AnyBackend i ->
  AnyBackend i ->
  AnyBackend i ->
  AnyBackend i
mergeAnyBackend f e1 e2 owise = case (e1, e2) of
  (PostgresVanillaValue x, PostgresVanillaValue y) -> PostgresVanillaValue (f x y)
  (PostgresCitusValue x, PostgresCitusValue y) -> PostgresCitusValue (f x y)
  (PostgresCockroachValue x, PostgresCockroachValue y) -> PostgresCockroachValue (f x y)
  (MSSQLValue x, MSSQLValue y) -> MSSQLValue (f x y)
  (BigQueryValue x, BigQueryValue y) -> BigQueryValue (f x y)
  (DataConnectorValue x, DataConnectorValue y) -> DataConnectorValue (f x y)
  (value1, value2) ->
    if mapBackend value1 (Const . const ()) == mapBackend value2 (Const . const ())
      then error "Programming error: missing case in mergeAnyBackend"
      else owise

-- | Try to unpack the type of an existential.
-- Returns @Just x@ upon a succesful match, @Nothing@ otherwise.
unpackAnyBackend ::
  forall
    (b :: BackendType)
    (i :: BackendType -> Type).
  (HasTag b) =>
  AnyBackend i ->
  Maybe (i b)
unpackAnyBackend exists = case (backendTag @b, exists) of
  (PostgresVanillaTag, PostgresVanillaValue x) -> Just x
  (PostgresCitusTag, PostgresCitusValue x) -> Just x
  (PostgresCockroachTag, PostgresCockroachValue x) -> Just x
  (MSSQLTag, MSSQLValue x) -> Just x
  (BigQueryTag, BigQueryValue x) -> Just x
  (DataConnectorTag, DataConnectorValue x) -> Just x
  (tag, value) ->
    if mapBackend (mkAnyBackend tag) (Const . const ()) == mapBackend value (Const . const ())
      then error "Programming error: missing case in unpackAnyBackend"
      else Nothing

--------------------------------------------------------------------------------
--

-- * Special case for arrows

-- | Dispatch variant for use with arrow syntax.
--
-- NOTE: The below function accepts two constraints, if the arrow
-- you want to dispatch only has one constraint then repeat the constraint twice.
-- For example:
--
-- > AB.dispatchAnyBackendArrow @BackendMetadata @BackendMetadata (proc (sourceMetadata, invalidationKeys)
dispatchAnyBackendArrow ::
  forall
    (c1 :: BackendType -> Constraint)
    (c2 :: BackendType -> Constraint)
    (i :: BackendType -> Type)
    (r :: Type)
    (arr :: Type -> Type -> Type)
    x.
  (ArrowChoice arr, AllBackendsSatisfy c1, AllBackendsSatisfy c2) =>
  (forall b. (c1 b) => (c2 b) => arr (i b, x) r) ->
  arr (AnyBackend i, x) r
dispatchAnyBackendArrow arrow = proc (ab, x) -> do
  case ab of
    PostgresVanillaValue val ->
      arrow @('Postgres 'Vanilla) -< (val, x)
    PostgresCitusValue val ->
      arrow @('Postgres 'Citus) -< (val, x)
    PostgresCockroachValue val ->
      arrow @('Postgres 'Cockroach) -< (val, x)
    MSSQLValue val ->
      arrow @'MSSQL -< (val, x)
    BigQueryValue val ->
      arrow @'BigQuery -< (val, x)
    DataConnectorValue val ->
      arrow @'DataConnector -< (val, x)

--------------------------------------------------------------------------------

-- * JSON functions

-- | Attempts to parse an 'AnyBackend' from a JSON value, using the provided
-- backend information.
parseAnyBackendFromJSON ::
  (i `SatisfiesForAllBackends` FromJSON) =>
  BackendType ->
  Value ->
  Parser (AnyBackend i)
parseAnyBackendFromJSON backendKind value = case backendKind of
  Postgres Vanilla -> PostgresVanillaValue <$> parseJSON value
  Postgres Citus -> PostgresCitusValue <$> parseJSON value
  Postgres Cockroach -> PostgresCockroachValue <$> parseJSON value
  MSSQL -> MSSQLValue <$> parseJSON value
  BigQuery -> BigQueryValue <$> parseJSON value
  DataConnector -> DataConnectorValue <$> parseJSON value

-- | Codec that can be used to decode and encode @AnyBackend i@ values. Throws
-- an error when attempting to encode a value with a mismatched @backendKind@
-- argument.
anyBackendCodec ::
  forall i.
  (i `SatisfiesForAllBackends` HasCodec) =>
  BackendType ->
  JSONCodec (AnyBackend i)
anyBackendCodec backendKind = case backendKind of
  Postgres Vanilla -> dimapCodec PostgresVanillaValue (\case (PostgresVanillaValue v) -> v; _ -> error msg) $ codec @(i ('Postgres 'Vanilla))
  Postgres Citus -> dimapCodec PostgresCitusValue (\case (PostgresCitusValue v) -> v; _ -> error msg) $ codec @(i ('Postgres 'Citus))
  Postgres Cockroach -> dimapCodec PostgresCockroachValue (\case (PostgresCockroachValue v) -> v; _ -> error msg) $ codec @(i ('Postgres 'Cockroach))
  MSSQL -> dimapCodec MSSQLValue (\case (MSSQLValue v) -> v; _ -> error msg) $ codec @(i 'MSSQL)
  BigQuery -> dimapCodec BigQueryValue (\case (BigQueryValue v) -> v; _ -> error msg) $ codec @(i 'BigQuery)
  DataConnector -> dimapCodec DataConnectorValue (\case (DataConnectorValue v) -> v; _ -> error msg) $ codec @(i 'DataConnector)
  where
    msg = "got unexpected backend type indicating anyBackendCodec was called with the wrong backendType value"

-- | Outputs a debug JSON value from an 'AnyBackend'. This function must only be
-- used for debug purposes, as it has no way of inserting the backend kind in
-- the output, since there's no guarantee that the output will be an object.
debugAnyBackendToJSON ::
  (i `SatisfiesForAllBackends` ToJSON) =>
  AnyBackend i ->
  Value
debugAnyBackendToJSON e = dispatchAnyBackend' @ToJSON e toJSON

--------------------------------------------------------------------------------

-- * Instances for 'AnyBackend'

deriving instance (i `SatisfiesForAllBackends` Show) => Show (AnyBackend i)

deriving instance (i `SatisfiesForAllBackends` Eq) => Eq (AnyBackend i)

deriving instance (i `SatisfiesForAllBackends` Ord) => Ord (AnyBackend i)

instance (i `SatisfiesForAllBackends` Hashable) => Hashable (AnyBackend i)

instance (i `SatisfiesForAllBackends` FromJSON) => FromJSONKeyValue (AnyBackend i) where
  parseJSONKeyValue (backendTypeStr, value) = do
    backendType <- parseBackendTypeFromText $ Key.toText backendTypeStr
    parseAnyBackendFromJSON backendType value

backendSourceKindFromText :: Text -> Maybe (AnyBackend BackendSourceKind)
backendSourceKindFromText text =
  PostgresVanillaValue <$> staticKindFromText PostgresVanillaKind
    <|> PostgresCitusValue <$> staticKindFromText PostgresCitusKind
    <|> PostgresCockroachValue <$> staticKindFromText PostgresCockroachKind
    <|> MSSQLValue <$> staticKindFromText MSSQLKind
    <|> BigQueryValue <$> staticKindFromText BigQueryKind
    -- IMPORTANT: This must be the last thing here, since it will accept (almost) any string
    <|> DataConnectorValue . DataConnectorKind <$> (preview _Right . mkDataConnectorName =<< GQL.mkName text)
  where
    staticKindFromText :: BackendSourceKind b -> Maybe (BackendSourceKind b)
    staticKindFromText kind =
      if text `elem` backendTextNames (backendTypeFromBackendSourceKind kind)
        then Just kind
        else Nothing

parseBackendSourceKindFromJSON :: Value -> Parser (AnyBackend BackendSourceKind)
parseBackendSourceKindFromJSON value =
  PostgresVanillaValue <$> parseJSON @(BackendSourceKind ('Postgres 'Vanilla)) value
    <|> PostgresCitusValue <$> parseJSON @(BackendSourceKind ('Postgres 'Citus)) value
    <|> PostgresCockroachValue <$> parseJSON @(BackendSourceKind ('Postgres 'Cockroach)) value
    <|> MSSQLValue <$> parseJSON @(BackendSourceKind ('MSSQL)) value
    <|> BigQueryValue <$> parseJSON @(BackendSourceKind ('BigQuery)) value
    -- IMPORTANT: This must the last thing here, since it will accept (almost) any string
    <|> DataConnectorValue <$> parseJSON @(BackendSourceKind ('DataConnector)) value
