{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.Types.Source
  ( -- * Metadata
    SourceInfo (..),
    BackendSourceInfo,
    SourceCache,
    unsafeSourceConfiguration,
    unsafeSourceFunctions,
    unsafeSourceInfo,
    unsafeSourceName,
    unsafeSourceTables,
    siConfiguration,
    siFunctions,
    siName,
    siQueryTagsConfig,
    siTables,
    siCustomization,

    -- * Schema cache
    ResolvedSource (..),
    ScalarSet (..),

    -- * Source resolver
    SourceResolver,
    MonadResolveSource (..),
    MaintenanceModeVersion (..),
  )
where

import Control.Lens hiding ((.=))
import Data.Aeson.Extended
import Database.PG.Query qualified as Q
import Hasura.Base.Error
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Function
import Hasura.RQL.Types.Instances ()
import Hasura.RQL.Types.QueryTags
import Hasura.RQL.Types.SourceCustomization
import Hasura.RQL.Types.Table
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.SQL.Backend
import Hasura.SQL.Tag
import Hasura.Tracing qualified as Tracing

--------------------------------------------------------------------------------
-- Metadata

data SourceInfo b = SourceInfo
  { _siName :: !SourceName,
    _siTables :: !(TableCache b),
    _siFunctions :: !(FunctionCache b),
    _siConfiguration :: !(SourceConfig b),
    _siQueryTagsConfig :: !(Maybe QueryTagsConfig),
    _siCustomization :: !SourceCustomization
  }
  deriving (Generic)

$(makeLenses ''SourceInfo)

instance (Backend b, ToJSONKeyValue (BooleanOperators b (PartialSQLExp b))) => ToJSON (SourceInfo b) where
  toJSON = genericToJSON hasuraJSON

type BackendSourceInfo = AB.AnyBackend SourceInfo

type SourceCache = HashMap SourceName BackendSourceInfo

-- Those functions cast the content of BackendSourceInfo in order to extract
-- a backend-specific SourceInfo. Ideally, those functions should NOT be used:
-- the rest of the code should be able to deal with any source, regardless of
-- backend, through usage of the appropriate typeclasses.
-- They are thus a temporary workaround as we work on generalizing code that
-- uses the schema cache.

unsafeSourceInfo :: forall b. HasTag b => BackendSourceInfo -> Maybe (SourceInfo b)
unsafeSourceInfo = AB.unpackAnyBackend

unsafeSourceName :: BackendSourceInfo -> SourceName
unsafeSourceName bsi = AB.dispatchAnyBackend @Backend bsi go
  where
    go (SourceInfo name _ _ _ _ _) = name

unsafeSourceTables :: forall b. HasTag b => BackendSourceInfo -> Maybe (TableCache b)
unsafeSourceTables = fmap _siTables . unsafeSourceInfo @b

unsafeSourceFunctions :: forall b. HasTag b => BackendSourceInfo -> Maybe (FunctionCache b)
unsafeSourceFunctions = fmap _siFunctions . unsafeSourceInfo @b

unsafeSourceConfiguration :: forall b. HasTag b => BackendSourceInfo -> Maybe (SourceConfig b)
unsafeSourceConfiguration = fmap _siConfiguration . unsafeSourceInfo @b

--------------------------------------------------------------------------------
-- Schema cache

-- | Contains Postgres connection configuration and essential metadata from the
-- database to build schema cache for tables and function.
data ResolvedSource b = ResolvedSource
  { _rsConfig :: SourceConfig b,
    _rsCustomization :: SourceTypeCustomization,
    _rsTables :: DBTablesMetadata b,
    _rsFunctions :: DBFunctionsMetadata b,
    _rsScalars :: ScalarSet b
  }

instance (L.ToEngineLog (ResolvedSource b) L.Hasura) where
  toEngineLog _ = (L.LevelDebug, L.ELTStartup, toJSON rsLog)
    where
      rsLog =
        object
          [ "kind" .= ("resolve_source" :: Text),
            "info" .= ("Successfully resolved source" :: Text)
          ]

-- | A set of scalar types for a given backend.
--
-- 'ScalarType' is a type family, and as such cannot be used in an
-- `AnyBackend`. This lighttweight newtype wrapper allows us to do so.
data ScalarSet b where
  ScalarSet :: Backend b => HashSet (ScalarType b) -> ScalarSet b

instance Backend b => Semigroup (ScalarSet b) where
  ScalarSet s1 <> ScalarSet s2 = ScalarSet $ s1 <> s2

instance Backend b => Monoid (ScalarSet b) where
  mempty = ScalarSet mempty

--------------------------------------------------------------------------------
-- Source resolver

-- | FIXME: this should be either in 'BackendMetadata', or into a new dedicated
-- 'BackendResolve', instead of listing backends explicitly. It could also be
-- moved to the app level.
type SourceResolver b =
  SourceName -> SourceConnConfiguration b -> IO (Either QErr (SourceConfig b))

class (Monad m) => MonadResolveSource m where
  getPGSourceResolver :: m (SourceResolver ('Postgres 'Vanilla))
  getMSSQLSourceResolver :: m (SourceResolver 'MSSQL)

instance (MonadResolveSource m) => MonadResolveSource (ExceptT e m) where
  getPGSourceResolver = lift getPGSourceResolver
  getMSSQLSourceResolver = lift getMSSQLSourceResolver

instance (MonadResolveSource m) => MonadResolveSource (ReaderT r m) where
  getPGSourceResolver = lift getPGSourceResolver
  getMSSQLSourceResolver = lift getMSSQLSourceResolver

instance (MonadResolveSource m) => MonadResolveSource (Tracing.TraceT m) where
  getPGSourceResolver = lift getPGSourceResolver
  getMSSQLSourceResolver = lift getMSSQLSourceResolver

instance (MonadResolveSource m) => MonadResolveSource (Q.TxET QErr m) where
  getPGSourceResolver = lift getPGSourceResolver
  getMSSQLSourceResolver = lift getMSSQLSourceResolver

-- FIXME: why is this here?
data MaintenanceModeVersion
  = -- | should correspond to the source catalog version from which the user
    -- is migrating from
    PreviousMMVersion
  | -- | should correspond to the latest source catalog version
    CurrentMMVersion
  deriving (Show, Eq)
