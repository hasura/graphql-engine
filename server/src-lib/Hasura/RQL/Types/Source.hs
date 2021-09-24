{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.Types.Source where

import Control.Lens
import Data.Aeson.Extended
import Data.HashMap.Strict qualified as M
import Database.PG.Query qualified as Q
import Hasura.Backends.Postgres.Connection
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Function
import Hasura.RQL.Types.Instances ()
import Hasura.RQL.Types.QueryTags
import Hasura.RQL.Types.Table
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.SQL.Backend
import Hasura.SQL.Tag
import Hasura.Session
import Hasura.Tracing qualified as Tracing

data SourceInfo b = SourceInfo
  { _siName :: !SourceName,
    _siTables :: !(TableCache b),
    _siFunctions :: !(FunctionCache b),
    _siConfiguration :: !(SourceConfig b),
    _siQueryTagsConfig :: !(Maybe QueryTagsConfig)
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
    go (SourceInfo name _ _ _ _) = name

unsafeSourceTables :: forall b. HasTag b => BackendSourceInfo -> Maybe (TableCache b)
unsafeSourceTables = fmap _siTables . unsafeSourceInfo @b

unsafeSourceFunctions :: forall b. HasTag b => BackendSourceInfo -> Maybe (FunctionCache b)
unsafeSourceFunctions = fmap _siFunctions . unsafeSourceInfo @b

unsafeSourceConfiguration :: forall b. HasTag b => BackendSourceInfo -> Maybe (SourceConfig b)
unsafeSourceConfiguration = fmap _siConfiguration . unsafeSourceInfo @b

getTableRoles :: BackendSourceInfo -> [RoleName]
getTableRoles bsi = AB.dispatchAnyBackend @Backend bsi go
  where
    go si = M.keys . _tiRolePermInfoMap =<< M.elems (_siTables si)

-- | Contains Postgres connection configuration and essential metadata from the
-- database to build schema cache for tables and function.
data ResolvedSource b = ResolvedSource
  { _rsConfig :: !(SourceConfig b),
    _rsTables :: !(DBTablesMetadata b),
    _rsFunctions :: !(DBFunctionsMetadata b),
    _rsPgScalars :: !(HashSet (ScalarType b))
  }

type SourceTables b = HashMap SourceName (TableCache b)

type SourceResolver =
  SourceName -> PostgresConnConfiguration -> IO (Either QErr (SourceConfig ('Postgres 'Vanilla)))

class (Monad m) => MonadResolveSource m where
  getSourceResolver :: m SourceResolver

instance (MonadResolveSource m) => MonadResolveSource (ExceptT e m) where
  getSourceResolver = lift getSourceResolver

instance (MonadResolveSource m) => MonadResolveSource (ReaderT r m) where
  getSourceResolver = lift getSourceResolver

instance (MonadResolveSource m) => MonadResolveSource (Tracing.TraceT m) where
  getSourceResolver = lift getSourceResolver

instance (MonadResolveSource m) => MonadResolveSource (Q.TxET QErr m) where
  getSourceResolver = lift getSourceResolver

data MaintenanceModeVersion
  = -- | should correspond to the source catalog version from which the user
    -- is migrating from
    PreviousMMVersion
  | -- | should correspond to the latest source catalog version
    CurrentMMVersion
  deriving (Show, Eq)
