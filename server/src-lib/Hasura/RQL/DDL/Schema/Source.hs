module Hasura.RQL.DDL.Schema.Source where

import           Control.Monad.Trans.Control         (MonadBaseControl)
import           Data.Text.Extended
import           Data.Typeable                       (cast)

import           Hasura.Backends.Postgres.Connection
import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.DDL.Deps
import           Hasura.RQL.DDL.Schema.Common
import           Hasura.RQL.Types

import qualified Data.Environment                    as Env
import qualified Data.HashMap.Strict                 as HM
import qualified Data.HashMap.Strict.InsOrd          as OMap
import qualified Database.PG.Query                   as Q

mkPgSourceResolver :: Q.PGLogger -> SourceResolver
mkPgSourceResolver pgLogger _ config = runExceptT do
  env <- lift Env.getEnvironment
  let PostgresSourceConnInfo urlConf connSettings = _pccConnectionInfo config
      PostgresPoolSettings maxConns idleTimeout retries = connSettings
  urlText <- resolveUrlConf env urlConf
  let connInfo = Q.ConnInfo retries $ Q.CDDatabaseURI $ txtToBs urlText
      connParams = Q.defaultConnParams{ Q.cpIdleTime = idleTimeout
                                      , Q.cpConns = maxConns
                                      }
  pgPool <- liftIO $ Q.initPGPool connInfo connParams pgLogger
  let pgExecCtx = mkPGExecCtx Q.ReadCommitted pgPool
  pure $ PGSourceConfig pgExecCtx connInfo Nothing

--- Metadata APIs related
runAddSource
  :: forall m b
   . (MonadError QErr m, CacheRWM m, MetadataM m, BackendMetadata b)
  => AddSource b -> m EncJSON
runAddSource (AddSource name sourceConfig) = do
  sources <- scSources <$> askSchemaCache
  onJust (HM.lookup name sources) $ const $
    throw400 AlreadyExists $ "postgres source with name " <> name <<> " already exists"
  buildSchemaCacheFor (MOSource name)
    $ MetadataModifier
    $ metaSources %~ OMap.insert name (mkSourceMetadata @b name sourceConfig)
  pure successMsg

runDropSource
  :: forall m. (MonadError QErr m, CacheRWM m, MonadIO m, MonadBaseControl IO m, MetadataM m)
  => DropSource -> m EncJSON
runDropSource (DropSource name cascade) = do
  sc <- askSchemaCache
  let sources = scSources sc
  backendSourceInfo <- onNothing (HM.lookup name sources) $
    throw400 NotExists $ "source with name " <> name <<> " does not exist"
  dropSource' sc backendSourceInfo
  pure successMsg
  where
    dropSource' :: SchemaCache -> BackendSourceInfo -> m ()
    dropSource' sc (BackendSourceInfo (sourceInfo :: SourceInfo b)) =
      case backendTag @b of
        PostgresTag -> dropSource sc (sourceInfo :: SourceInfo 'Postgres)
        MSSQLTag    -> dropSource sc (sourceInfo :: SourceInfo 'MSSQL)

    dropSource :: forall b. (BackendMetadata b) => SchemaCache -> SourceInfo b -> m ()
    dropSource sc sourceInfo = do
      let sourceConfig = _siConfiguration sourceInfo
      let indirectDeps = mapMaybe getIndirectDep $
                         getDependentObjs sc (SOSource name)

      when (not cascade && indirectDeps /= []) $ reportDepsExt (map (SOSourceObj name) indirectDeps) []

      metadataModifier <- execWriterT $ do
        mapM_ (purgeDependentObject name >=> tell) indirectDeps
        tell $ MetadataModifier $ metaSources %~ OMap.delete name

      buildSchemaCacheFor (MOSource name) metadataModifier
      postDropSourceHook sourceConfig
      where
        getIndirectDep :: SchemaObjId -> Maybe (SourceObjId b)
        getIndirectDep = \case
          SOSourceObj s o -> if s == name then Nothing else cast o -- consider only *this* backend specific dependencies
          _               -> Nothing
