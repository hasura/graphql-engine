module Hasura.RQL.DDL.Schema.Source where

import           Hasura.Prelude

import qualified Data.Environment                    as Env
import qualified Data.HashMap.Strict                 as HM
import qualified Data.HashMap.Strict.InsOrd          as OMap
import qualified Database.PG.Query                   as Q

import           Control.Lens                        (at, (.~), (^.))
import           Control.Monad.Trans.Control         (MonadBaseControl)
import           Data.Text.Extended

import qualified Hasura.SQL.AnyBackend               as AB

import           Hasura.Backends.Postgres.Connection
import           Hasura.EncJSON
import           Hasura.RQL.DDL.Deps
import           Hasura.RQL.DDL.Schema.Common
import           Hasura.RQL.Types


mkPgSourceResolver :: Q.PGLogger -> SourceResolver
mkPgSourceResolver pgLogger _ config = runExceptT do
  env <- lift Env.getEnvironment
  let PostgresSourceConnInfo urlConf connSettings allowPrepare = _pccConnectionInfo config
  -- If the user does not provide values for the pool settings, then use the default values
  let (maxConns, idleTimeout, retries) = getDefaultPGPoolSettingIfNotExists connSettings defaultPostgresPoolSettings
  urlText <- resolveUrlConf env urlConf
  let connInfo = Q.ConnInfo retries $ Q.CDDatabaseURI $ txtToBs urlText
      connParams = Q.defaultConnParams{ Q.cpIdleTime = idleTimeout
                                      , Q.cpConns = maxConns
                                      , Q.cpAllowPrepare = allowPrepare
                                      }
  pgPool <- liftIO $ Q.initPGPool connInfo connParams pgLogger
  let pgExecCtx = mkPGExecCtx Q.ReadCommitted pgPool
  pure $ PGSourceConfig pgExecCtx connInfo Nothing

--- Metadata APIs related
runAddSource
  :: forall m b
   . (MonadError QErr m, CacheRWM m, MetadataM m, BackendMetadata b)
  => AddSource b -> m EncJSON
runAddSource (AddSource name sourceConfig replaceConfiguration) = do
  sources <- scSources <$> askSchemaCache
  let sourceMetadata = mkSourceMetadata @b name sourceConfig

  metadataModifier <- MetadataModifier <$>
    if HM.member name sources then
      if replaceConfiguration then pure $ metaSources.ix name .~ sourceMetadata
      else throw400 AlreadyExists $ "source with name " <> name <<> " already exists"
    else pure $ metaSources %~ OMap.insert name sourceMetadata

  buildSchemaCacheFor (MOSource name) metadataModifier
  pure successMsg

runDropSource
  :: forall m. (MonadError QErr m, CacheRWM m, MonadIO m, MonadBaseControl IO m, MetadataM m)
  => DropSource -> m EncJSON
runDropSource (DropSource name cascade) = do
  sc <- askSchemaCache
  let sources = scSources sc
  case HM.lookup name sources of
    Just backendSourceInfo ->
      AB.dispatchAnyBackend @BackendMetadata backendSourceInfo $ dropSource sc

    Nothing -> do
      metadata <- getMetadata
      void $ onNothing (metadata ^. metaSources . at name) $
          throw400 NotExists $ "source with name " <> name <<> " does not exist"
      if cascade
        then
          -- Without sourceInfo we can't cascade, so throw an error
          throw400 Unexpected $ "source with name " <> name <<> " is inconsistent"
        else
          -- Drop source from metadata
          buildSchemaCacheFor (MOSource name) dropSourceMetadataModifier
  pure successMsg
  where
    dropSource :: forall b. (BackendMetadata b) => SchemaCache -> SourceInfo b -> m ()
    dropSource sc sourceInfo = do
      let sourceConfig = _siConfiguration sourceInfo
      let indirectDeps = mapMaybe getIndirectDep $
                         getDependentObjs sc (SOSource name)

      when (not cascade && indirectDeps /= [])
        $ reportDepsExt
            (map (SOSourceObj name . AB.mkAnyBackend) indirectDeps)
            []

      metadataModifier <- execWriterT $ do
        mapM_ (purgeDependentObject name >=> tell) indirectDeps
        tell dropSourceMetadataModifier

      buildSchemaCacheFor (MOSource name) metadataModifier
      postDropSourceHook sourceConfig
      where
        getIndirectDep :: SchemaObjId -> Maybe (SourceObjId b)
        getIndirectDep = \case
          SOSourceObj s o ->
            if s == name
              then Nothing
              -- consider only *this* backend specific dependencies
              else AB.unpackAnyBackend o
          _               -> Nothing

    dropSourceMetadataModifier = MetadataModifier $ metaSources %~ OMap.delete name
