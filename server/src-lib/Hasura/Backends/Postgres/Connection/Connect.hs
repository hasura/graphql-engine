-- | Connect to a postgres db and run queries.
--   This module is meant for simple one-off checks against
--   a postgres database, such as health checks or version checks,
--   and not for normal work.
module Hasura.Backends.Postgres.Connection.Connect
  ( withPostgresDB,
  )
where

import Data.Environment (getEnvironment)
import Database.PG.Query qualified as PG
import Hasura.Backends.Postgres.Connection qualified as PG
import Hasura.Base.Error (QErr)
import Hasura.Prelude
import Hasura.RQL.Types.Common (resolveUrlConf)

-- | Connect to a postgres database and run a transaction.
withPostgresDB :: PG.PostgresConnConfiguration -> PG.TxET QErr IO a -> IO (Either QErr a)
withPostgresDB PG.PostgresConnConfiguration {..} tx = do
  generateMinimalPool _pccConnectionInfo >>= \case
    Left err ->
      -- Cannot able to intialise a pool due to a bad connection config.
      pure $ Left err
    Right pool -> runExceptT (PG.runTx' pool tx)
  where
    generateMinimalPool :: PG.PostgresSourceConnInfo -> IO (Either QErr PG.PGPool)
    generateMinimalPool PG.PostgresSourceConnInfo {..} = runExceptT do
      env <- lift getEnvironment
      urlText <- resolveUrlConf env _psciDatabaseUrl
      let connInfo = PG.ConnInfo 0 $ PG.CDDatabaseURI $ txtToBs urlText
          -- Create pool with only one connection
          connParams = PG.defaultConnParams {PG.cpConns = 1}
      liftIO $ PG.initPGPool connInfo connParams (\_ -> pure ())
