module Hasura.Backends.MySQL.Connection where


import           Data.Pool                   (createPool, withResource)
import qualified Data.Text                   as T
import           Database.MySQL.Base
import           Hasura.Backends.MySQL.Meta  (getMetadata)
import           Hasura.Backends.MySQL.Types
import           Hasura.Base.Error
import           Hasura.Prelude
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.Source
import           Hasura.SQL.Backend


resolveSourceConfig :: (MonadIO m) =>
  SourceName -> ConnSourceConfig -> m (Either QErr SourceConfig)
resolveSourceConfig _name csc@ConnSourceConfig{_cscPoolSettings = ConnPoolSettings{..}, ..} =
  let connectInfo =
        defaultConnectInfo
          { connectHost = T.unpack _cscHost
          , connectPort = _cscPort
          , connectUser = T.unpack _cscUser
          , connectPassword = T.unpack _cscPassword
          , connectDatabase = T.unpack  _cscDatabase
          }
   in runExceptT $
        SourceConfig csc <$>
          liftIO
            (createPool
              (connect connectInfo)
              close
              1
              (fromIntegral _cscIdleTimeout)
              (fromIntegral _cscMaxConnections))


resolveDatabaseMetadata :: (MonadIO m) =>
  SourceConfig ->
  m (Either QErr (ResolvedSource 'MySQL))
resolveDatabaseMetadata sc@SourceConfig{..} =
  runExceptT $ do
    metadata <- liftIO $ withResource scConnectionPool (getMetadata scConfig)
    pure $ ResolvedSource sc metadata mempty mempty
