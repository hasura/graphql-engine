module Hasura.Backends.MySQL.Connection where


import           Data.Pool                   (createPool)
import qualified Data.Text                   as T
import           Database.MySQL.Base
import           Hasura.Backends.MySQL.Types
import           Hasura.Base.Error
import           Hasura.Prelude
import           Hasura.RQL.Types.Common


resolveSourceConfig :: (MonadIO m) =>
  SourceName -> ConnSourceConfig -> m (Either QErr SourceConfig)
resolveSourceConfig _name csc@ConnSourceConfig{..} =
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
          liftIO (createPool (connect connectInfo) close 1 (60 {-seconds-} * 60 {-minutes-}) 1)
