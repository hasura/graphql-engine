module Hasura.Backends.MySQL.SQL
  ( runSQL
  , RunSQL (..)
  )
where

import qualified Data.Aeson                       as J
import           Data.Aeson.TH
import           Data.ByteString                  hiding (null, reverse)
import           Data.Pool                        (withResource)
import           Data.String                      (fromString)
import qualified Data.Text                        as T
import           Data.Text.Encoding               (decodeUtf8With)
import           Data.Text.Encoding.Error         (lenientDecode)
import           Database.MySQL.Base              (fetchFields, query, storeResult)
import           Database.MySQL.Base.Types        (Field (fieldName))
import           Hasura.Backends.MySQL.Connection (fetchAllRows)
import           Hasura.Backends.MySQL.Types      (SourceConfig (..))
import           Hasura.Base.Error
import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.DDL.Schema            (RunSQLRes (..))
import           Hasura.RQL.Types


data RunSQL
  = RunSQL
    { _Sql    :: !Text
    , _Source :: !SourceName
    } deriving (Show, Eq)

$(deriveJSON hasuraJSON ''RunSQL)


runSQL :: (MonadIO m, CacheRWM m, MonadError QErr m, MetadataM m) => RunSQL -> m EncJSON
runSQL (RunSQL sql source) = do
  pool <- scConnectionPool <$> askSourceConfig @'MySQL source
  result :: [[Maybe ByteString]] <- liftIO $ withResource pool $ \conn -> do
    query conn (fromString . T.unpack $ sql)
    r <- storeResult conn
    fieldNames <- fmap (Just . fieldName) <$> fetchFields r -- fieldNames as Maybes for convenience
    rows <- fetchAllRows r
    pure (fieldNames:rows)
  pure . encJFromJValue $
    if null result
      then RunSQLRes "CommandOK" J.Null
      else RunSQLRes "TuplesOk" . J.toJSON . (fmap . fmap . fmap) (decodeUtf8With lenientDecode) $ result


