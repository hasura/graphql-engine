module Hasura.Backends.MySQL.Connection
  ( runJSONPathQuery
  , resolveSourceConfig
  , resolveDatabaseMetadata
  , fetchAllRows
  )
where


import           Data.Aeson                    hiding (Result)
import qualified Data.Aeson                    as J
import           Data.Aeson.Text               (encodeToTextBuilder)
import           Data.ByteString               (ByteString)
import qualified Data.Environment              as Env
import qualified Data.HashMap.Strict           as HM
import           Data.Pool
import           Data.Scientific               (fromFloatDigits)
import qualified Data.Text                     as T
import           Data.Text.Encoding            (decodeUtf8)
import           Data.Text.Lazy                (toStrict)
import           Data.Text.Lazy.Builder        (toLazyText)
import           Database.MySQL.Base
import           Database.MySQL.Base.Types     (Field (..))
import qualified Database.MySQL.Simple.Result  as MySQL
import           Hasura.Backends.MySQL.Meta    (getMetadata)
import           Hasura.Backends.MySQL.ToQuery (Query (..))
import           Hasura.Backends.MySQL.Types
import           Hasura.Base.Error
import           Hasura.Prelude
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.Source
import           Hasura.SQL.Backend


resolveSourceConfig :: (MonadIO m) => SourceName -> ConnSourceConfig -> Env.Environment -> m (Either QErr SourceConfig)
resolveSourceConfig _name csc@ConnSourceConfig{_cscPoolSettings = ConnPoolSettings{..}, ..} _env = do
  let connectInfo =
        defaultConnectInfo
          { connectHost = T.unpack _cscHost
          , connectPort = _cscPort
          , connectUser = T.unpack _cscUser
          , connectPassword = T.unpack _cscPassword
          , connectDatabase = T.unpack  _cscDatabase
          }
  runExceptT $
    SourceConfig csc <$>
      liftIO
        (createPool
          (connect connectInfo)
          close
          1
          (fromIntegral _cscIdleTimeout)
          (fromIntegral _cscMaxConnections))



resolveDatabaseMetadata :: (MonadIO m) => SourceConfig -> m (Either QErr (ResolvedSource 'MySQL))
resolveDatabaseMetadata sc@SourceConfig{..} =
  runExceptT $ do
    metadata <- liftIO $ withResource scConnectionPool (getMetadata scConfig)
    pure $ ResolvedSource sc metadata mempty mempty


parseFieldResult :: Field -> Maybe ByteString -> Value
parseFieldResult f@Field{..} mBs =
  case fieldType of
    Long ->
      let fvalue :: Double = MySQL.convert f mBs
      in  Number $ fromFloatDigits fvalue
    VarString ->
      let fvalue :: Text = MySQL.convert f mBs
      in  J.String fvalue
    DateTime -> maybe J.Null (J.String . decodeUtf8) mBs
    _ -> error $ "parseResult: not implemented yet "  <> show f <> " " <> show mBs
    -- TODO: handle remaining cases


fieldsToAeson :: [Field] -> [[Maybe ByteString]] -> [Value]
fieldsToAeson column rows =
    [ Object $ HM.fromList $
        [ (decodeUtf8 (fieldName c)) .= (parseFieldResult c r)
        | (c, r) <- (zip column row :: [(Field, Maybe ByteString)]) ]
    | row <- (rows :: [[Maybe ByteString]]) ]


runJSONPathQuery :: (MonadError QErr m, MonadIO m)
  => (Pool Connection)
  -> Query
  -> m Text
runJSONPathQuery pool (Query querySql) = do
  result <- liftIO $
    withResource pool $ \conn -> do
      query conn querySql
      result <- storeResult conn
      fields <- fetchFields result
      rows <- fetchAllRows result
      pure $ fieldsToAeson fields rows
  pure $ toStrict $ toLazyText $ encodeToTextBuilder $ toJSON result


fetchAllRows :: Result -> IO [[Maybe ByteString]]
fetchAllRows r = reverse <$> go [] r
  where
    go acc res =
      fetchRow res >>= \case
        [] -> pure acc
        r' -> go (r' : acc) res
