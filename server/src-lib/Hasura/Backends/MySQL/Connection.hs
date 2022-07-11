module Hasura.Backends.MySQL.Connection
  ( runJSONPathQuery,
    resolveSourceConfig,
    resolveDatabaseMetadata,
    postDropSourceHook,
    fetchAllRows,
    runQueryYieldingRows,
    withMySQLPool,
    parseTextRows,
  )
where

import Data.Aeson hiding (Result)
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Text (encodeToTextBuilder)
import Data.ByteString (ByteString)
import Data.Environment qualified as Env
import Data.HashMap.Strict.InsOrd qualified as OMap
import Data.Pool
import Data.Scientific (fromFloatDigits)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Database.MySQL.Base
import Database.MySQL.Base.Types (Field (..))
import Database.MySQL.Simple.Result qualified as MySQL
import Hasura.Backends.MySQL.DataLoader.Plan qualified as DataLoaderPlan
import Hasura.Backends.MySQL.Meta (getMetadata)
import Hasura.Backends.MySQL.ToQuery (Query (..))
import Hasura.Backends.MySQL.Types
import Hasura.Base.Error
import Hasura.Logging (Hasura, Logger)
import Hasura.Prelude
import Hasura.RQL.Types.Backend (BackendConfig)
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.SourceCustomization
import Hasura.SQL.Backend

resolveSourceConfig :: (MonadIO m) => Logger Hasura -> SourceName -> ConnSourceConfig -> BackendSourceKind 'MySQL -> BackendConfig 'MySQL -> Env.Environment -> m (Either QErr SourceConfig)
resolveSourceConfig _logger _name csc@ConnSourceConfig {_cscPoolSettings = ConnPoolSettings {..}, ..} _backendKind _backendConfig _env = do
  let connectInfo =
        defaultConnectInfo
          { connectHost = T.unpack _cscHost,
            connectPort = _cscPort,
            connectUser = T.unpack _cscUser,
            connectPassword = T.unpack _cscPassword,
            connectDatabase = T.unpack _cscDatabase
          }
  runExceptT $
    SourceConfig csc
      <$> liftIO
        ( createPool
            (connect connectInfo)
            close
            1
            (fromIntegral _cscIdleTimeout)
            (fromIntegral _cscMaxConnections)
        )

resolveDatabaseMetadata :: (MonadIO m) => SourceConfig -> SourceTypeCustomization -> m (Either QErr (ResolvedSource 'MySQL))
resolveDatabaseMetadata sc@SourceConfig {..} sourceCustomization =
  runExceptT $ do
    metadata <- liftIO $ withResource scConnectionPool (getMetadata scConfig)
    pure $ ResolvedSource sc sourceCustomization metadata mempty mempty

postDropSourceHook ::
  (MonadIO m) =>
  SourceConfig ->
  m ()
postDropSourceHook _ =
  -- As of now, we do not add any Hasura related stuff to source DB hence
  -- no need to clean things up.
  pure ()

parseFieldResult :: Field -> Maybe ByteString -> Value
parseFieldResult f@Field {..} mBs =
  case fieldType of
    Long ->
      let fvalue :: Double = MySQL.convert f mBs
       in Number $ fromFloatDigits fvalue
    VarString ->
      let fvalue :: Text = MySQL.convert f mBs
       in J.String fvalue
    Blob ->
      let fvalue :: Text = MySQL.convert f mBs
       in J.String fvalue
    DateTime -> maybe J.Null (J.String . decodeUtf8) mBs
    _ -> error $ "parseResult: not implemented yet " <> show f <> " " <> show mBs

-- TODO: handle remaining cases

fieldsToAeson :: [Field] -> [[Maybe ByteString]] -> [Value]
fieldsToAeson column rows =
  [ Object $
      KM.fromList $
        [ (K.fromText (decodeUtf8 (fieldName c))) .= (parseFieldResult c r)
          | (c, r) <- (zip column row :: [(Field, Maybe ByteString)])
        ]
    | row <- (rows :: [[Maybe ByteString]])
  ]

runJSONPathQuery ::
  (MonadError QErr m, MonadIO m) =>
  (Pool Connection) ->
  Query ->
  m Text
runJSONPathQuery pool (Query querySql) = do
  result <- liftIO $
    withResource pool $ \conn -> do
      query conn querySql
      result <- storeResult conn
      fields <- fetchFields result
      rows <- fetchAllRows result
      pure $ fieldsToAeson fields rows
  pure $ toStrict $ toLazyText $ encodeToTextBuilder $ toJSON result

-- | Used by the dataloader to produce rows of records. Those rows of
-- records are then manipulated by the dataloader to do Haskell-side
-- joins. Is a Vector of HashMaps the most efficient choice? A
-- pandas-style data frame could also be more efficient,
-- dependingly. However, this is a legible approach; efficiency
-- improvements can be added later.
parseAndCollectRows ::
  [Field] ->
  [[Maybe ByteString]] ->
  Vector (InsOrdHashMap DataLoaderPlan.FieldName J.Value)
parseAndCollectRows columns rows =
  V.fromList
    [ OMap.fromList
        [ (DataLoaderPlan.FieldName . decodeUtf8 . fieldName $ column, parseFieldResult column value)
          | (column, value) <- zip columns row :: [(Field, Maybe ByteString)]
        ]
      | row <- rows :: [[Maybe ByteString]]
    ]

-- | Run a query immediately and parse up the results into a vector.
runQueryYieldingRows ::
  (MonadIO m) =>
  Pool Connection ->
  Query ->
  m (Vector (InsOrdHashMap DataLoaderPlan.FieldName J.Value))
runQueryYieldingRows pool (Query querySql) = do
  liftIO $
    withResource pool $ \conn -> do
      query conn querySql
      result <- storeResult conn
      fields <- fetchFields result
      rows <- fetchAllRows result
      pure (parseAndCollectRows fields rows)

fetchAllRows :: Result -> IO [[Maybe ByteString]]
fetchAllRows r = reverse <$> go [] r
  where
    go acc res =
      fetchRow res >>= \case
        [] -> pure acc
        r' -> go (r' : acc) res

parseTextRows :: [Field] -> [[Maybe ByteString]] -> [[Text]]
parseTextRows columns rows = zipWith (\column row -> map (MySQL.convert column) row) columns rows

withMySQLPool :: (MonadIO m) => Pool Connection -> (Connection -> IO a) -> m a
withMySQLPool pool = liftIO . withResource pool
