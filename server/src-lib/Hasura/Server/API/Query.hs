-- | The RQL metadata query ('/v2/query')
module Hasura.Server.API.Query where

import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.DDL.Schema
import           Hasura.RQL.DML.Count
import           Hasura.RQL.DML.Delete
import           Hasura.RQL.DML.Insert
import           Hasura.RQL.DML.Select
import           Hasura.RQL.DML.Update
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Run
import           Hasura.Server.Version (HasVersion)
import           Hasura.Session
import           Hasura.SQL.Types

import qualified Hasura.Tracing        as Tracing

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH

import qualified Data.Environment      as Env
import qualified Data.HashMap.Strict   as M
import qualified Data.Text             as T
import qualified Database.PG.Query     as Q
import qualified Network.HTTP.Client   as HTTP

data RQLQuery
  = RQInsert !InsertQuery
  | RQSelect !SelectQuery
  | RQUpdate !UpdateQuery
  | RQDelete !DeleteQuery
  | RQCount  !CountQuery

  | RQRunSql !RunSQL
  | RQBulk ![RQLQuery]
  deriving (Show)

$(deriveJSON
  defaultOptions { constructorTagModifier = snakeCase . drop 2
                 , sumEncoding = TaggedObject "type" "args"
                 }
  ''RQLQuery)

queryNeedsAdmin :: RQLQuery -> Bool
queryNeedsAdmin = \case
  RQRunSql _ -> True
  RQBulk   l -> any queryNeedsAdmin l
  _          -> False

data QueryWithSource
  = QueryWithSource
    { _qwsSource :: !SourceName
    , _qwsQuery  :: !RQLQuery
    } deriving (Show)

instance FromJSON QueryWithSource where
  parseJSON = withObject "Object" $ \o -> do
    source <- o .:? "source" .!= defaultSource
    rqlQuery <- parseJSON $ Object o
    pure $ QueryWithSource source rqlQuery

instance ToJSON QueryWithSource where
  toJSON (QueryWithSource source rqlQuery) =
    case toJSON rqlQuery of
      Object o -> Object $ M.insert "source" (toJSON source) o
      -- never happens since JSON value of RQL queries are always objects
      _        -> error "Unexpected: toJSON of RQL queries are not objects"

runQuery
  :: ( HasVersion
     , MonadIO m
     , MonadError QErr m
     , Tracing.MonadTrace m
     )
  => Env.Environment
  -> UserInfo
  -> HTTP.Manager
  -> SQLGenCtx
  -> RebuildableSchemaCache
  -> Metadata
  -> QueryWithSource
  -> m (EncJSON, Maybe MetadataStateResult)
runQuery env userInfo httpManager sqlGenCtx schemaCache metadata request = do
  traceCtx <- Tracing.currentContext
  accessMode <- fromMaybe Q.ReadWrite <$> getQueryAccessMode rqlQuery
  let sc = lastBuiltSchemaCache schemaCache
  sourceConfig <- fmap _pcConfiguration $ onNothing (M.lookup source (scPostgres sc)) $
                  throw400 NotExists $ "source " <> source <<> " does not exist"
  runQueryM env source rqlQuery & Tracing.interpTraceT \x -> do
    (((r, tracemeta), rsc, ci), meta)
      <- x & runCacheRWT schemaCache (APIV2Query mempty)
           & peelQueryRun sourceConfig accessMode (Just traceCtx)
             (RunCtx userInfo httpManager sqlGenCtx) metadata
           & runExceptT
           & liftEitherM
    let metadataStateRes = MetadataStateResult rsc ci meta
    pure $ bool ((r, Nothing), tracemeta)
                ((r, Just metadataStateRes), tracemeta)
                $ queryModifiesSchema rqlQuery
  where
    QueryWithSource source rqlQuery = request
    queryModifiesSchema = \case
      RQRunSql q  -> isSchemaCacheBuildRequiredRunSQL q
      RQBulk l    -> any queryModifiesSchema l
      _           -> False

    getQueryAccessMode = \case
      RQRunSql RunSQL{_rTxAccessMode} -> pure $ Just _rTxAccessMode
      RQBulk   qs                     ->
        foldM reconcileAccessModeWith Nothing (zip [0 :: Integer ..] qs)
      _                               -> pure Nothing
      where
        reconcileAccessModeWith expectedMode (i, q) = do
          queryMode <- getQueryAccessMode q
          onLeft (reconcileAccessModes expectedMode queryMode) $ \errMode ->
            throw400 BadRequest $
            "incompatible access mode requirements in bulk query, " <>
            "expected access mode: " <>
            (T.pack $ maybe "ANY" show expectedMode) <>
            " but " <>
            "$.args[" <>
            (T.pack $ show i) <>
            "] forces " <>
            (T.pack $ show errMode)

        reconcileAccessModes l r = case (l, r) of
          (Nothing, r') -> Right r'
          (l', Nothing) -> Right l'
          (Just l', Just r') ->
            if | l' == r' -> Right $ Just l'
               | otherwise -> Left r'


runQueryM
  :: ( HasVersion
     , MonadIO m
     , UserInfoM m
     , CacheRWM m
     , MonadTx m
     , HasSQLGenCtx m
     , Tracing.MonadTrace m
     )
  => Env.Environment -> SourceName -> RQLQuery -> m EncJSON
runQueryM env source = \case
  RQInsert q -> runInsert env source q
  RQSelect q -> runSelect source q
  RQUpdate q -> runUpdate env source q
  RQDelete q -> runDelete env source q
  RQCount  q -> runCount source q
  RQRunSql q -> runRunSQL source q
  RQBulk   l -> encJFromList <$> indexedMapM (runQueryM env source) l
