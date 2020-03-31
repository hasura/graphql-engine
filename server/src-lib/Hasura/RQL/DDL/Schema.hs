{-| This module (along with the various @Hasura.RQL.DDL.Schema.*@ modules) provides operations to
load and modify the Hasura catalog and schema cache.

* The /catalog/ refers to the set of PostgreSQL tables and views that store all schema information
    known by Hasura. This includes any tracked Postgres tables, views, and functions, all remote
    schemas, and any additionaly Hasura-specific information such as permissions and relationships.

    Primitive functions for loading and modifying the catalog are defined in
    "Hasura.RQL.DDL.Schema.Catalog", but most uses are wrapped by other functions to synchronize
    catalog information with the information in the schema cache.

* The /schema cache/ is a process-global value of type 'SchemaCache' that stores an in-memory
    representation of the data stored in the catalog. The in-memory representation is not identical
    to the data in the catalog, since it has some post-processing applied to it in order to make it
    easier to consume for other parts of the system, such as GraphQL schema generation. For example,
    although column information is represented by 'PGRawColumnInfo', the schema cache contains
    “processed” 'PGColumnInfo' values, instead.

    Ultimately, the catalog is the source of truth for all information contained in the schema
    cache, but to avoid rebuilding the entire schema cache on every change to the catalog, various
    functions incrementally update the cache when they modify the catalog.
-}

{-# LANGUAGE RecordWildCards #-}

module Hasura.RQL.DDL.Schema
 ( module Hasura.RQL.DDL.Schema.Cache
 , module Hasura.RQL.DDL.Schema.Catalog
 , module Hasura.RQL.DDL.Schema.Function
 , module Hasura.RQL.DDL.Schema.Rename
 , module Hasura.RQL.DDL.Schema.Table

 , RunSQL(..)
 , runRunSQL
 ) where

import           Hasura.Prelude

import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as TE
import qualified Database.PG.Query              as Q
import qualified Database.PostgreSQL.LibPQ      as PQ

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Language.Haskell.TH.Syntax     (Lift)

import           Hasura.EncJSON
import           Hasura.RQL.DDL.Schema.Cache
import           Hasura.RQL.DDL.Schema.Catalog
import           Hasura.RQL.DDL.Schema.Function
import           Hasura.RQL.DDL.Schema.Rename
import           Hasura.RQL.DDL.Schema.Table
import           Hasura.RQL.Instances           ()
import           Hasura.RQL.Types
import           Hasura.Server.Utils            (matchRegex)

data RunSQL
  = RunSQL
  { rSql                      :: Text
  , rCascade                  :: !Bool
  , rCheckMetadataConsistency :: !(Maybe Bool)
  , rTxAccessMode             :: !Q.TxAccess
  } deriving (Show, Eq, Lift)

instance FromJSON RunSQL where
  parseJSON = withObject "RunSQL" $ \o -> do
    rSql <- o .: "sql"
    rCascade <- o .:? "cascade" .!= False
    rCheckMetadataConsistency <- o .:? "check_metadata_consistency"
    isReadOnly <- o .:? "read_only" .!= False
    let rTxAccessMode = if isReadOnly then Q.ReadOnly else Q.ReadWrite
    pure RunSQL{..}

instance ToJSON RunSQL where
  toJSON RunSQL {..} =
    object
      [ "sql" .= rSql
      , "cascade" .= rCascade
      , "check_metadata_consistency" .= rCheckMetadataConsistency
      , "read_only" .=
        case rTxAccessMode of
          Q.ReadOnly  -> True
          Q.ReadWrite -> False
      ]

runRunSQL :: (MonadTx m, CacheRWM m, HasSQLGenCtx m) => RunSQL -> m EncJSON
runRunSQL RunSQL {..} = do
  metadataCheckNeeded <- case rTxAccessMode of
    Q.ReadOnly  -> pure False
    Q.ReadWrite -> onNothing rCheckMetadataConsistency $ containsMutationKeyword rSql
  bool (execRawSQL rSql) (withMetadataCheck rCascade $ execRawSQL rSql) metadataCheckNeeded
  where
    execRawSQL :: (MonadTx m) => Text -> m EncJSON
    execRawSQL =
      fmap (encJFromJValue @RunSQLRes) . liftTx . Q.multiQE rawSqlErrHandler . Q.fromText
      where
        rawSqlErrHandler txe =
          let e = err400 PostgresError "query execution failed"
           in e {qeInternal = Just $ toJSON txe}

    containsMutationKeyword :: QErrM m => T.Text -> m Bool
    containsMutationKeyword = either throwErr return . matchRegex regex False
      where
        throwErr s = throw500 $ "compiling regex failed: " <> T.pack s
        regex = "\\balter\\b|\\bdrop\\b|\\breplace\\b|\\bcreate function\\b|\\bcomment on\\b"

data RunSQLRes
  = RunSQLRes
  { rrResultType :: !Text
  , rrResult     :: !Value
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 2 snakeCase) ''RunSQLRes)

instance Q.FromRes RunSQLRes where
  fromRes (Q.ResultOkEmpty _) =
    return $ RunSQLRes "CommandOk" Null
  fromRes (Q.ResultOkData res) = do
    csvRows <- resToCSV res
    return $ RunSQLRes "TuplesOk" $ toJSON csvRows
    where
      resToCSV :: PQ.Result -> ExceptT T.Text IO [[Text]]
      resToCSV r =  do
        nr  <- liftIO $ PQ.ntuples r
        nc  <- liftIO $ PQ.nfields r

        hdr <- forM [0..pred nc] $ \ic -> do
          colNameBS <- liftIO $ PQ.fname r ic
          maybe (return "unknown") decodeBS colNameBS

        rows <- forM [0..pred nr] $ \ir ->
          forM [0..pred nc] $ \ic -> do
            cellValBS <- liftIO $ PQ.getvalue r ir ic
            maybe (return "NULL") decodeBS cellValBS

        return $ hdr:rows

      decodeBS = either (throwError . T.pack . show) return . TE.decodeUtf8'
