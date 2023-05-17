-- | This module (along with the various @Hasura.RQL.DDL.Schema.*@ modules) provides operations to
-- load and modify the Hasura catalog and schema cache.
--
-- * The /catalog/ refers to the set of PostgreSQL tables and views that store all schema information
--    known by Hasura. This includes any tracked Postgres tables, views, and functions, all remote
--    schemas, and any additionaly Hasura-specific information such as permissions and relationships.
--
--    Primitive functions for loading and modifying the catalog are defined in
--    "Hasura.RQL.DDL.Schema.Catalog", but most uses are wrapped by other functions to synchronize
--    catalog information with the information in the schema cache.
--
-- * The /schema cache/ is a process-global value of type 'SchemaCache' that stores an in-memory
--    representation of the data stored in the catalog. The in-memory representation is not identical
--    to the data in the catalog, since it has some post-processing applied to it in order to make it
--    easier to consume for other parts of the system, such as GraphQL schema generation. For example,
--    although column information is represented by 'RawColumnInfo', the schema cache contains
--    “processed” 'ColumnInfo' values, instead.
--
--    Ultimately, the catalog is the source of truth for all information contained in the schema
--    cache, but to avoid rebuilding the entire schema cache on every change to the catalog, various
--    functions incrementally update the cache when they modify the catalog.
module Hasura.RQL.DDL.Schema
  ( module M,
    RunSQLRes (..),
  )
where

import Data.Aeson
import Data.Text.Encoding qualified as TE
import Database.PG.Query qualified as PG
import Database.PostgreSQL.LibPQ qualified as PQ
import Hasura.Prelude
import Hasura.RQL.DDL.Schema.Cache as M
import Hasura.RQL.DDL.Schema.Catalog as M
import Hasura.RQL.DDL.Schema.Rename as M
import Hasura.Table.API as M

data RunSQLRes = RunSQLRes
  { rrResultType :: Text,
    rrResult :: Value
  }
  deriving (Show, Generic, Eq)

instance FromJSON RunSQLRes where
  parseJSON = genericParseJSON hasuraJSON

instance ToJSON RunSQLRes where
  toJSON = genericToJSON hasuraJSON
  toEncoding = genericToEncoding hasuraJSON

instance PG.FromRes RunSQLRes where
  fromRes (PG.ResultOkEmpty _) =
    return $ RunSQLRes "CommandOk" Null
  fromRes (PG.ResultOkData res) = do
    csvRows <- resToCSV res
    return $ RunSQLRes "TuplesOk" $ toJSON csvRows
    where
      resToCSV :: PQ.Result -> ExceptT Text IO [[Text]]
      resToCSV r = do
        nr <- liftIO $ PQ.ntuples r
        nc <- liftIO $ PQ.nfields r

        hdr <- forM [0 .. pred nc] $ \ic -> do
          colNameBS <- liftIO $ PQ.fname r ic
          maybe (return "unknown") decodeBS colNameBS

        rows <- forM [0 .. pred nr] $ \ir ->
          forM [0 .. pred nc] $ \ic -> do
            cellValBS <- liftIO $ PQ.getvalue r ir ic
            maybe (return "NULL") decodeBS cellValBS

        return $ hdr : rows

      decodeBS = either (throwError . tshow) return . TE.decodeUtf8'
