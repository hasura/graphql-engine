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
import qualified Text.Regex.TDFA                as TDFA

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
import           Hasura.Server.Utils            (quoteRegex)

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
  -- see Note [Checking metadata consistency in run_sql]
  let metadataCheckNeeded = case rTxAccessMode of
        Q.ReadOnly  -> False
        Q.ReadWrite -> fromMaybe (containsDDLKeyword rSql) rCheckMetadataConsistency
  if metadataCheckNeeded
    then withMetadataCheck rCascade $ execRawSQL rSql
    else execRawSQL rSql
  where
    execRawSQL :: (MonadTx m) => Text -> m EncJSON
    execRawSQL =
      fmap (encJFromJValue @RunSQLRes) . liftTx . Q.multiQE rawSqlErrHandler . Q.fromText
      where
        rawSqlErrHandler txe =
          (err400 PostgresError "query execution failed") { qeExtra = Just $ EEInternal $ toJSON txe }

    -- see Note [Checking metadata consistency in run_sql]
    containsDDLKeyword :: Text -> Bool
    containsDDLKeyword = TDFA.match $$(quoteRegex
      TDFA.defaultCompOpt
        { TDFA.caseSensitive = False
        , TDFA.multiline = True
        , TDFA.lastStarGreedy = True }
      TDFA.defaultExecOpt
        { TDFA.captureGroups = False }
      "\\balter\\b|\\bdrop\\b|\\breplace\\b|\\bcreate function\\b|\\bcomment on\\b")

{- Note [Checking metadata consistency in run_sql]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SQL queries executed by run_sql may change the Postgres schema in arbitrary
ways. We attempt to automatically update the metadata to reflect those changes
as much as possible---for example, if a table is renamed, we want to update the
metadata to track the table under its new name instead of its old one. This
schema diffing (plus some integrity checking) is handled by withMetadataCheck.

But this process has overhead---it involves reloading the metadata, diffing it,
and rebuilding the schema cache---so we don’t want to do it if it isn’t
necessary. The user can explicitly disable the check via the
check_metadata_consistency option, and we also skip it if the current
transaction is in READ ONLY mode, since the schema can’t be modified in that
case, anyway.

However, even if neither read_only or check_metadata_consistency is passed, lots
of queries may not modify the schema at all. As a (fairly stupid) heuristic, we
check if the query contains any keywords for DDL operations, and if not, we skip
the metadata check as well. -}

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
