{-# LANGUAGE ViewPatterns #-}

module Hasura.Backends.MSSQL.DDL.RunSQL
  ( runSQL
  , MSSQLRunSQL(..)
  , sqlContainsDDLKeyword
  )
where

import           Hasura.Prelude

import qualified Data.Aeson                       as J
import qualified Data.HashMap.Strict              as M
import qualified Data.HashSet                     as HS
import qualified Data.Text                        as T
import qualified Database.ODBC.Internal           as ODBC
import qualified Text.Regex.TDFA                  as TDFA

import           Data.Aeson.TH
import           Data.String                      (fromString)

import           Hasura.Backends.MSSQL.Connection
import           Hasura.Backends.MSSQL.Meta
import           Hasura.Base.Error
import           Hasura.EncJSON
import           Hasura.RQL.DDL.Schema
import           Hasura.RQL.DDL.Schema.Diff
import           Hasura.RQL.Types                 hiding (TableName, tmTable)
import           Hasura.Server.Utils              (quoteRegex)


odbcValueToJValue :: ODBC.Value -> J.Value
odbcValueToJValue = \case
  ODBC.TextValue t       -> J.String t
  ODBC.ByteStringValue b -> J.String $ bsToTxt b
  ODBC.BinaryValue b     -> J.String $ bsToTxt $ ODBC.unBinary b
  ODBC.BoolValue b       -> J.Bool b
  ODBC.DoubleValue d     -> J.toJSON d
  ODBC.FloatValue f      -> J.toJSON f
  ODBC.IntValue i        -> J.toJSON i
  ODBC.ByteValue b       -> J.toJSON b
  ODBC.DayValue d        -> J.toJSON d
  ODBC.TimeOfDayValue td -> J.toJSON td
  ODBC.LocalTimeValue l  -> J.toJSON l
  ODBC.NullValue         -> J.Null

data MSSQLRunSQL
  = MSSQLRunSQL
  { _mrsSql    :: Text
  , _mrsSource :: !SourceName
  } deriving (Show, Eq)
$(deriveJSON hasuraJSON ''MSSQLRunSQL)

runSQL
  :: (MonadIO m, CacheRWM m, MonadError QErr m, MetadataM m)
  => MSSQLRunSQL
  -> m EncJSON
runSQL (MSSQLRunSQL sqlText source) = do
  SourceInfo _ tableCache _ sourceConfig <- askSourceInfo @'MSSQL source
  let pool = _mscConnectionPool sourceConfig
  results <- if sqlContainsDDLKeyword sqlText then withMetadataCheck tableCache pool else runSQLQuery pool
  pure $ encJFromJValue $ toResult results
  where
    runSQLQuery pool = withMSSQLPool pool $ \conn ->
      ODBC.query conn $ fromString $ T.unpack sqlText

    toTableMeta dbTablesMeta = M.toList dbTablesMeta <&> \(table, dbTableMeta) ->
      TableMeta table dbTableMeta [] -- No computed fields

    withMetadataCheck tableCache pool = do
      -- If the SQL modifies the schema of the database then check for any metadata changes
      preActionTablesMeta <- toTableMeta <$> loadDBMetadata pool
      results <- runSQLQuery pool
      postActionTablesMeta <- toTableMeta <$> loadDBMetadata pool
      let trackedTablesMeta = filter (flip M.member tableCache . tmTable) preActionTablesMeta
          schemaDiff = getSchemaDiff trackedTablesMeta postActionTablesMeta
      metadataUpdater <- execWriterT $ processSchemaDiff source tableCache schemaDiff
      -- Build schema cache with updated metadata
      withNewInconsistentObjsCheck $
        buildSchemaCacheWithInvalidations mempty{ciSources = HS.singleton source} metadataUpdater
      pure results

sqlContainsDDLKeyword :: Text -> Bool
sqlContainsDDLKeyword = TDFA.match $$(quoteRegex
  TDFA.defaultCompOpt
    { TDFA.caseSensitive = False
    , TDFA.multiline = True
    , TDFA.lastStarGreedy = True }
    TDFA.defaultExecOpt
    { TDFA.captureGroups = False }
    "\\balter\\b|\\bdrop\\b|\\bsp_rename\\b")

toResult :: [[(ODBC.Column, ODBC.Value)]] -> RunSQLRes
toResult result = case result of
  []           -> RunSQLRes "CommandOk" J.Null
  (firstRow:_) -> RunSQLRes "TuplesOk" $ J.toJSON $ toHeader firstRow : toRows result
  where
    toRows   = map $ map $ odbcValueToJValue . snd
    toHeader = map $ J.String . ODBC.columnName . fst
