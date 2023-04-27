module Hasura.Backends.DataConnector.Adapter.RunSQL
  ( DataConnectorRunSQL (..),
    runSQL,
  )
where

import Data.Aeson qualified as J
import Data.HashMap.Strict qualified as HashMap
import Data.Text.Extended (ToTxt (..))
import Hasura.Backends.DataConnector.API (RawRequest (..))
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.Adapter.Types (DataConnectorName (), SourceConfig (..))
import Hasura.Base.Error (Code (DataConnectorError), QErr (qeInternal), QErrExtra (ExtraInternal), err400)
import Hasura.EncJSON (EncJSON, encJFromJValue)
import Hasura.Prelude
import Hasura.RQL.DDL.Schema (RunSQLRes (..))
import Hasura.RQL.Types.BackendType (BackendType (DataConnector))
import Hasura.RQL.Types.Common (SourceName (), sourceNameToText)
import Hasura.RQL.Types.SchemaCache (askSourceConfig)
import Hasura.RQL.Types.SchemaCache.Build (CacheRWM, MetadataM)
import Servant.Client (mkClientEnv, runClientM, (//))
import Servant.Client.Generic (genericClient)
import Witch qualified

data DataConnectorRunSQL = DataConnectorRunSQL
  { _dcSource :: SourceName,
    _dcSql :: Text
  }
  deriving (Show, Eq)

instance J.FromJSON DataConnectorRunSQL where
  parseJSON = J.withObject "DataConnectorRunSQL" $ \o -> do
    _dcSql <- o J..: "sql"
    _dcSource <- o J..: "source"
    do
      -- Throw errors on unsupported operations
      cascade <- o J..:? "cascade"
      when (cascade == Just True) do
        fail "Cascade not supported for raw data connector queries"
      readOnly <- o J..:? "read_only"
      when (readOnly == Just True) do
        fail "Read-only not supported for raw data connector queries"
    pure DataConnectorRunSQL {..}

instance J.ToJSON DataConnectorRunSQL where
  toJSON DataConnectorRunSQL {..} =
    J.object
      [ "sql" J..= _dcSql
      ]

-- TODO:
--
-- This is defined in the same manner as runSQL variants for other existing backends.
--
-- The pattern used here should be improved since:

-- * It is brittle: Not as type-safe as it could be

-- * It is slow: Doesn't reuse schema-cache

-- * It is verbose: Code duplication i.e. templates

-- * It is incorrect: Uses runClientM directly without tracing capabilities

--
-- The intent is to refactor all usage of raw sql queries rather than try to fix everything
-- in this PR.
--
runSQL ::
  forall m.
  (MonadIO m, CacheRWM m, MonadError QErr m, MetadataM m) =>
  DataConnectorName ->
  DataConnectorRunSQL ->
  m EncJSON
runSQL methodConnectorName DataConnectorRunSQL {..} = do
  SourceConfig {..} <- askSourceConfig @'DataConnector _dcSource

  -- There is no way to know if the source prefix matches the backend type until we have `SourceConfig` available.
  unless (_scDataConnectorName == methodConnectorName) do
    throwError
      ( err400
          DataConnectorError
          ( "run_sql query referencing connector type "
              <> Witch.from methodConnectorName
              <> " not supported on source "
              <> sourceNameToText _dcSource
              <> " for data connector of type "
              <> Witch.from _scDataConnectorName
          )
      )

  let clientEnv = mkClientEnv _scManager _scEndpoint
  let client = (genericClient // API._raw) (toTxt _dcSource) _scConfig (RawRequest _dcSql)

  resultsE <- liftIO $ runClientM client clientEnv

  case tupleRows <$> resultsE of
    Left e ->
      throwError
        (err400 DataConnectorError "Error performing raw query to data connector")
          { qeInternal = Just (ExtraInternal (J.String (tshow e)))
          }
    Right [] -> pure $ encJFromJValue $ RunSQLRes "CommandOk" J.Null
    Right results@(firstRow : _) ->
      let toRow = map snd
          toHeader = map $ J.String . fst
       in pure $ encJFromJValue $ RunSQLRes "TuplesOk" $ J.toJSON $ toHeader firstRow : map toRow results

tupleRows :: API.RawResponse -> [[(Text, J.Value)]]
tupleRows (API.RawResponse rs) = case rs of
  [] -> []
  xs@(x : _) ->
    let ks = HashMap.keys x
        lookupKeys m = (\k -> maybe [] (pure . (k,)) $ HashMap.lookup k m) =<< ks
     in map lookupKeys xs
