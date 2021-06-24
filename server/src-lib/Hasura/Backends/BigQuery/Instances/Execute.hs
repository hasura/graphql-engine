{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.BigQuery.Instances.Execute () where

import           Hasura.Prelude

import qualified Data.Aeson                                  as Aeson
import qualified Data.HashMap.Strict.InsOrd                  as OMap
import qualified Data.Text                                   as T
import qualified Data.Vector                                 as V
import qualified Hasura.Logging                              as L
import qualified Language.GraphQL.Draft.Syntax               as G

import qualified Hasura.Backends.BigQuery.DataLoader.Execute as DataLoader
import qualified Hasura.Backends.BigQuery.DataLoader.Plan    as DataLoader
import qualified Hasura.Backends.BigQuery.Types              as BigQuery
import qualified Hasura.Base.Error                           as E
import qualified Hasura.Tracing                              as Tracing

import           Hasura.Backends.BigQuery.Plan
import           Hasura.Base.Error
import           Hasura.EncJSON
import           Hasura.GraphQL.Execute.Backend
import           Hasura.GraphQL.Parser
import           Hasura.RQL.IR
import           Hasura.RQL.Types
import           Hasura.Server.Types                         (RequestId)
import           Hasura.Session


instance BackendExecute 'BigQuery where
  type MultiplexedQuery 'BigQuery = Void

  executeQueryField = bqDBQueryPlan
  executeMutationField = bqDBMutationPlan
  makeLiveQueryPlan _ _ _ _ =
    throwError $ E.internalError "Cannot currently perform subscriptions on BigQuery sources."
  explainQueryField = bqDBQueryExplain
  explainLiveQuery _ =
    throwError $ E.internalError "Cannot currently retrieve query execution plans on BigQuery sources."
  executeMultiplexedQuery = error "Not supported."
  executeRemoteRelationship = undefined

-- query

run :: (MonadIO m, MonadError QErr m) => ExceptT QErr IO a -> m a
run action = do
  result <- liftIO $ runExceptT action
  result `onLeft` throwError

bqDBQueryPlan
  :: forall m.
     ( MonadError E.QErr m
     , MonadIO m
     , Tracing.MonadTrace m
     )
  => RequestId
  -> L.Logger L.Hasura
  -> UserInfo
  -> SourceName
  -> SourceConfig 'BigQuery
  -> QueryDB 'BigQuery (Const Void) (UnpreparedValue 'BigQuery)
  -> m EncJSON
bqDBQueryPlan _requestId _logger userInfo _sourceName sourceConfig qrf = do
  select <- planNoPlan userInfo qrf
  let (!headAndTail, !plannedActionsList) =
        DataLoader.runPlan
          (DataLoader.planSelectHeadAndTail Nothing Nothing select)
      !actionsForest = DataLoader.actionsForest id plannedActionsList
  let action = do
        result <-
          DataLoader.runExecute
            sourceConfig
            headAndTail
            (DataLoader.execute actionsForest)
        case result of
          Left err        -> throw500WithDetail "dataLoader error" $ Aeson.toJSON $ show err
          Right recordSet -> pure $! recordSetToEncJSON (BigQuery.selectCardinality select) recordSet
  Tracing.interpTraceT run action

-- | Convert the dataloader's 'RecordSet' type to JSON.
recordSetToEncJSON :: BigQuery.Cardinality -> DataLoader.RecordSet -> EncJSON
recordSetToEncJSON cardinality DataLoader.RecordSet {rows} =
  case cardinality of
    BigQuery.One
      | Just row <- rows V.!? 0 -> encJFromRecord row
      | otherwise -> encJFromList (toList (fmap encJFromRecord rows))
    BigQuery.Many -> encJFromList (toList (fmap encJFromRecord rows))
  where
    encJFromRecord =
      encJFromInsOrdHashMap . fmap encJFromOutputValue . OMap.mapKeys coerce
    encJFromOutputValue outputValue =
      case outputValue of
        DataLoader.NullOutputValue -> encJFromJValue Aeson.Null
        DataLoader.DecimalOutputValue !i -> encJFromJValue i
        DataLoader.BigDecimalOutputValue !i -> encJFromJValue i
        DataLoader.FloatOutputValue !i -> encJFromJValue i
        DataLoader.TextOutputValue !i -> encJFromJValue i
        DataLoader.BytesOutputValue !i -> encJFromJValue i
        DataLoader.DateOutputValue !i -> encJFromJValue i
        DataLoader.TimestampOutputValue !i -> encJFromJValue i
        DataLoader.TimeOutputValue !i -> encJFromJValue i
        DataLoader.DatetimeOutputValue !i -> encJFromJValue i
        DataLoader.GeographyOutputValue !i -> encJFromJValue i
        DataLoader.BoolOutputValue !i -> encJFromJValue i
        DataLoader.IntegerOutputValue !i -> encJFromJValue i
        DataLoader.ArrayOutputValue !vector ->
          encJFromList (toList (fmap encJFromOutputValue vector))
        -- Really, the case below shouldn't be happening. But if it
        -- does, it's not a problem either. The output will just have
        -- a record in it.
        DataLoader.RecordOutputValue !record -> encJFromRecord record


-- mutation

bqDBMutationPlan
  :: forall m.
     ( MonadError E.QErr m
     )
  => RequestId
  -> L.Logger L.Hasura
  -> UserInfo
  -> Bool
  -> SourceName
  -> SourceConfig 'BigQuery
  -> MutationDB 'BigQuery (Const Void) (UnpreparedValue 'BigQuery)
  -> m EncJSON
bqDBMutationPlan _requestId _logger _userInfo _stringifyNum _sourceName _sourceConfig _mrf =
  throw500 "mutations are not supported in BigQuery; this should be unreachable"


-- explain

bqDBQueryExplain
  :: MonadError E.QErr m
  => G.Name
  -> UserInfo
  -> SourceName
  -> SourceConfig 'BigQuery
  -> QueryDB 'BigQuery (Const Void) (UnpreparedValue 'BigQuery)
  -> m EncJSON
bqDBQueryExplain fieldName userInfo _sourceName _sourceConfig qrf = do
  actionsForest <- planToForest userInfo qrf
  pure $ encJFromJValue $ ExplainPlan
    fieldName
    (Just $ DataLoader.drawActionsForestSQL actionsForest)
    (Just $ T.lines $ DataLoader.drawActionsForest actionsForest)
