{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.BigQuery.Instances.Execute () where

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict.InsOrd as OMap
import qualified Hasura.Backends.BigQuery.DataLoader.Execute as DataLoader
import qualified Hasura.Backends.BigQuery.DataLoader.Plan as DataLoader
import           Hasura.EncJSON
import           Hasura.Prelude
import qualified Hasura.RQL.Types.Error as RQL
import qualified Hasura.SQL.AnyBackend as AB

import qualified Data.Environment as Env
import qualified Language.GraphQL.Draft.Syntax as G
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP

import qualified Hasura.Tracing as Tracing

import           Hasura.Backends.BigQuery.Plan
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Execute.Backend
import           Hasura.GraphQL.Parser
import           Hasura.RQL.Types
import           Hasura.Session

-- MultiplexedQuery
instance BackendExecute 'BigQuery where
  type PreparedQuery  'BigQuery = Text
  type ExecutionMonad 'BigQuery = Tracing.TraceT (ExceptT QErr IO)
  type MultiplexedQuery 'BigQuery = Void
  getRemoteJoins = const []

  mkDBQueryPlan = msDBQueryPlan
  mkDBMutationPlan = msDBMutationPlan
  mkDBSubscriptionPlan _ _ _ _ =
    throwError $ RQL.internalError "Cannot currently perform subscriptions on BigQuery sources."
  mkDBQueryExplain _ _ _ _ _ = throwError $ RQL.internalError "Cannot currently retrieve query execution plans on BigQuery sources."
  mkLiveQueryExplain _ = throwError $ RQL.internalError "Cannot currently retrieve query execution plans on BigQuery sources."

-- query

msDBQueryPlan
  :: forall m.
     ( MonadError QErr m
     )
  => Env.Environment
  -> HTTP.Manager
  -> [HTTP.Header]
  -> UserInfo
  -> [G.Directive G.Name]
  -> SourceName
  -> SourceConfig 'BigQuery
  -> QueryDB 'BigQuery (UnpreparedValue 'BigQuery)
  -> m ExecutionStep
msDBQueryPlan _env _manager _reqHeaders userInfo _directives sourceName sourceConfig qrf = do
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
          Left err -> throw500WithDetail "dataLoader error" $ Aeson.toJSON $ show err
          Right recordSet -> pure $! recordSetToEncJSON recordSet
  pure
    $ ExecStepDB []
    . AB.mkAnyBackend
    $ DBStepInfo sourceName sourceConfig (Just (DataLoader.drawActionsForest actionsForest)) action

-- | Convert the dataloader's 'RecordSet' type to JSON.
recordSetToEncJSON :: DataLoader.RecordSet -> EncJSON
recordSetToEncJSON DataLoader.RecordSet {rows} =
  encJFromList (toList (fmap encJFromRecord rows))
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

msDBMutationPlan
  :: forall m.
     ( MonadError QErr m
     )
  => Env.Environment
  -> HTTP.Manager
  -> [HTTP.Header]
  -> UserInfo
  -> Bool
  -> SourceName
  -> SourceConfig 'BigQuery
  -> MutationDB 'BigQuery (UnpreparedValue 'BigQuery)
  -> m ExecutionStep
msDBMutationPlan _env _manager _reqHeaders _userInfo _stringifyNum _sourceName _sourceConfig _mrf =
  throw500 "mutations are not supported in BigQuery; this should be unreachable"
