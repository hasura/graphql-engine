{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.DataWrapper.Adapter.Transport () where

--------------------------------------------------------------------------------

import Control.Exception.Safe (throwIO)
import Data.Aeson qualified as J
import Data.Text.Extended ((<>>))
import Hasura.Backends.DataWrapper.Adapter.Execute ()
import Hasura.Backends.DataWrapper.Adapter.Types (SourceConfig)
import Hasura.Backends.DataWrapper.Plan qualified as GDW
import Hasura.Base.Error (Code (NotSupported), QErr, throw400)
import Hasura.EncJSON (EncJSON)
import Hasura.GraphQL.Execute.Backend (DBStepInfo (..))
import Hasura.GraphQL.Logging qualified as HGL
import Hasura.GraphQL.Namespace (RootFieldAlias)
import Hasura.GraphQL.Transport.Backend (BackendTransport (..))
import Hasura.GraphQL.Transport.HTTP.Protocol (GQLReqUnparsed)
import Hasura.Logging (Hasura, Logger)
import Hasura.Prelude
import Hasura.SQL.Backend (BackendType (DataWrapper))
import Hasura.Server.Types (RequestId)
import Hasura.Session (UserInfo)
import Hasura.Tracing qualified as Tracing

--------------------------------------------------------------------------------

instance BackendTransport 'DataWrapper where
  runDBQuery = runDBQuery'
  runDBQueryExplain = runDBQueryExplain'
  runDBMutation _ _ _ _ _ _ _ _ =
    throw400 NotSupported "runDBMutation: not implemented for GraphQL Data Wrappers."
  runDBStreamingSubscription _ _ _ =
    liftIO . throwIO $ userError "runDBStreamingSubscription: not implemented for GraphQL Data Wrappers."
  runDBSubscription _ _ _ =
    liftIO . throwIO $ userError "runDBSubscription: not implemented for GraphQL Data Wrappers."

runDBQuery' ::
  ( MonadIO m,
    MonadError QErr m,
    Tracing.MonadTrace m,
    HGL.MonadQueryLog m
  ) =>
  RequestId ->
  GQLReqUnparsed ->
  RootFieldAlias ->
  UserInfo ->
  Logger Hasura ->
  SourceConfig ->
  Tracing.TraceT (ExceptT QErr IO) a ->
  Maybe GDW.Plan ->
  m (DiffTime, a)
runDBQuery' requestId query fieldName _userInfo logger _sourceConfig action ir = do
  void $ HGL.logQueryLog logger $ mkQueryLog query fieldName ir requestId
  withElapsedTime
    . Tracing.trace ("Dynamic backend query for root field " <>> fieldName)
    . Tracing.interpTraceT (liftEitherM . liftIO . runExceptT)
    $ action

mkQueryLog ::
  GQLReqUnparsed ->
  RootFieldAlias ->
  Maybe GDW.Plan ->
  RequestId ->
  HGL.QueryLog
mkQueryLog gqlQuery fieldName maybePlan requestId =
  HGL.QueryLog
    gqlQuery
    ((\plan -> (fieldName, HGL.GeneratedQuery (GDW.renderPlan plan) J.Null)) <$> maybePlan)
    requestId
    HGL.QueryLogKindDatabase

runDBQueryExplain' ::
  (MonadIO m, MonadError QErr m) =>
  DBStepInfo 'DataWrapper ->
  m EncJSON
runDBQueryExplain' (DBStepInfo _ _ _ action) =
  liftEitherM $
    liftIO $
      runExceptT $
        Tracing.runTraceTWithReporter Tracing.noReporter "explain" $
          action
