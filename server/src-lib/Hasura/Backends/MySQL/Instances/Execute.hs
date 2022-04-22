{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.MySQL.Instances.Execute () where

import Data.Aeson as J
import Data.Bifunctor
import Data.Coerce
import Data.HashMap.Strict.InsOrd qualified as OMap
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Tree
import Database.MySQL.Base (fetchFields, query, storeResult)
import Hasura.Backends.MySQL.Connection
import Hasura.Backends.MySQL.DataLoader.Execute (OutputValue (..), RecordSet (..))
import Hasura.Backends.MySQL.DataLoader.Execute qualified as DataLoader
import Hasura.Backends.MySQL.DataLoader.Plan qualified as DataLoader
import Hasura.Backends.MySQL.Plan
import Hasura.Backends.MySQL.ToQuery as ToQuery
import Hasura.Backends.MySQL.Types qualified as MySQL
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.GraphQL.Execute.Backend
import Hasura.GraphQL.Namespace
import Hasura.GraphQL.Parser
import Hasura.Prelude hiding (first, second)
import Hasura.RQL.IR
import Hasura.RQL.Types
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Session
import Hasura.Tracing qualified as Tracing

instance BackendExecute 'MySQL where
  type PreparedQuery 'MySQL = Text
  type MultiplexedQuery 'MySQL = Void
  type ExecutionMonad 'MySQL = Tracing.TraceT (ExceptT QErr IO)
  mkDBQueryPlan = mysqlDBQueryPlan
  mkDBMutationPlan = error "mkDBMutationPlan: MySQL backend does not support this operation yet."
  mkLiveQuerySubscriptionPlan _ _ _ _ = error "mkLiveQuerySubscriptionPlan: MySQL backend does not support this operation yet."
  mkDBStreamingSubscriptionPlan _ _ _ _ = error "mkDBStreamingSubscriptionPlan: MySQL backend does not support this operation yet."
  mkDBQueryExplain = mysqlDBQueryExplain
  mkSubscriptionExplain _ = error "mkSubscriptionExplain: MySQL backend does not support this operation yet."
  mkDBRemoteRelationshipPlan = error "mkDBRemoteRelationshipPlan: MySQL does not support this operation yet."

mysqlDBQueryPlan ::
  forall m.
  ( MonadError QErr m
  ) =>
  UserInfo ->
  SourceName ->
  SourceConfig 'MySQL ->
  QueryDB 'MySQL Void (UnpreparedValue 'MySQL) ->
  m (DBStepInfo 'MySQL)
mysqlDBQueryPlan userInfo sourceName sourceConfig qrf = do
  (headAndTail, actionsForest) <- queryToActionForest userInfo qrf
  pure
    ( DBStepInfo
        @'MySQL
        sourceName
        sourceConfig
        (Just (T.pack (drawForest (fmap (fmap show) actionsForest))))
        ( do
            result <-
              DataLoader.runExecute
                sourceConfig
                headAndTail
                (DataLoader.execute actionsForest)
            either
              (throw500WithDetail "MySQL DataLoader Error" . toJSON . show)
              (pure . encJFromRecordSet)
              result
        )
    )

--------------------------------------------------------------------------------
-- Encoding for Hasura's GraphQL JSON representation

mysqlDBQueryExplain ::
  MonadError QErr m =>
  RootFieldAlias ->
  UserInfo ->
  SourceName ->
  SourceConfig 'MySQL ->
  QueryDB 'MySQL Void (UnpreparedValue 'MySQL) ->
  m (AB.AnyBackend DBStepInfo)
mysqlDBQueryExplain fieldName userInfo sourceName sourceConfig qrf = do
  select :: MySQL.Select <- planQuery (_uiSession userInfo) qrf
  let sqlQuery = selectSQLTextForQuery select
      sqlQueryText = (T.decodeUtf8 . unQuery . toQueryPretty) (ToQuery.fromSelect select)
      explainResult =
        withMySQLPool
          (MySQL.scConnectionPool sourceConfig)
          ( \conn -> do
              query conn ("EXPLAIN FORMAT=JSON " <> (unQuery sqlQuery))
              result <- storeResult conn
              fields <- fetchFields result
              rows <- fetchAllRows result
              let texts = concat $ parseTextRows fields rows
              pure $ encJFromJValue $ ExplainPlan fieldName (Just sqlQueryText) (Just texts)
          )
  pure $
    AB.mkAnyBackend $
      DBStepInfo @'MySQL sourceName sourceConfig Nothing explainResult

selectSQLTextForQuery :: MySQL.Select -> ToQuery.Query
selectSQLTextForQuery select = toQueryFlat $ ToQuery.fromSelect select

encJFromRecordSet :: RecordSet -> EncJSON
encJFromRecordSet RecordSet {rows} =
  encJFromList
    ( map
        ( encJFromAssocList
            . map (first coerce . second encJFromOutputValue)
            . OMap.toList
        )
        (toList rows)
    )

encJFromOutputValue :: DataLoader.OutputValue -> EncJSON
encJFromOutputValue =
  \case
    ArrayOutputValue array -> encJFromList (map encJFromOutputValue (toList array))
    RecordOutputValue m ->
      encJFromAssocList
        . map (first coerce . second encJFromOutputValue)
        . OMap.toList
        $ m
    ScalarOutputValue value -> encJFromJValue value
    NullOutputValue {} -> encJFromJValue J.Null
