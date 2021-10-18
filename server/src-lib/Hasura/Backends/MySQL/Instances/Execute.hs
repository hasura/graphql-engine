{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.MySQL.Instances.Execute where

import Data.Aeson as J
import Data.HashMap.Strict.InsOrd qualified as OMap
import Data.Text qualified as T
import Data.Tree
import Hasura.Backends.MySQL.DataLoader.Execute (OutputValue (..), RecordSet (..))
import Hasura.Backends.MySQL.DataLoader.Execute qualified as DataLoader
import Hasura.Backends.MySQL.DataLoader.Plan qualified as DataLoader
import Hasura.Backends.MySQL.Plan
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.GraphQL.Execute.Backend
import Hasura.GraphQL.Parser
import Hasura.Prelude
import Hasura.RQL.IR
import Hasura.RQL.Types
import Hasura.Session

instance BackendExecute 'MySQL where
  type PreparedQuery 'MySQL = Text
  type MultiplexedQuery 'MySQL = Void
  type ExecutionMonad 'MySQL = ExceptT QErr IO
  mkDBQueryPlan = mysqlDBQueryPlan
  mkDBMutationPlan = error "mkDBMutationPlan: MySQL backend does not support this operation yet."
  mkDBSubscriptionPlan _ _ _ _ = error "mkDBSubscriptionPlan: MySQL backend does not support this operation yet."
  mkDBQueryExplain = error "mkDBQueryExplain: MySQL backend does not support this operation yet."
  mkLiveQueryExplain _ = error "mkLiveQueryExplain: MySQL backend does not support this operation yet."
  mkDBRemoteRelationshipPlan = error "mkDBRemoteRelationshipPlan: MySQL does not support this operation yet."

mysqlDBQueryPlan ::
  forall m.
  ( MonadError QErr m
  ) =>
  UserInfo ->
  SourceName ->
  SourceConfig 'MySQL ->
  QueryDB 'MySQL (Const Void) (UnpreparedValue 'MySQL) ->
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
