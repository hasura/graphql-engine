{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.MySQL.Instances.Execute where

import           Data.String                      (IsString (..))
import           Hasura.Backends.MySQL.Connection
import           Hasura.Backends.MySQL.Plan
import           Hasura.Backends.MySQL.ToQuery
import qualified Hasura.Backends.MySQL.Types      as MySQL
import           Hasura.Base.Error
import           Hasura.EncJSON
import           Hasura.GraphQL.Execute.Backend
import           Hasura.GraphQL.Parser
import           Hasura.Prelude
import           Hasura.QueryTags                 (QueryTagsComment (..))
import           Hasura.RQL.IR
import           Hasura.RQL.Types
import           Hasura.Session

instance BackendExecute 'MySQL where
  type PreparedQuery    'MySQL = Text
  type MultiplexedQuery 'MySQL = Void
  type ExecutionMonad   'MySQL = ExceptT QErr IO
  mkDBQueryPlan                = mysqlDBQueryPlan
  mkDBMutationPlan             = error "mkDBMutationPlan: MySQL backend does not support this operation yet."
  mkDBSubscriptionPlan _ _ _ _ = error "mkDBSubscriptionPlan: MySQL backend does not support this operation yet."
  mkDBQueryExplain             = error "mkDBQueryExplain: MySQL backend does not support this operation yet."
  mkLiveQueryExplain _         = error "mkLiveQueryExplain: MySQL backend does not support this operation yet."

mysqlDBQueryPlan
  :: forall m.
     ( MonadError QErr m
     )
  => UserInfo
  -> SourceName
  -> SourceConfig 'MySQL
  -> QueryDB 'MySQL (Const Void) (UnpreparedValue 'MySQL)
  -> QueryTagsComment
  -> m (DBStepInfo 'MySQL)
mysqlDBQueryPlan userInfo sourceName sourceConfig qrf _qtc = do
  let sessionVariables = _uiSession userInfo
  statement :: MySQL.Select <- planQuery sessionVariables qrf
  let printer :: Printer = fromSelect statement
      queryString = toQueryPretty printer
      pool  = MySQL.scConnectionPool sourceConfig
      mysqlQuery = encJFromText <$> runJSONPathQuery pool (toQueryFlat printer)
  pure $ DBStepInfo @'MySQL sourceName sourceConfig (Just $ fromString $ show queryString) mysqlQuery
