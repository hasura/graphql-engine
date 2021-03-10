{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.MSSQL.Instances.Execute (NoMultiplex(..)) where

import           Hasura.Prelude

import qualified Data.Environment                      as Env
import qualified Data.HashMap.Strict.InsOrd            as OMap
import qualified Database.ODBC.SQLServer               as ODBC
import qualified Language.GraphQL.Draft.Syntax         as G
import qualified Network.HTTP.Client                   as HTTP
import qualified Network.HTTP.Types                    as HTTP

import           Data.Text.Extended

import           Hasura.Backends.MSSQL.Connection
import           Hasura.Backends.MSSQL.Plan
import           Hasura.Backends.MSSQL.ToQuery
import           Hasura.EncJSON
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Execute.Backend
import           Hasura.GraphQL.Execute.LiveQuery.Plan
import           Hasura.GraphQL.Parser
import           Hasura.RQL.Types
import           Hasura.Session


instance BackendExecute 'MSSQL where
  type PreparedQuery    'MSSQL = Text
  type MultiplexedQuery 'MSSQL = NoMultiplex
  type ExecutionMonad   'MSSQL = ExceptT QErr IO
  getRemoteJoins = const []

  mkDBQueryPlan = msDBQueryPlan
  mkDBMutationPlan = msDBMutationPlan
  mkDBSubscriptionPlan = msDBSubscriptionPlan



-- multiplexed query

newtype NoMultiplex = NoMultiplex (G.Name, ODBC.Query)

instance ToTxt NoMultiplex where
  toTxt (NoMultiplex (_name, query)) = toTxt query


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
  -> SourceConfig 'MSSQL
  -> QueryDB 'MSSQL (UnpreparedValue 'MSSQL)
  -> m ExecutionStep
msDBQueryPlan _env _manager _reqHeaders userInfo _directives sourceConfig qrf = do
  select <- fromSelect <$> planNoPlan userInfo qrf
  let queryString = ODBC.renderQuery $ toQueryPretty select
      pool  = _mscConnectionPool sourceConfig
      odbcQuery = encJFromText <$> runJSONPathQuery pool (toQueryFlat select)
  pure $ ExecStepDB sourceConfig (Just queryString) [] odbcQuery

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
  -> SourceConfig 'MSSQL
  -> MutationDB 'MSSQL (UnpreparedValue 'MSSQL)
  -> m ExecutionStep
msDBMutationPlan _env _manager _reqHeaders _userInfo _stringifyNum _sourceConfig _mrf =
  throw500 "mutations are not supported in MSSQL; this should be unreachable"


-- subscription

msDBSubscriptionPlan
  :: forall m.
     ( MonadError QErr m
     )
  => UserInfo
  -> SourceConfig 'MSSQL
  -> InsOrdHashMap G.Name (QueryDB 'MSSQL (UnpreparedValue 'MSSQL))
  -> m (LiveQueryPlan 'MSSQL (MultiplexedQuery 'MSSQL))
msDBSubscriptionPlan userInfo sourceConfig rootFields = do
  -- WARNING: only keeping the first root field for now!
  query <- traverse mkQuery $ head $ OMap.toList rootFields
  let roleName = _uiRole userInfo
      parameterizedPlan = ParameterizedLiveQueryPlan roleName $ NoMultiplex query
  pure
    $ LiveQueryPlan parameterizedPlan sourceConfig
    $ mkCohortVariables mempty mempty mempty mempty
  where
    mkQuery = fmap (toQueryFlat . fromSelect) . planNoPlan userInfo
