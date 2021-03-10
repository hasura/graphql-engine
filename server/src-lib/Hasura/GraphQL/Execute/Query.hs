module Hasura.GraphQL.Execute.Query
  ( convertQuerySelSet
  -- , queryOpFromPlan
  -- , ReusableQueryPlan
  , PreparedSql(..)
  , traverseQueryDB
  , parseGraphQLQuery
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                                 as J
import qualified Data.Environment                           as Env
import qualified Data.HashMap.Strict                        as Map
import qualified Data.HashMap.Strict.InsOrd                 as OMap
import qualified Data.Sequence.NonEmpty                     as NESeq
import qualified Language.GraphQL.Draft.Syntax              as G
import qualified Network.HTTP.Client                        as HTTP
import qualified Network.HTTP.Types                         as HTTP

import qualified Hasura.GraphQL.Transport.HTTP.Protocol     as GH
import qualified Hasura.Logging                             as L
import qualified Hasura.Tracing                             as Tracing

import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Execute.Action
import           Hasura.GraphQL.Execute.Backend
import           Hasura.GraphQL.Execute.Common
import           Hasura.GraphQL.Execute.Remote
import           Hasura.GraphQL.Execute.Resolve
import           Hasura.GraphQL.Parser
import           Hasura.RQL.Types
import           Hasura.Server.Version                      (HasVersion)
import           Hasura.Session

-- backend instances
import           Hasura.Backends.MSSQL.Instances.Execute    ()
import           Hasura.Backends.Postgres.Instances.Execute ()


parseGraphQLQuery
  :: MonadError QErr m
  => GQLContext
  -> [G.VariableDefinition]
  -> Maybe (HashMap G.Name J.Value)
  -> G.SelectionSet G.NoFragments G.Name
  -> m ( InsOrdHashMap G.Name (QueryRootField UnpreparedValue)
       , QueryReusability
       )
parseGraphQLQuery gqlContext varDefs varValsM fields =
  resolveVariables varDefs (fromMaybe Map.empty varValsM) fields
  >>= (gqlQueryParser gqlContext >>> (`onLeft` reportParseErrors))
  where
    reportParseErrors errs = case NESeq.head errs of
      -- TODO: Our error reporting machinery doesn’t currently support reporting
      -- multiple errors at once, so we’re throwing away all but the first one
      -- here. It would be nice to report all of them!
      ParseError{ pePath, peMessage, peCode } ->
        throwError (err400 peCode peMessage){ qePath = pePath }


convertQuerySelSet
  :: forall m .
     ( MonadError QErr m
     , HasVersion
     , Tracing.MonadTrace m
     , MonadIO m
     )
  => Env.Environment
  -> L.Logger L.Hasura
  -> GQLContext
  -> UserInfo
  -> HTTP.Manager
  -> HTTP.RequestHeaders
  -> [G.Directive G.Name]
  -> G.SelectionSet G.NoFragments G.Name
  -> [G.VariableDefinition]
  -> Maybe GH.VariableValues
  -> m (ExecutionPlan, [QueryRootField UnpreparedValue])
convertQuerySelSet env logger gqlContext userInfo manager reqHeaders directives fields varDefs varValsM = do
  -- Parse the GraphQL query into the RQL AST
  (unpreparedQueries, _reusability) <- parseGraphQLQuery gqlContext varDefs varValsM fields

  -- Transform the query plans into an execution plan
  let usrVars = _uiSession userInfo
  executionPlan <- for unpreparedQueries \case
    RFDB _ (sourceConfig :: SourceConfig b) (QDBR db) -> case backendTag @b of
      PostgresTag -> mkDBQueryPlan env manager reqHeaders userInfo directives sourceConfig db
      MSSQLTag    -> mkDBQueryPlan env manager reqHeaders userInfo directives sourceConfig db
    RFRemote rf -> do
      RemoteFieldG remoteSchemaInfo remoteField <- for rf $ resolveRemoteVariable userInfo
      pure $ buildExecStepRemote remoteSchemaInfo G.OperationTypeQuery [G.SelectionField remoteField]
    RFAction a -> do
      action <- case a of
        AQQuery s -> AEPSync . _aerExecution <$> resolveActionExecution env logger userInfo s (ActionExecContext manager reqHeaders usrVars)
        AQAsync s -> pure $ AEPAsyncQuery (_aaaqActionId s) $ resolveAsyncActionQuery userInfo s
      pure $ ExecStepAction action []
    RFRaw r ->
      pure $ ExecStepRaw r

  -- See Note [Temporarily disabling query plan caching]
  pure (executionPlan, OMap.elems unpreparedQueries)
