{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.MSSQL.Instances.Execute
  (
    MultiplexedQuery'(..),
    PreparedQuery'(..),
    multiplexRootReselect,
    queryEnvJson
  )
  where

import           Hasura.Prelude

import qualified Data.Aeson.Extended                   as J
import qualified Data.Environment                      as Env
import qualified Data.HashSet                          as Set
import qualified Data.List.NonEmpty                    as NE
import qualified Data.Text.Extended                    as T
import qualified Database.ODBC.SQLServer               as ODBC
import qualified Language.GraphQL.Draft.Syntax         as G
import qualified Network.HTTP.Client                   as HTTP
import qualified Network.HTTP.Types                    as HTTP

import qualified Hasura.SQL.AnyBackend                 as AB

import           Hasura.Backends.MSSQL.Connection
import           Hasura.Backends.MSSQL.FromIr          as TSQL
import           Hasura.Backends.MSSQL.Plan
import           Hasura.Backends.MSSQL.SQL.Value       (toTxtEncodedVal)
import           Hasura.Backends.MSSQL.ToQuery
import           Hasura.Backends.MSSQL.Types           as TSQL
import           Hasura.Base.Error
import           Hasura.EncJSON
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Execute.Backend
import           Hasura.GraphQL.Execute.LiveQuery.Plan
import           Hasura.GraphQL.Parser
import           Hasura.RQL.Types
import           Hasura.Session


instance BackendExecute 'MSSQL where
  type PreparedQuery    'MSSQL = PreparedQuery'
  type MultiplexedQuery 'MSSQL = MultiplexedQuery'
  type ExecutionMonad   'MSSQL = ExceptT QErr IO
  getRemoteJoins = const []

  mkDBQueryPlan = msDBQueryPlan
  mkDBMutationPlan = msDBMutationPlan
  mkDBSubscriptionPlan = msDBSubscriptionPlan
  mkDBQueryExplain = msDBQueryExplain
  mkLiveQueryExplain = msDBLiveQueryExplain


-- Prepared query

data PreparedQuery' = PreparedQuery'
  { pqQueryString :: Text
  , pqGraphQlEnv  :: PrepareState
  , pqSession     :: SessionVariables
  }

-- | Render as a JSON object the variables that have been collected from an RQL
-- expression.
queryEnvJson :: PrepareState -> SessionVariables -> J.Value
queryEnvJson (PrepareState posArgs namedArgs requiredSessionVars) sessionVars =
  let sessionVarValues = filterSessionVariables (\k _ -> Set.member k requiredSessionVars) sessionVars
  in J.object
  [ "session" J..= sessionVarValues
  , "namedArguments" J..= toTxtEncodedVal namedArgs
  , "positionalArguments" J..= toTxtEncodedVal posArgs
  ]

-- multiplexed query
newtype MultiplexedQuery' = MultiplexedQuery' Reselect

instance T.ToTxt MultiplexedQuery' where
  toTxt (MultiplexedQuery' reselect) = T.toTxt $ toQueryPretty $ fromReselect reselect


-- query

msDBQueryPlan
  :: forall m.
     ( MonadError QErr m
     )
  => Env.Environment
  -> HTTP.Manager
  -> [HTTP.Header]
  -> UserInfo
  -> SourceName
  -> SourceConfig 'MSSQL
  -> QueryDB 'MSSQL (UnpreparedValue 'MSSQL)
  -> m ExecutionStep
msDBQueryPlan _env _manager _reqHeaders userInfo sourceName sourceConfig qrf = do
  let sessionVariables = _uiSession userInfo
  (statement, queryEnv) <- planQuery sessionVariables qrf
  let selectWithEnv = joinEnv statement sessionVariables queryEnv
      printer = fromSelect selectWithEnv
      queryString = ODBC.renderQuery $ toQueryPretty printer
      pool  = _mscConnectionPool sourceConfig
      odbcQuery = encJFromText <$> runJSONPathQuery pool (toQueryFlat printer)
  pure
    $ ExecStepDB []
    . AB.mkAnyBackend
    $ DBStepInfo @'MSSQL sourceName sourceConfig (Just $ PreparedQuery' queryString queryEnv sessionVariables) odbcQuery

joinEnv :: Select -> SessionVariables -> PrepareState -> Select
joinEnv querySelect sessionVars prepState =
  querySelect
    -- We *prepend* the variables of 'prepState' to the list of joins to make
    -- them available in the @where@ clause as well as subsequent queries nested
    -- in @join@ clauses.
    { selectJoins = [prepJoin] <> selectJoins querySelect }

  where
    prepJoin :: Join
    prepJoin = Join
      { joinSource = JoinSelect $ (select
          (
            FromOpenJson
              Aliased
                { aliasedThing =
                    OpenJson
                      { openJsonExpression =
                          ValueExpression (ODBC.TextValue $ lbsToTxt $ J.encode $ queryEnvJson prepState sessionVars)
                      , openJsonWith =
                          NE.fromList
                            [ JsonField "session" Nothing
                            , JsonField "namedArguments" Nothing
                            , JsonField "positionalArguments" Nothing
                            ]
                      }
                , aliasedAlias = rowAlias
                }
          )) {selectProjections = [ StarProjection ]
        }
      ,joinJoinAlias =
          JoinAlias
            { joinAliasEntity = rowAlias
            , joinAliasField = Nothing
            }
      }

select :: From -> Select
select from =
  Select
    { selectFrom        = from
    , selectTop         = NoTop
    , selectProjections = []
    , selectJoins       = []
    , selectWhere       = Where []
    , selectOrderBy     = Nothing
    , selectFor         = NoFor
    , selectOffset      = Nothing
    }



runShowplan
  :: ODBC.Query -> ODBC.Connection -> IO [Text]
runShowplan query conn = do
  ODBC.exec conn "SET SHOWPLAN_TEXT ON"
  texts <- ODBC.query conn query
  ODBC.exec conn "SET SHOWPLAN_TEXT OFF"
  -- we don't need to use 'finally' here - if an exception occurs,
  -- the connection is removed from the resource pool in 'withResource'.
  pure texts

msDBQueryExplain
  :: MonadError QErr m
  => G.Name
  -> UserInfo
  -> SourceName
  -> SourceConfig 'MSSQL
  -> QueryDB 'MSSQL (UnpreparedValue 'MSSQL)
  -> m (AB.AnyBackend DBStepInfo)
msDBQueryExplain fieldName userInfo sourceName sourceConfig qrf = do
  let sessionVariables = _uiSession userInfo
  (statement, queryEnv) <- planQuery sessionVariables qrf
  let selectWithEnv = joinEnv statement sessionVariables queryEnv
      query         = toQueryPretty (fromSelect selectWithEnv)
      queryString   = ODBC.renderQuery $ query
      pool          = _mscConnectionPool sourceConfig
      odbcQuery     =
        withMSSQLPool
          pool
          (\conn -> do
            showplan <- runShowplan query conn
            pure (encJFromJValue $
                  ExplainPlan
                    fieldName
                    (Just queryString)
                    (Just showplan)))
  pure
    $ AB.mkAnyBackend
    $ DBStepInfo @'MSSQL sourceName sourceConfig Nothing odbcQuery

msDBLiveQueryExplain
  :: (MonadIO m, MonadError QErr m)
  => LiveQueryPlan 'MSSQL (MultiplexedQuery 'MSSQL) -> m LiveQueryPlanExplanation
msDBLiveQueryExplain (LiveQueryPlan plan sourceConfig variables) = do
  let (MultiplexedQuery' reselect) = _plqpQuery plan
      query = toQueryPretty $ fromSelect $ multiplexRootReselect [(dummyCohortId, variables)] reselect
      pool = _mscConnectionPool sourceConfig
  explainInfo <- withMSSQLPool pool (runShowplan query)
  pure $ LiveQueryPlanExplanation (T.toTxt query) explainInfo variables

--------------------------------------------------------------------------------
-- Producing the correct SQL-level list comprehension to multiplex a query

-- Problem description:
--
-- Generate a query that repeats the same query N times but with
-- certain slots replaced:
--
-- [ Select x y | (x,y) <- [..] ]
--

multiplexRootReselect
  :: [(CohortId, CohortVariables)]
  -> TSQL.Reselect
  -> TSQL.Select
multiplexRootReselect variables rootReselect =
  Select
    { selectTop = NoTop
    , selectProjections =
        [ FieldNameProjection
            Aliased
              { aliasedThing =
                  TSQL.FieldName
                    {fieldNameEntity = rowAlias, fieldName = resultIdAlias}
              , aliasedAlias = resultIdAlias
              }
        , ExpressionProjection
            Aliased
              { aliasedThing =
                  ColumnExpression
                    (TSQL.FieldName
                      { fieldNameEntity = resultAlias
                      , fieldName = TSQL.jsonFieldName
                      })
              , aliasedAlias = resultAlias
              }
        ]
    , selectFrom =
        FromOpenJson
          Aliased
            { aliasedThing =
                OpenJson
                  { openJsonExpression =
                      ValueExpression (ODBC.TextValue $ lbsToTxt $ J.encode variables)
                  , openJsonWith =
                      NE.fromList
                        [ UuidField resultIdAlias (Just $ IndexPath RootPath 0)
                        , JsonField resultVarsAlias (Just $ IndexPath RootPath 1)
                        ]
                  }
            , aliasedAlias = rowAlias
            }
    , selectJoins =
        [ Join
            { joinSource = JoinReselect rootReselect
            , joinJoinAlias =
                JoinAlias
                  { joinAliasEntity = resultAlias
                  , joinAliasField = Just TSQL.jsonFieldName
                  }
            }
        ]
    , selectWhere = Where mempty
    , selectFor =
        JsonFor ForJson {jsonCardinality = JsonArray, jsonRoot = NoRoot}
    , selectOrderBy = Nothing
    , selectOffset = Nothing
    }


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
  -> SourceConfig 'MSSQL
  -> MutationDB 'MSSQL (UnpreparedValue 'MSSQL)
  -> m ExecutionStep
msDBMutationPlan _env _manager _reqHeaders _userInfo _stringifyNum _sourceName _sourceConfig _mrf =
  throw500 "mutations are not supported in MSSQL; this should be unreachable"

-- subscription

msDBSubscriptionPlan
  :: forall m.
     ( MonadError QErr m
     )
  => UserInfo
  -> SourceName
  -> SourceConfig 'MSSQL
  -> InsOrdHashMap G.Name (QueryDB 'MSSQL (UnpreparedValue 'MSSQL))
  -> m (LiveQueryPlan 'MSSQL (MultiplexedQuery 'MSSQL))
msDBSubscriptionPlan UserInfo {_uiSession, _uiRole} _sourceName sourceConfig rootFields = do
  (reselect, prepareState) <- planSubscription rootFields _uiSession

  let cohortVariables = prepareStateCohortVariables _uiSession prepareState
      parameterizedPlan = ParameterizedLiveQueryPlan _uiRole $ MultiplexedQuery' reselect

  pure
    $ LiveQueryPlan parameterizedPlan sourceConfig cohortVariables

prepareStateCohortVariables :: SessionVariables -> PrepareState -> CohortVariables
prepareStateCohortVariables session prepState =
  let PrepareState{sessionVariables, namedArguments, positionalArguments} = prepState
  -- TODO: call MSSQL validateVariables
  -- (see https://github.com/hasura/graphql-engine-mono/issues/1210)
  -- We need to ensure that the values provided for variables are correct according to MSSQL.
  -- Without this check an invalid value for a variable for one instance of the subscription will
  -- take down the entire multiplexed query.
  in mkCohortVariables
        sessionVariables
        session
        (toTxtEncodedVal namedArguments)
        (toTxtEncodedVal positionalArguments)
