-- | Planning T-SQL queries and subscriptions.

module Hasura.Backends.MSSQL.Plan where
  -- TODO: Re-add the export list after cleaning up the module
  -- ( planNoPlan
  -- , planNoPlanMap
  -- , planMultiplex
  -- ) where

import           Hasura.Prelude                hiding (first)

import qualified Data.Aeson                    as J
import qualified Data.HashMap.Strict           as HM
import qualified Data.HashMap.Strict.InsOrd    as OMap
import qualified Data.HashSet                  as Set
import qualified Data.Text                     as T
import qualified Database.ODBC.SQLServer       as ODBC
import qualified Language.GraphQL.Draft.Syntax as G

import           Control.Monad.Validate
import           Data.ByteString.Lazy          (toStrict)
import           Data.Text.Extended

import qualified Hasura.GraphQL.Parser         as GraphQL
import qualified Hasura.RQL.Types.Column       as RQL

import           Hasura.Backends.MSSQL.FromIr  as TSQL
import           Hasura.Backends.MSSQL.Types   as TSQL
import           Hasura.GraphQL.Context
import           Hasura.RQL.Types.Error
import           Hasura.SQL.Backend
import           Hasura.Session


newtype QDB v b = QDB (QueryDB b v)

type SubscriptionRootFieldMSSQL v = RootField (QDB v) Void Void {-(RQL.AnnActionAsyncQuery 'MSSQL v)-} Void


-- --------------------------------------------------------------------------------
-- -- Top-level planner

planNoPlan
  :: MonadError QErr m
  => UserInfo
  -> QueryDB 'MSSQL (GraphQL.UnpreparedValue 'MSSQL)
  -> m Select
planNoPlan userInfo queryDB = do
  rootField <- traverseQueryDB (prepareValueNoPlan (_uiSession userInfo)) queryDB
  select <-
    runValidate (TSQL.runFromIr (TSQL.fromRootField rootField))
    `onLeft` (throw400 NotSupported . tshow)
  pure
    select
      { selectFor =
          case selectFor select of
            NoFor           -> NoFor
            JsonFor forJson -> JsonFor forJson {jsonRoot = Root "root"}
      }

planMultiplex
  :: MonadError QErr m
  => OMap.InsOrdHashMap G.Name (QueryDB 'MSSQL (GraphQL.UnpreparedValue 'MSSQL))
  -> SessionVariables
  -> m (Reselect, PrepareState)
planMultiplex unpreparedMap sessionVariables = do
  let (rootFieldMap, prepareState) =
        runState
          (traverse
            (traverseQueryDB (prepareValueMultiplex (getSessionVariablesSet sessionVariables)))
            unpreparedMap)
          emptyPrepareState
  selectMap <-
    runValidate (TSQL.runFromIr (traverse TSQL.fromRootField rootFieldMap))
    `onLeft` (throw400 NotSupported . tshow)
  pure (collapseMap selectMap, prepareState)

  -- Plan a query without prepare/exec.
  -- planNoPlanMap ::
  --      OMap.InsOrdHashMap G.Name (SubscriptionRootFieldMSSQL (GraphQL.UnpreparedValue 'MSSQL))
  --   -> Either PrepareError Reselect
  -- planNoPlanMap _unpreparedMap =
  -- let rootFieldMap = runIdentity $
  --       traverse (traverseQueryRootField (pure . prepareValueNoPlan)) unpreparedMap
  -- selectMap <-
  --   first
  --     FromIrError
  --     (runValidate (TSQL.runFromIr (traverse TSQL.fromRootField rootFieldMap)))
  -- pure (collapseMap selectMap)

--------------------------------------------------------------------------------
-- Converting a root field into a T-SQL select statement

-- | Collapse a set of selects into a single select that projects
-- these as subselects.
collapseMap :: OMap.InsOrdHashMap G.Name Select
            -> Reselect
collapseMap selects =
  Reselect
    { reselectFor =
        JsonFor ForJson {jsonCardinality = JsonSingleton, jsonRoot = NoRoot}
    , reselectWhere = Where mempty
    , reselectProjections =
        map projectSelect (OMap.toList selects)
    }
  where
    projectSelect :: (G.Name, Select) -> Projection
    projectSelect (name, select) =
      ExpressionProjection
        (Aliased
           { aliasedThing = SelectExpression select
           , aliasedAlias = G.unName name
           })

--------------------------------------------------------------------------------
-- Session variables

globalSessionExpression :: TSQL.Expression
globalSessionExpression =
  ValueExpression (ODBC.TextValue "current_setting('hasura.user')::json")


--------------------------------------------------------------------------------
-- Resolving values

data PrepareError
  = FromIrError (NonEmpty TSQL.Error)

data PrepareState = PrepareState
  { positionalArguments :: ![RQL.ColumnValue 'MSSQL]
  , namedArguments      :: !(HashMap G.Name (RQL.ColumnValue 'MSSQL))
  , sessionVariables    :: !(Set.HashSet SessionVariable)
  }

emptyPrepareState :: PrepareState
emptyPrepareState = PrepareState
  { positionalArguments = mempty
  , namedArguments = mempty
  , sessionVariables = mempty
  }

-- | Prepare a value without any query planning; we just execute the
-- query with the values embedded.
prepareValueNoPlan
  :: MonadError QErr m
  => SessionVariables
  -> GraphQL.UnpreparedValue 'MSSQL
  -> m TSQL.Expression
prepareValueNoPlan sessionVariables =
  \case
    GraphQL.UVLiteral x -> pure x
    GraphQL.UVSession -> pure $ ValueExpression $ ODBC.ByteStringValue $ toStrict $ J.encode sessionVariables
    GraphQL.UVParameter _ RQL.ColumnValue{..} -> pure $ ValueExpression cvValue
    GraphQL.UVSessionVar _typ sessionVariable -> do
      value <- getSessionVariableValue sessionVariable sessionVariables
        `onNothing` throw400 NotFound ("missing session variable: " <>> sessionVariable)
      pure $ ValueExpression $ ODBC.TextValue value

-- | Prepare a value for multiplexed queries.
prepareValueMultiplex
  :: Set.HashSet SessionVariable
  -> GraphQL.UnpreparedValue 'MSSQL
  -> State PrepareState TSQL.Expression
prepareValueMultiplex globalVariables =
  \case
    GraphQL.UVLiteral x -> pure x
    GraphQL.UVSession -> do
      modify' (\s -> s {sessionVariables = sessionVariables s <> globalVariables})
      pure $ JsonValueExpression
        (ColumnExpression
          FieldName
          { fieldNameEntity = rowAlias
          , fieldName = resultVarsAlias
          })
        (RootPath `FieldPath` "session")
    GraphQL.UVSessionVar _typ text -> do
      modify' (\s -> s {sessionVariables = text `Set.insert` sessionVariables s})
      pure $ JsonValueExpression
        (ColumnExpression
          FieldName
          { fieldNameEntity = rowAlias
          , fieldName = resultVarsAlias
          })
        (RootPath `FieldPath` "session" `FieldPath` toTxt text)
    GraphQL.UVParameter mVariableInfo columnValue ->
      case fmap GraphQL.getName mVariableInfo of
        Nothing -> do
          modify' (\s -> s {
            positionalArguments = positionalArguments s <> [columnValue] })
          index <- (toInteger . length) <$> gets positionalArguments
          pure
            (JsonValueExpression
               (ColumnExpression
                  FieldName
                    { fieldNameEntity = rowAlias
                    , fieldName = resultVarsAlias
                    })
               (RootPath `FieldPath` "synthetic" `IndexPath` index))
        Just name -> do
          modify
            (\s ->
               s
                 { namedArguments =
                     HM.insert name columnValue (namedArguments s)
                 })
          pure
            (JsonValueExpression
               (ColumnExpression
                  FieldName
                    { fieldNameEntity = rowAlias
                    , fieldName = resultVarsAlias
                    })
               (RootPath `FieldPath` "query" `FieldPath` G.unName name))


resultIdAlias :: T.Text
resultIdAlias = "result_id"

resultVarsAlias :: T.Text
resultVarsAlias = "result_vars"

resultAlias :: T.Text
resultAlias = "result"

rowAlias :: T.Text
rowAlias = "row"
