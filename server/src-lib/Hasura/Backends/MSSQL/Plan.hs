-- | MSSQL Plan
--
-- Planning T-SQL queries and subscription by translating IR to MSSQL-specific
-- SQL query types.
module Hasura.Backends.MSSQL.Plan
  ( PrepareState (..),
    planQuery,
    planSourceRelationship,
    planSubscription,
    prepareValueQuery,
    resultAlias,
    resultIdAlias,
    resultVarsAlias,
    rowAlias,
  )
where

-- TODO: Re-add the export list after cleaning up the module
-- ( planQuery
-- , planSubscription
-- ) where

import Control.Applicative (Const (Const))
import Data.Aeson qualified as J
import Data.ByteString.Lazy (toStrict)
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.HashSet qualified as Set
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Text.Extended
import Database.ODBC.SQLServer qualified as ODBC
import Hasura.Backends.MSSQL.FromIr
import Hasura.Backends.MSSQL.FromIr.Query (fromQueryRootField, fromSourceRelationship)
import Hasura.Backends.MSSQL.Types.Internal
import Hasura.Base.Error
import Hasura.GraphQL.Parser qualified as GraphQL
import Hasura.Prelude hiding (first)
import Hasura.RQL.IR
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column qualified as RQL
import Hasura.RQL.Types.Common qualified as RQL
import Hasura.SQL.Types
import Hasura.Session
import Language.GraphQL.Draft.Syntax qualified as G
import Network.HTTP.Types qualified as HTTP

--------------------------------------------------------------------------------
-- Top-level planner

planQuery ::
  (MonadError QErr m) =>
  SessionVariables ->
  QueryDB 'MSSQL Void (UnpreparedValue 'MSSQL) ->
  m (QueryWithDDL Select)
planQuery sessionVariables queryDB = do
  rootField <- traverse (prepareValueQuery sessionVariables) queryDB
  runIrWrappingRoot $ fromQueryRootField rootField

-- | For more information, see the module/documentation of 'Hasura.GraphQL.Execute.RemoteJoin.Source'.
planSourceRelationship ::
  (MonadError QErr m) =>
  SessionVariables ->
  -- | List of json objects, each of which becomes a row of the table
  NE.NonEmpty J.Object ->
  -- | The above objects have this schema
  HashMap.HashMap RQL.FieldName (ColumnName, ScalarType) ->
  RQL.FieldName ->
  (RQL.FieldName, SourceRelationshipSelection 'MSSQL Void UnpreparedValue) ->
  m Select
planSourceRelationship
  sessionVariables
  lhs
  lhsSchema
  argumentId
  (relationshipName, sourceRelationshipRaw) = do
    sourceRelationship <-
      traverseSourceRelationshipSelection
        (fmap Const . prepareValueQuery sessionVariables)
        sourceRelationshipRaw
    qwdQuery
      <$> runIrWrappingRoot
        ( fromSourceRelationship
            lhs
            lhsSchema
            argumentId
            (relationshipName, sourceRelationship)
        )

runIrWrappingRoot ::
  (MonadError QErr m) =>
  FromIr Select ->
  m (QueryWithDDL Select)
runIrWrappingRoot selectAction =
  runFromIrUseCTEs selectAction `onLeft` (throwError . overrideQErrStatus HTTP.status400 NotSupported)

-- | Prepare a value without any query planning; we just execute the
-- query with the values embedded.
prepareValueQuery ::
  (MonadError QErr m) =>
  SessionVariables ->
  UnpreparedValue 'MSSQL ->
  m Expression
prepareValueQuery sessionVariables =
  {- History note:
      This function used to be called 'planNoPlan', and was used for building sql
      expressions for queries. That evolved differently, but this function is now
      left as a *suggestion* for implementing support for mutations.
      -}
  \case
    UVLiteral x -> pure x
    UVSession -> pure $ ValueExpression $ ODBC.ByteStringValue $ toStrict $ J.encode sessionVariables
    UVParameter _ RQL.ColumnValue {..} -> pure $ ValueExpression cvValue
    UVSessionVar typ sessionVariable -> do
      value <-
        getSessionVariableValue sessionVariable sessionVariables
          `onNothing` throw400 NotFound ("missing session variable: " <>> sessionVariable)
      -- See https://github.com/fpco/odbc/pull/34#issuecomment-812223147
      -- We first cast to nvarchar(max) because casting from ntext is not supported
      CastExpression (CastExpression (ValueExpression $ ODBC.TextValue value) WvarcharType DataLengthMax)
        <$> case typ of
          CollectableTypeScalar baseTy ->
            pure baseTy
          CollectableTypeArray {} ->
            throw400 NotSupported "Array types are currently not supported in MS SQL Server"
        <*> pure DataLengthMax

planSubscription ::
  (MonadError QErr m) =>
  InsOrdHashMap.InsOrdHashMap G.Name (QueryDB 'MSSQL Void (UnpreparedValue 'MSSQL)) ->
  SessionVariables ->
  m (Reselect, PrepareState)
planSubscription unpreparedMap sessionVariables = do
  (rootFieldMap, prepareState) <-
    runStateT
      ( traverse
          (traverse (prepareValueSubscription (getSessionVariablesSet sessionVariables)))
          unpreparedMap
      )
      emptyPrepareState
  let rootFields :: InsOrdHashMap G.Name (FromIr Select)
      rootFields = fmap fromQueryRootField rootFieldMap
  selectMap <- fmap qwdQuery <$> runFromIrUseCTEsT rootFields
  pure (collapseMap selectMap, prepareState)

-- Plan a query without prepare/exec.
-- planNoPlanMap ::
--      InsOrdHashMap.InsOrdHashMap G.Name (SubscriptionRootFieldMSSQL (UnpreparedValue 'MSSQL))
--   -> Either PrepareError Reselect
-- planNoPlanMap _unpreparedMap =
-- let rootFieldMap = runIdentity $
--       traverse (traverseQueryRootField (pure . prepareValueNoPlan)) unpreparedMap
-- selectMap <-
--   first
--     FromIrError
--     (runValidate (runFromIr (traverse fromRootField rootFieldMap)))
-- pure (collapseMap selectMap)

--------------------------------------------------------------------------------
-- Converting a root field into a T-SQL select statement

-- | Collapse a set of selects into a single select that projects
-- these as subselects.
collapseMap ::
  InsOrdHashMap.InsOrdHashMap G.Name Select ->
  Reselect
collapseMap selects =
  Reselect
    { reselectFor =
        JsonFor ForJson {jsonCardinality = JsonSingleton, jsonRoot = NoRoot},
      reselectWhere = Where mempty,
      reselectProjections =
        map projectSelect (InsOrdHashMap.toList selects)
    }
  where
    projectSelect :: (G.Name, Select) -> Projection
    projectSelect (name, sel) =
      ExpressionProjection
        ( Aliased
            { aliasedThing = SelectExpression sel,
              aliasedAlias = G.unName name
            }
        )

--------------------------------------------------------------------------------
-- Session variables

-- globalSessionExpression :: Expression
-- globalSessionExpression =
--   ValueExpression (ODBC.TextValue "current_setting('hasura.user')::json")

--------------------------------------------------------------------------------
-- Resolving values

-- data PrepareError
--   = FromIrError (NonEmpty Error)

data PrepareState = PrepareState
  { positionalArguments :: [RQL.ColumnValue 'MSSQL],
    namedArguments :: HashMap G.Name (RQL.ColumnValue 'MSSQL),
    sessionVariables :: Set.HashSet SessionVariable
  }

emptyPrepareState :: PrepareState
emptyPrepareState =
  PrepareState
    { positionalArguments = mempty,
      namedArguments = mempty,
      sessionVariables = mempty
    }

-- | Prepare a value for multiplexed queries.
prepareValueSubscription ::
  (MonadState PrepareState m, MonadError QErr m) =>
  Set.HashSet SessionVariable ->
  UnpreparedValue 'MSSQL ->
  m Expression
prepareValueSubscription globalVariables =
  \case
    UVLiteral x -> pure x
    UVSession -> do
      modify' (\s -> s {sessionVariables = sessionVariables s <> globalVariables})
      pure $ resultVarExp (RootPath `FieldPath` "session")
    UVSessionVar _typ text -> do
      unless (text `Set.member` globalVariables)
        $ throw400
          NotFound
          ("missing session variable: " <>> sessionVariableToText text)
      modify' (\s -> s {sessionVariables = text `Set.insert` sessionVariables s})
      pure $ resultVarExp (sessionDot $ toTxt text)
    UVParameter (FromGraphQL variableInfo) columnValue -> do
      let name = GraphQL.getName variableInfo

      modify
        ( \s ->
            s
              { namedArguments =
                  HashMap.insert name columnValue (namedArguments s)
              }
        )
      pure $ resultVarExp (queryDot $ G.unName name)
    UVParameter _ columnValue -> do
      currentIndex <- toInteger . length <$> gets positionalArguments
      modify'
        ( \s ->
            s
              { positionalArguments = positionalArguments s <> [columnValue]
              }
        )
      pure (resultVarExp (syntheticIx currentIndex))
  where
    resultVarExp :: JsonPath -> Expression
    resultVarExp =
      JsonValueExpression
        $ ColumnExpression
        $ FieldName
          { fieldNameEntity = rowAlias,
            fieldName = resultVarsAlias
          }

    queryDot :: Text -> JsonPath
    queryDot name = RootPath `FieldPath` "query" `FieldPath` name

    syntheticIx :: Integer -> JsonPath
    syntheticIx i = RootPath `FieldPath` "synthetic" `IndexPath` i

    sessionDot :: Text -> JsonPath
    sessionDot name = RootPath `FieldPath` "session" `FieldPath` name

resultIdAlias :: T.Text
resultIdAlias = "result_id"

resultVarsAlias :: T.Text
resultVarsAlias = "result_vars"

resultAlias :: T.Text
resultAlias = "result"

rowAlias :: T.Text
rowAlias = "row"
