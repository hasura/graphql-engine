-- | Planning T-SQL queries and subscriptions.
module Hasura.Backends.MSSQL.Plan
  ( PrepareState (..),
    planQuery,
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

import Control.Monad.Validate
import Data.Aeson qualified as J
import Data.ByteString.Lazy (toStrict)
import Data.HashMap.Strict qualified as HM
import Data.HashMap.Strict.InsOrd qualified as OMap
import Data.HashSet qualified as Set
import Data.Text qualified as T
import Data.Text.Extended
import Database.ODBC.SQLServer qualified as ODBC
import Hasura.Backends.MSSQL.FromIr
import Hasura.Backends.MSSQL.Types.Internal
import Hasura.Base.Error
import Hasura.GraphQL.Parser qualified as GraphQL
import Hasura.Prelude hiding (first)
import Hasura.RQL.IR
import Hasura.RQL.Types.Column qualified as RQL
import Hasura.SQL.Backend
import Hasura.SQL.Types
import Hasura.Session
import Language.GraphQL.Draft.Syntax qualified as G

--------------------------------------------------------------------------------
-- Top-level planner

planQuery ::
  MonadError QErr m =>
  SessionVariables ->
  QueryDB 'MSSQL Void (GraphQL.UnpreparedValue 'MSSQL) ->
  m Select
planQuery sessionVariables queryDB = do
  rootField <- traverse (prepareValueQuery sessionVariables) queryDB
  runValidate (runFromIr (fromRootField rootField))
    `onLeft` (throw400 NotSupported . tshow)

-- | Prepare a value without any query planning; we just execute the
-- query with the values embedded.
prepareValueQuery ::
  MonadError QErr m =>
  SessionVariables ->
  GraphQL.UnpreparedValue 'MSSQL ->
  m Expression
prepareValueQuery sessionVariables =
  {- History note:
      This function used to be called 'planNoPlan', and was used for building sql
      expressions for queries. That evolved differently, but this function is now
      left as a *suggestion* for implementing support for mutations.
      -}
  \case
    GraphQL.UVLiteral x -> pure x
    GraphQL.UVSession -> pure $ ValueExpression $ ODBC.ByteStringValue $ toStrict $ J.encode sessionVariables
    GraphQL.UVParameter _ RQL.ColumnValue {..} -> pure $ ValueExpression cvValue
    GraphQL.UVSessionVar typ sessionVariable -> do
      value <-
        getSessionVariableValue sessionVariable sessionVariables
          `onNothing` throw400 NotFound ("missing session variable: " <>> sessionVariable)
      -- See https://github.com/fpco/odbc/pull/34#issuecomment-812223147
      -- We first cast to nvarchar(max) because casting from ntext is not supported
      CastExpression (CastExpression (ValueExpression $ ODBC.TextValue value) "nvarchar(max)")
        <$> case typ of
          CollectableTypeScalar baseTy ->
            pure (scalarTypeDBName baseTy)
          CollectableTypeArray {} ->
            throw400 NotSupported "Array types are currently not supported in MS SQL Server"

planSubscription ::
  MonadError QErr m =>
  OMap.InsOrdHashMap G.Name (QueryDB 'MSSQL Void (GraphQL.UnpreparedValue 'MSSQL)) ->
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
  selectMap <-
    runValidate (runFromIr (traverse fromRootField rootFieldMap))
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
--     (runValidate (runFromIr (traverse fromRootField rootFieldMap)))
-- pure (collapseMap selectMap)

--------------------------------------------------------------------------------
-- Converting a root field into a T-SQL select statement

-- | Collapse a set of selects into a single select that projects
-- these as subselects.
collapseMap ::
  OMap.InsOrdHashMap G.Name Select ->
  Reselect
collapseMap selects =
  Reselect
    { reselectFor =
        JsonFor ForJson {jsonCardinality = JsonSingleton, jsonRoot = NoRoot},
      reselectWhere = Where mempty,
      reselectProjections =
        map projectSelect (OMap.toList selects)
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
  { positionalArguments :: ![RQL.ColumnValue 'MSSQL],
    namedArguments :: !(HashMap G.Name (RQL.ColumnValue 'MSSQL)),
    sessionVariables :: !(Set.HashSet SessionVariable)
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
  GraphQL.UnpreparedValue 'MSSQL ->
  m Expression
prepareValueSubscription globalVariables =
  \case
    GraphQL.UVLiteral x -> pure x
    GraphQL.UVSession -> do
      modify' (\s -> s {sessionVariables = sessionVariables s <> globalVariables})
      pure $ resultVarExp (RootPath `FieldPath` "session")
    GraphQL.UVSessionVar _typ text -> do
      if Set.member text globalVariables
        then pure ()
        else
          throw400
            NotFound
            ("missing session variable: " <>> sessionVariableToText text)
      modify' (\s -> s {sessionVariables = text `Set.insert` sessionVariables s})
      pure $ resultVarExp (sessionDot $ toTxt text)
    GraphQL.UVParameter mVariableInfo columnValue ->
      case fmap GraphQL.getName mVariableInfo of
        Nothing -> do
          currentIndex <- toInteger . length <$> gets positionalArguments
          modify'
            ( \s ->
                s
                  { positionalArguments = positionalArguments s <> [columnValue]
                  }
            )
          pure (resultVarExp (syntheticIx currentIndex))
        Just name -> do
          modify
            ( \s ->
                s
                  { namedArguments =
                      HM.insert name columnValue (namedArguments s)
                  }
            )
          pure $ resultVarExp (queryDot $ G.unName name)
  where
    resultVarExp :: JsonPath -> Expression
    resultVarExp =
      JsonValueExpression $
        ColumnExpression $
          FieldName
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
