-- | Planning T-SQL queries and subscriptions.

module Hasura.Backends.MSSQL.Plan
  ( planNoPlan
  , planNoPlanMap
  , planMultiplex
  ) where

import           Hasura.Prelude                     hiding (first)

import qualified Data.HashMap.Strict                as HM
import qualified Data.HashMap.Strict.InsOrd         as OMap
import qualified Data.List.NonEmpty                 as NE
import qualified Data.Text                          as T
import qualified Database.ODBC.SQLServer            as Odbc
import qualified Language.GraphQL.Draft.Syntax      as G

import           Control.Monad.Validate
import           Data.Bifunctor                     (first)


import qualified Hasura.GraphQL.Execute.Query       as EQ
import qualified Hasura.GraphQL.Parser              as Graphql
import           Hasura.Backends.MSSQL.FromIr       as Tsql
import           Hasura.Backends.MSSQL.Types        as Tsql
import           Hasura.Prelude ()
import qualified Hasura.RQL.Types.Action            as RQL
import           Hasura.GraphQL.Context
import           Hasura.SQL.Backend

type SubscriptionRootFieldMSSQL v = RootField (QueryDB 'MSSQL v) Void (RQL.AnnActionAsyncQuery 'MSSQL v) Void


-- --------------------------------------------------------------------------------
-- -- Top-level planner

-- -- | Plan a query without prepare/exec.
planNoPlan ::
     SubscriptionRootFieldMSSQL Graphql.UnpreparedValue
  -> Either PrepareError Select
planNoPlan unpreparedRoot = do
  rootField <- EQ.traverseQueryRootField prepareValueNoPlan unpreparedRoot
  select <-
    first
      FromIrError
      (runValidate (Tsql.runFromIr (Tsql.fromRootField rootField)))
  pure
    select
      { selectFor =
          case selectFor select of
            NoFor           -> NoFor
            JsonFor forJson -> JsonFor forJson {jsonRoot = Root "root"}
      }

planMultiplex ::
     OMap.InsOrdHashMap G.Name (SubscriptionRootFieldMSSQL Graphql.UnpreparedValue)
  -> Either PrepareError Select
planMultiplex unpreparedMap = do
  rootFieldMap <-
    evalStateT
      (traverse
         (EQ.traverseQueryRootField prepareValueMultiplex)
         unpreparedMap)
      emptyPrepareState
  selectMap <-
    first
      FromIrError
      (runValidate (Tsql.runFromIr (traverse Tsql.fromRootField rootFieldMap)))
  pure (multiplexRootReselect (collapseMap selectMap))

-- | Plan a query without prepare/exec.
planNoPlanMap ::
     OMap.InsOrdHashMap G.Name (SubscriptionRootFieldMSSQL Graphql.UnpreparedValue)
  -> Either PrepareError Reselect
planNoPlanMap unpreparedMap = do
  rootFieldMap <-
    traverse (EQ.traverseQueryRootField prepareValueNoPlan) unpreparedMap
  selectMap <-
    first
      FromIrError
      (runValidate (Tsql.runFromIr (traverse Tsql.fromRootField rootFieldMap)))
  pure (collapseMap selectMap)

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
        NE.fromList (map projectSelect (OMap.toList selects))
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

globalSessionExpression :: Tsql.Expression
globalSessionExpression =
  ValueExpression (Odbc.TextValue "TODO: sessionExpression")

-- TODO: real env object.
envObjectExpression :: Tsql.Expression
envObjectExpression =
  ValueExpression (Odbc.TextValue "[{\"result_id\":1,\"result_vars\":{\"synthetic\":[10]}}]")

--------------------------------------------------------------------------------
-- Resolving values

data PrepareError
  = {-UVLiteralNotSupported-}
   SessionVarNotSupported
  -- | UnsupportedPgType Odbc.Value -- PG.PGScalarValue
  | FromIrError (NonEmpty Tsql.Error)
  -- deriving (Show, Eq)

data PrepareState = PrepareState
  { positionalArguments :: !Integer
  , namedArguments      :: !(HashMap G.Name Graphql.PGColumnValue)
  }

emptyPrepareState :: PrepareState
emptyPrepareState =
  PrepareState {positionalArguments = 0, namedArguments = mempty}

-- | Prepare a value without any query planning; we just execute the
-- query with the values embedded.
prepareValueNoPlan :: Graphql.UnpreparedValue -> Either PrepareError Tsql.Expression
prepareValueNoPlan =
  \case
    -- FIXME: Cannot compile this until there is a generic literal type.
    Graphql.UVLiteral {} -> undefined
    {-Graphql.UVLiteral (_ :: Expression {-PG.SQLExp-}) -> Left UVLiteralNotSupported-}
    Graphql.UVSession -> pure (JsonQueryExpression globalSessionExpression)
    Graphql.UVSessionVar _typ _text -> Left SessionVarNotSupported
    -- FIXME: Cannot compile this until there is a generic "ColumnValue" type.
    Graphql.UVParameter {} -> undefined
    {-Graphql.UVParameter Graphql.PGColumnValue {pcvValue = PG.WithScalarType {pstValue}} _mVariableInfo ->
      case fromPgScalarValue pstValue of
        Nothing    -> Left (UnsupportedPgType pstValue)
        Just value -> pure (ValueExpression value)-}

-- | Prepare a value for multiplexed queries.
prepareValueMultiplex ::
     Graphql.UnpreparedValue
  -> StateT PrepareState (Either PrepareError) Tsql.Expression
prepareValueMultiplex =
  \case
    -- FIXME: Cannot compile this until there is a generic literal type.
    Graphql.UVLiteral {} -> undefined
    {-Graphql.UVLiteral (_ :: Expression {-PG.SQLExp-}) -> lift (Left UVLiteralNotSupported)-}
    Graphql.UVSession ->
      pure (JsonQueryExpression globalSessionExpression)
    Graphql.UVSessionVar _typ _text -> lift (Left SessionVarNotSupported)
    Graphql.UVParameter pgcolumnvalue mVariableInfo ->
      case fmap Graphql.getName mVariableInfo of
        Nothing -> do
          index <- gets positionalArguments
          modify' (\s -> s {positionalArguments = index + 1})
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
                     HM.insert name pgcolumnvalue (namedArguments s)
                 })
          pure
            (JsonValueExpression
               envObjectExpression
               (RootPath `FieldPath` "query" `FieldPath` G.unName name))

--------------------------------------------------------------------------------
-- Producing the correct SQL-level list comprehension to multiplex a query

-- Problem description:
--
-- Generate a query that repeats the same query N times but with
-- certain slots replaced:
--
-- [ Select x y | (x,y) <- [..] ]
--

multiplexRootReselect :: Tsql.Reselect -> Tsql.Select
multiplexRootReselect rootReselect =
  Select
    { selectTop = NoTop
    , selectProjections =
        NE.fromList
          [ FieldNameProjection
              Aliased
                { aliasedThing =
                    FieldName
                      {fieldNameEntity = rowAlias, fieldName = resultIdAlias}
                , aliasedAlias = resultIdAlias
                }
          , ExpressionProjection
              Aliased
                { aliasedThing =
                    JsonQueryExpression
                      (ColumnExpression
                         (FieldName
                            { fieldNameEntity = resultAlias
                            , fieldName = Tsql.jsonFieldName
                            }))
                , aliasedAlias = resultAlias
                }
          ]
    , selectFrom =
        FromOpenJson
          Aliased
            { aliasedThing =
                OpenJson
                  { openJsonExpression = envObjectExpression
                  , openJsonWith =
                      NE.fromList
                        [IntField resultIdAlias, JsonField resultVarsAlias]
                  }
            , aliasedAlias = rowAlias
            }
    , selectJoins =
        [ Join
            { joinSource = JoinReselect rootReselect
            , joinJoinAlias =
                JoinAlias
                  { joinAliasEntity = resultAlias
                  , joinAliasField = Just Tsql.jsonFieldName
                  }
            }
        ]
    , selectWhere = Where mempty
    , selectFor =
        JsonFor ForJson {jsonCardinality = JsonArray, jsonRoot = NoRoot}
    , selectOrderBy = Nothing
    , selectOffset = Nothing
    }

resultIdAlias :: T.Text
resultIdAlias = "result_id"

resultVarsAlias :: T.Text
resultVarsAlias = "result_vars"

resultAlias :: T.Text
resultAlias = "result"

rowAlias :: T.Text
rowAlias = "row"

--------------------------------------------------------------------------------
-- PG compat

-- No longer needed

-- -- | Convert from PG values to ODBC. Later, this shouldn't be
-- -- necessary; the value should come in as an ODBC value already.
-- fromPgScalarValue :: PG.PGScalarValue -> Maybe Odbc.Value
-- fromPgScalarValue =
--   \case
--     PG.PGValInteger i32 ->
--       pure (Odbc.IntValue ((fromIntegral :: Int32 -> Int) i32))
--     PG.PGValSmallInt i16 ->
--       pure (Odbc.IntValue ((fromIntegral :: Int16 -> Int) i16))
--     PG.PGValBigInt i64 ->
--       pure (Odbc.IntValue ((fromIntegral :: Int64 -> Int) i64))
--     PG.PGValFloat float -> pure (Odbc.FloatValue float)
--     PG.PGValDouble double -> pure (Odbc.DoubleValue double)
--     PG.PGNull _pgscalartype -> pure Odbc.NullValue
--     PG.PGValBoolean boolean -> pure (Odbc.BoolValue boolean)
--     PG.PGValVarchar text -> pure (Odbc.TextValue text)
--     PG.PGValText text -> pure (Odbc.TextValue text)
--     PG.PGValDate day -> pure (Odbc.DayValue day)
--     PG.PGValTimeStamp localtime -> pure (Odbc.LocalTimeValue localtime)
--      -- For these, see Datetime2 in Database.ODBC.SQLServer.
--     PG.PGValTimeStampTZ _utctime -> Nothing -- TODO: PG.PGValTimeStampTZ utctime
--     PG.PGValTimeTZ _zonedtimeofday -> Nothing -- TODO: PG.PGValTimeTZ zonedtimeofday
--     PG.PGValNumeric _scientific -> Nothing -- TODO: PG.PGValNumeric scientific
--     PG.PGValMoney _scientific -> Nothing -- TODO: PG.PGValMoney scientific
--     PG.PGValChar _char -> Nothing -- TODO: PG.PGValChar char
--     PG.PGValCitext _text -> Nothing -- TODO: PG.PGValCitext text
--      -- I'm not sure whether it's fine to encode as string, because
--      -- that's what SQL Server treats JSON as. But wrapping it in a
--      -- JsonQueryExpression might help.
--     PG.PGValJSON _json -> Nothing -- TODO: PG.PGValJSON json
--     PG.PGValJSONB _jsonb -> Nothing -- TODO: PG.PGValJSONB jsonb
--     PG.PGValGeo _geometrywithcrs -> Nothing -- TODO: PG.PGValGeo geometrywithcrs
--     PG.PGValRaster _rasterwkb -> Nothing -- TODO: PG.PGValRaster rasterwkb
--      -- There is a UUID type in SQL Server, but it needs research.
--     PG.PGValUUID _uuid -> Nothing -- TODO: PG.PGValUUID uuid
--     PG.PGValUnknown _text -> Nothing -- TODO: PG.PGValUnknown text
