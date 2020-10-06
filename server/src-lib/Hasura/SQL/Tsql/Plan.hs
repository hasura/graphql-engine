-- | Planning T-SQL queries and subscriptions.

module Hasura.SQL.Tsql.Plan
  ( rootFieldToSelect
  , prepareValueNoPlan
  , prepareValueMultiplex
  ) where

import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Validate
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Strict.InsOrd as OMap
import           Data.Int
import qualified Data.List.NonEmpty as NE
import           Data.Void
import qualified Database.ODBC.SQLServer as Odbc
import qualified Hasura.GraphQL.Context as Graphql
import qualified Hasura.GraphQL.Parser.Column as Graphql
import qualified Hasura.GraphQL.Parser.Schema as PS
import qualified Hasura.RQL.DML.Select as DS
import qualified Hasura.SQL.DML as Sql
import qualified Hasura.SQL.Tsql.FromIr as Tsql
import           Hasura.SQL.Tsql.Types as Tsql
import qualified Hasura.SQL.Types as Sql
import qualified Hasura.SQL.Value as Sql
import qualified Language.GraphQL.Draft.Syntax as G
import           Prelude

--------------------------------------------------------------------------------
-- Converting a root field into a T-SQL select statement

-- TODO: Use 'alias'.
rootFieldToSelect ::
     Sql.Alias
  -> Graphql.RootField (Graphql.QueryDB Void) Void Void Void
  -> Tsql.FromIr Select
rootFieldToSelect _alias =
  \case
    Graphql.RFDB (Graphql.QDBPrimaryKey s) ->
      Tsql.mkSQLSelect DS.JASSingleObject s
    Graphql.RFDB (Graphql.QDBSimple s) -> Tsql.mkSQLSelect DS.JASMultipleRows s
    Graphql.RFDB (Graphql.QDBAggregation s) -> Tsql.fromSelectAggregate s
    Graphql.RFDB (Graphql.QDBConnection {}) ->
      refute (pure Tsql.ConnectionsNotSupported)

--------------------------------------------------------------------------------
-- Session variables

globalSessionExpression :: Tsql.Expression
globalSessionExpression =
  ValueExpression (Odbc.TextValue "TODO: sessionExpression")

envObjectExpression :: Tsql.Expression
envObjectExpression =
  ValueExpression (Odbc.TextValue "TODO: envObjectExpression")

--------------------------------------------------------------------------------
-- Resolving values

data PrepareError
  = UVLiteralNotSupported
  | SessionVarNotSupported
  | UnsupportedPgType Sql.PGScalarValue
  deriving (Show, Eq)

data PrepareState = PrepareState
  { positionalArguments :: !Integer
  , namedArguments :: !(HashMap G.Name Graphql.PGColumnValue)
  }

-- | Prepare a value without any query planning; we just execute the
-- query with the values embedded.
prepareValueNoPlan :: Graphql.UnpreparedValue -> Either PrepareError Tsql.Expression
prepareValueNoPlan =
  \case
    Graphql.UVLiteral (_ :: Sql.SQLExp) -> Left UVLiteralNotSupported
    Graphql.UVSession -> pure (JsonQueryExpression globalSessionExpression Nothing)
    Graphql.UVSessionVar _typ _text -> Left SessionVarNotSupported
    Graphql.UVParameter Graphql.PGColumnValue {pcvValue = Sql.WithScalarType {pstValue}} _mVariableInfo ->
      case fromPgScalarValue pstValue of
        Nothing -> Left (UnsupportedPgType pstValue)
        Just value -> pure (ValueExpression value)

-- | Prepare a value for multiplexed queries.
prepareValueMultiplex ::
     Graphql.UnpreparedValue
  -> StateT PrepareState (Either PrepareError) Tsql.Expression
prepareValueMultiplex =
  \case
    Graphql.UVLiteral (_ :: Sql.SQLExp) -> lift (Left UVLiteralNotSupported)
    Graphql.UVSession ->
      pure (JsonQueryExpression globalSessionExpression Nothing)
    Graphql.UVSessionVar _typ _text -> lift (Left SessionVarNotSupported)
    Graphql.UVParameter pgcolumnvalue mVariableInfo ->
      case fmap PS.getName mVariableInfo of
        Nothing -> do
          index <- gets positionalArguments
          modify' (\s -> s {positionalArguments = index + 1})
          pure
            (JsonQueryExpression
               envObjectExpression
               (Just (RootPath `FieldPath` "synthetic" `IndexPath` index)))
        Just name -> do
          modify
            (\s ->
               s
                 { namedArguments =
                     HM.insert name pgcolumnvalue (namedArguments s)
                 })
          pure
            (JsonQueryExpression
               envObjectExpression
               (Just
                  (RootPath `FieldPath` "query" `FieldPath` G.unName name)))

--------------------------------------------------------------------------------
-- Producing the correct SQL-level list comprehension to multiplex a query

-- Problem description:
--
-- Generate a query that repeats the same query N times but with
-- certain slots replaced:
--
-- [ Select x y | (x,y) <- [..] ]
--

multiplexQueries ::
     OMap.InsOrdHashMap G.Name (Graphql.RootField (Graphql.QueryDB Void) Void Void Void)
  -> Tsql.Select
multiplexQueries _ =
  undefined

multiplexedRootField :: Tsql.Select -> Tsql.Select
multiplexedRootField originalSelect =
  Select
    { selectTop = NoTop
    , selectProjections =
        NE.fromList
          [ FieldNameProjection
              Aliased
                { aliasedThing =
                    FieldName
                      {fieldNameEntity = rowAlias, fieldName = "result_id"}
                , aliasedAlias = "result_id"
                }
          , FieldNameProjection
              Aliased
                { aliasedThing =
                    FieldName
                      {fieldNameEntity = resultAlias, fieldName = Tsql.jsonFieldName}
                , aliasedAlias = "result"
                }
          ]
    , selectFrom =
        FromOpenJson
          Aliased
            { aliasedThing =
                OpenJson
                  { openJsonExpression = ValueExpression (Odbc.TextValue "{..}") -- TODO: this comes from somewhere...
                  , openJsonWith =
                      NE.fromList
                        [IntField "result_id", JsonField "result_vars"]
                  }
            , aliasedAlias = rowAlias
            }
    , selectJoins =
        [ Join
            { joinSource = JoinSelect originalSelect
            , joinJoinAlias =
                JoinAlias
                  { joinAliasEntity = resultAlias
                  , joinAliasField = Just Tsql.jsonFieldName
                  }
            }
        ]
    , selectWhere = Where mempty
    , selectFor = JsonFor JsonArray
    , selectOrderBy = Nothing
    , selectOffset = Nothing
    }
  where
    resultAlias = "result"
    rowAlias = "row"

--------------------------------------------------------------------------------
-- PG compat

-- | Convert from PG values to ODBC. Later, this shouldn't be
-- necessary; the value should come in as an ODBC value already.
fromPgScalarValue :: Sql.PGScalarValue -> Maybe Odbc.Value
fromPgScalarValue =
  \case
    Sql.PGValInteger i32 ->
      pure (Odbc.IntValue ((fromIntegral :: Int32 -> Int) i32))
    Sql.PGValSmallInt i16 ->
      pure (Odbc.IntValue ((fromIntegral :: Int16 -> Int) i16))
    Sql.PGValBigInt i64 ->
      pure (Odbc.IntValue ((fromIntegral :: Int64 -> Int) i64))
    Sql.PGValFloat float -> pure (Odbc.FloatValue float)
    Sql.PGValDouble double -> pure (Odbc.DoubleValue double)
    Sql.PGNull _pgscalartype -> pure Odbc.NullValue
    Sql.PGValBoolean bool -> pure (Odbc.BoolValue bool)
    Sql.PGValVarchar text -> pure (Odbc.TextValue text)
    Sql.PGValText text -> pure (Odbc.TextValue text)
    Sql.PGValDate day -> pure (Odbc.DayValue day)
    Sql.PGValTimeStamp localtime -> pure (Odbc.LocalTimeValue localtime)
     -- For these, see Datetime2 in Database.ODBC.SQLServer.
    Sql.PGValTimeStampTZ _utctime -> Nothing -- TODO: Sql.PGValTimeStampTZ utctime
    Sql.PGValTimeTZ _zonedtimeofday -> Nothing -- TODO: Sql.PGValTimeTZ zonedtimeofday
    Sql.PGValNumeric _scientific -> Nothing -- TODO: Sql.PGValNumeric scientific
    Sql.PGValMoney _scientific -> Nothing -- TODO: Sql.PGValMoney scientific
    Sql.PGValChar _char -> Nothing -- TODO: Sql.PGValChar char
    Sql.PGValCitext _text -> Nothing -- TODO: Sql.PGValCitext text
     -- I'm not sure whether it's fine to encode as string, because
     -- that's what SQL Server treats JSON as. But wrapping it in a
     -- JsonQueryExpression might help.
    Sql.PGValJSON _json -> Nothing -- TODO: Sql.PGValJSON json
    Sql.PGValJSONB _jsonb -> Nothing -- TODO: Sql.PGValJSONB jsonb
    Sql.PGValGeo _geometrywithcrs -> Nothing -- TODO: Sql.PGValGeo geometrywithcrs
    Sql.PGValRaster _rasterwkb -> Nothing -- TODO: Sql.PGValRaster rasterwkb
     -- There is a UUID type in SQL Server, but it needs research.
    Sql.PGValUUID _uuid -> Nothing -- TODO: Sql.PGValUUID uuid
    Sql.PGValUnknown _text -> Nothing -- TODO: Sql.PGValUnknown text
