-- | Translate from the DML to the BigQuery dialect.

module Hasura.Backends.BigQuery.FromIr
  ( fromSelectRows
  , mkSQLSelect
  , fromRootField
  , fromSelectAggregate
  , Error(..)
  , runFromIr
  , FromIr
  , FromIrConfig(..)
  , defaultFromIrConfig
  , bigQuerySourceConfigToFromIrConfig
  , Top(..) -- Re-export for FromIrConfig.
  ) where

import           Hasura.Backends.BigQuery.Source          (BigQuerySourceConfig (..))
import           Hasura.Prelude

import qualified Data.HashMap.Strict                      as HM
import qualified Data.List.NonEmpty                       as NE
import qualified Data.Map.Strict                          as M
import qualified Data.Text                                as T

import           Control.Monad.Validate
import           Data.Map.Strict                          (Map)
import           Data.Proxy

import qualified Hasura.RQL.IR                            as Ir
import qualified Hasura.RQL.Types.Column                  as Rql
import qualified Hasura.RQL.Types.Common                  as Rql
import qualified Hasura.RQL.Types.Relationship            as Rql

import           Hasura.Backends.BigQuery.Instances.Types ()
import           Hasura.Backends.BigQuery.Types           as BigQuery
import           Hasura.SQL.Backend


--------------------------------------------------------------------------------
-- Types

-- | Most of these errors should be checked for legitimacy.
data Error
  = FromTypeUnsupported (Ir.SelectFromG 'BigQuery Expression)
  | NoOrderSpecifiedInOrderBy
  | MalformedAgg
  | FieldTypeUnsupportedForNow (Ir.AnnFieldG 'BigQuery (Const Void) Expression)
  | AggTypeUnsupportedForNow (Ir.TableAggregateFieldG 'BigQuery (Const Void) Expression)
  | NodesUnsupportedForNow (Ir.TableAggregateFieldG 'BigQuery (Const Void) Expression)
  | NoProjectionFields
  | NoAggregatesMustBeABug
  | UnsupportedArraySelect (Ir.ArraySelectG 'BigQuery (Const Void) Expression)
  | UnsupportedOpExpG (Ir.OpExpG 'BigQuery Expression)
  | UnsupportedSQLExp Expression
  | UnsupportedDistinctOn
  | InvalidIntegerishSql Expression
  | DistinctIsn'tSupported
  | ConnectionsNotSupported
  | ActionsNotSupported

instance Show Error where
  show =
    \case
      FromTypeUnsupported {}        -> "FromTypeUnsupported"
      NoOrderSpecifiedInOrderBy {}  -> "NoOrderSpecifiedInOrderBy"
      MalformedAgg {}               -> "MalformedAgg"
      FieldTypeUnsupportedForNow {} -> "FieldTypeUnsupportedForNow"
      AggTypeUnsupportedForNow {}   -> "AggTypeUnsupportedForNow"
      NodesUnsupportedForNow {}     -> "NodesUnsupportedForNow"
      NoProjectionFields {}         -> "NoProjectionFields"
      NoAggregatesMustBeABug {}     -> "NoAggregatesMustBeABug"
      UnsupportedArraySelect {}     -> "UnsupportedArraySelect"
      UnsupportedOpExpG {}          -> "UnsupportedOpExpG"
      UnsupportedSQLExp {}          -> "UnsupportedSQLExp"
      UnsupportedDistinctOn {}      -> "UnsupportedDistinctOn"
      InvalidIntegerishSql {}       -> "InvalidIntegerishSql"
      DistinctIsn'tSupported {}     -> "DistinctIsn'tSupported"
      ConnectionsNotSupported {}    -> "ConnectionsNotSupported"
      ActionsNotSupported {}        -> "ActionsNotSupported"

-- | The base monad used throughout this module for all conversion
-- functions.
--
-- It's a Validate, so it'll continue going when it encounters errors
-- to accumulate as many as possible.
--
-- It also contains a mapping from entity prefixes to counters. So if
-- my prefix is "table" then there'll be a counter that lets me
-- generate table1, table2, etc. Same for any other prefix needed
-- (e.g. names for joins).
--
-- A ReaderT is used around this in most of the module too, for
-- setting the current entity that a given field name refers to. See
-- @fromPGCol@.
newtype FromIr a = FromIr
  { unFromIr :: ReaderT FromIrReader (StateT FromIrState (Validate (NonEmpty Error))) a
  } deriving (Functor, Applicative, Monad, MonadValidate (NonEmpty Error))

data FromIrState = FromIrState
  { indices :: !(Map Text Int)
  }

data FromIrReader = FromIrReader
  { config :: !FromIrConfig
  }

-- | Config values for the from-IR translator.
data FromIrConfig = FromIrConfig
  { globalSelectLimit :: !Top
    -- ^ Applies globally to all selects, and may be reduced to
    -- something even smaller by permission/user args.
  }

-- | A default config.
defaultFromIrConfig :: FromIrConfig
defaultFromIrConfig = FromIrConfig {globalSelectLimit = NoTop}

data StringifyNumbers
  = StringifyNumbers
  | LeaveNumbersAlone
  deriving (Eq)

--------------------------------------------------------------------------------
-- Runners

runFromIr :: FromIrConfig -> FromIr a -> Validate (NonEmpty Error) a
runFromIr config fromIr =
  evalStateT
    (runReaderT (unFromIr fromIr) (FromIrReader {config}))
    (FromIrState {indices = mempty})

bigQuerySourceConfigToFromIrConfig :: BigQuerySourceConfig -> FromIrConfig
bigQuerySourceConfigToFromIrConfig BigQuerySourceConfig {_scGlobalSelectLimit} =
  FromIrConfig {globalSelectLimit = Top _scGlobalSelectLimit}

--------------------------------------------------------------------------------
-- Similar rendition of old API

-- | Here is where we apply a top-level annotation to the select to
-- indicate to the data loader that this select ought to produce a
-- single object or an array.
mkSQLSelect
  :: Rql.JsonAggSelect
  -> Ir.AnnSelectG 'BigQuery (Const Void) (Ir.AnnFieldG 'BigQuery (Const Void)) Expression
  -> FromIr BigQuery.Select
mkSQLSelect jsonAggSelect annSimpleSel = do
  select <- fromSelectRows annSimpleSel
  pure
    (select
       { selectCardinality =
           case jsonAggSelect of
             Rql.JASMultipleRows -> Many
             Rql.JASSingleObject -> One
       })

-- | Convert from the IR database query into a select.
fromRootField :: Ir.QueryDB 'BigQuery (Const Void) Expression -> FromIr Select
fromRootField =
  \case
    (Ir.QDBSingleRow s)    -> mkSQLSelect Rql.JASSingleObject s
    (Ir.QDBMultipleRows s) -> mkSQLSelect Rql.JASMultipleRows s
    (Ir.QDBAggregation s)  -> fromSelectAggregate Nothing s
    (Ir.QDBConnection _)   -> refute $ pure ConnectionsNotSupported

--------------------------------------------------------------------------------
-- Top-level exported functions

fromSelectRows :: Ir.AnnSelectG 'BigQuery (Const Void) (Ir.AnnFieldG 'BigQuery (Const Void)) Expression -> FromIr BigQuery.Select
fromSelectRows annSelectG = do
  selectFrom <-
    case from of
      Ir.FromTable qualifiedObject -> fromQualifiedTable qualifiedObject
      _                            -> refute (pure (FromTypeUnsupported from))
  Args { argsOrderBy
       , argsWhere
       , argsJoins
       , argsTop
       , argsDistinct = Proxy
       , argsOffset
       , argsExistingJoins
       } <- runReaderT (fromSelectArgsG args) (fromAlias selectFrom)
  fieldSources <-
    runReaderT
      (traverse (fromAnnFieldsG argsExistingJoins stringifyNumbers) fields)
      (fromAlias selectFrom)
  filterExpression <-
    runReaderT (fromAnnBoolExp permFilter) (fromAlias selectFrom)
  selectProjections <-
    NE.nonEmpty (concatMap (toList . fieldSourceProjections) fieldSources)
      `onNothing` refute (pure NoProjectionFields)
  globalTop <- getGlobalTop
  pure
    Select
      {selectCardinality = Many, selectFinalWantedFields = pure (fieldTextNames fields),  selectGroupBy = mempty
      , selectOrderBy = argsOrderBy
      -- We DO APPLY the global top here, because this pulls down all rows.
      , selectTop = globalTop <> permissionBasedTop <> argsTop
      , selectProjections
      , selectFrom
      , selectJoins = argsJoins <> mapMaybe fieldSourceJoin fieldSources
      , selectWhere = argsWhere <> Where [filterExpression]

      , selectOffset = argsOffset
      }
  where
    Ir.AnnSelectG { _asnFields = fields
                  , _asnFrom = from
                  , _asnPerm = perm
                  , _asnArgs = args
                  , _asnStrfyNum = num
                  } = annSelectG
    Ir.TablePerm {_tpLimit = mPermLimit, _tpFilter = permFilter} = perm
    permissionBasedTop =
      maybe NoTop Top mPermLimit
    stringifyNumbers =
      if num
        then StringifyNumbers
        else LeaveNumbersAlone

fromSelectAggregate
  :: Maybe (EntityAlias, HashMap ColumnName ColumnName)
  -> Ir.AnnSelectG 'BigQuery (Const Void) (Ir.TableAggregateFieldG 'BigQuery (Const Void)) Expression
  -> FromIr BigQuery.Select
fromSelectAggregate minnerJoinFields annSelectG = do
  selectFrom <-
    case from of
      Ir.FromTable qualifiedObject -> fromQualifiedTable qualifiedObject
      _                            -> refute (pure (FromTypeUnsupported from))
  args'@Args {argsWhere, argsOrderBy, argsJoins, argsTop, argsOffset, argsDistinct = Proxy} <-
    runReaderT (fromSelectArgsG args) (fromAlias selectFrom)
  filterExpression <-
    runReaderT (fromAnnBoolExp permFilter) (fromAlias selectFrom)
  mforeignKeyConditions <-
    for minnerJoinFields $ \(entityAlias, mapping) ->
      runReaderT
        (fromMappingFieldNames (fromAlias selectFrom) mapping)
        entityAlias
  fieldSources <-
    runReaderT
      (traverse
         (fromTableAggregateFieldG
            args'
            permissionBasedTop

            stringifyNumbers)
         fields)
      (fromAlias selectFrom)
  selectProjections <-
    onNothing
    (NE.nonEmpty
       (concatMap (toList . fieldSourceProjections) fieldSources))
    (refute (pure NoProjectionFields))
  indexAlias <- generateEntityAlias IndexTemplate
  pure
    Select
      { selectCardinality = One
      , selectFinalWantedFields = Nothing
      , selectGroupBy = mempty
      , selectProjections
      , selectTop = NoTop
      , selectFrom =
          FromSelect
            (Aliased
               { aliasedThing =
                   Select
                     { selectProjections =
                         case mforeignKeyConditions of
                           Nothing -> pure StarProjection
                           Just innerJoinFields ->
                             pure StarProjection <>
                             -- We setup an index over every row in
                             -- the sub select.  Then if you look at
                             -- the outer Select, you can see we apply
                             -- a WHERE that uses this index for
                             -- LIMIT/OFFSET.
                             pure
                               (WindowProjection
                                  (Aliased
                                     { aliasedAlias = unEntityAlias indexAlias
                                     , aliasedThing =
                                         RowNumberOverPartitionBy
                                           -- The row numbers start from 1.
                                           (NE.fromList (map fst innerJoinFields))
                                           argsOrderBy
                                           -- Above: Having the order by
                                           -- in here ensures that the
                                           -- row numbers are ordered by
                                           -- this ordering. Below, we
                                           -- order again for the
                                           -- general row order. Both
                                           -- are needed!
                                     }))
                     , selectFrom
                     , selectJoins =
                         argsJoins <> mapMaybe fieldSourceJoin fieldSources
                     , selectWhere = argsWhere <> (Where [filterExpression])
                     , selectOrderBy = argsOrderBy
                     -- Above: This is important to have here, because
                     -- offset/top apply AFTER ordering is applied, so
                     -- you can't put an order by in afterwards in a
                     -- parent query. Therefore be careful about
                     -- putting this elsewhere.
                     , selectFinalWantedFields = Nothing
                     , selectCardinality = Many
                     , selectTop = maybe argsTop (const NoTop) mforeignKeyConditions
                     , selectOffset = maybe argsOffset (const Nothing) mforeignKeyConditions
                     , selectGroupBy = mempty
                     }
               , aliasedAlias = entityAliasText (fromAlias selectFrom)
               })
      , selectJoins = mempty
      , selectWhere =
          case mforeignKeyConditions of
            Nothing -> mempty
            Just {} ->
              let offset =
                    case argsOffset of
                      Nothing -> mempty
                      Just offset' ->
                        Where
                          -- Apply an offset using the row_number from above.
                          [ OpExpression
                              MoreOp
                              (ColumnExpression
                                 FieldName
                                   { fieldNameEntity =
                                       coerce (fromAlias selectFrom)
                                   , fieldName = unEntityAlias indexAlias
                                   })
                              offset'
                          ]
                  limit =
                    case argsTop of
                      NoTop -> mempty
                      Top limit' ->
                        Where
                          -- Apply a limit using the row_number from above.
                          [ OpExpression
                              LessOp
                              (ColumnExpression
                                 FieldName
                                   { fieldNameEntity =
                                       coerce (fromAlias selectFrom)
                                   , fieldName = unEntityAlias indexAlias
                                   })
                              (ValueExpression . IntegerValue . Int64 . tshow $
                               limit' + 1 -- Because the row_number() indexing starts at 1.
                               -- So idx<l+1  means idx<2 where l = 1 i.e. "limit to 1 row".
                               )
                          ]
               in offset <> limit
      , selectOrderBy = Nothing
      , selectOffset = Nothing
      }
  where
    Ir.AnnSelectG { _asnFields = fields
                  , _asnFrom = from
                  , _asnPerm = perm
                  , _asnArgs = args
                  , _asnStrfyNum = num -- TODO: Do we ignore this for aggregates?
                  } = annSelectG
    Ir.TablePerm {_tpLimit = mPermLimit, _tpFilter = permFilter} = perm
    permissionBasedTop =
      maybe NoTop Top mPermLimit
    stringifyNumbers =
      if num
        then StringifyNumbers
        else LeaveNumbersAlone

--------------------------------------------------------------------------------
-- GraphQL Args

data Args = Args
  { argsWhere         :: Where
  , argsOrderBy       :: Maybe (NonEmpty OrderBy)
  , argsJoins         :: [Join]
  , argsTop           :: Top
  , argsOffset        :: Maybe Expression
  , argsDistinct      :: Proxy (Maybe (NonEmpty FieldName))
  , argsExistingJoins :: Map TableName EntityAlias
  } deriving (Show)

data UnfurledJoin = UnfurledJoin
  { unfurledJoin             :: Join
  , unfurledObjectTableAlias :: Maybe (TableName, EntityAlias)
    -- ^ Recorded if we joined onto an object relation.
  } deriving (Show)

fromSelectArgsG :: Ir.SelectArgsG 'BigQuery Expression -> ReaderT EntityAlias FromIr Args
fromSelectArgsG selectArgsG = do
  let argsOffset = ValueExpression . IntegerValue . Int64 . tshow <$> moffset
  argsWhere <-
    maybe (pure mempty) (fmap (Where . pure) . fromAnnBoolExp) mannBoolExp
  argsTop <- maybe (pure mempty) (pure . Top) mlimit
  -- Not supported presently, per Vamshi:
  --
  -- > It is hardly used and we don't have to go to great lengths to support it.
  --
  -- But placeholdering the code so that when it's ready to be used,
  -- you can just drop the Proxy wrapper.
  argsDistinct <-
    case mdistinct of
      Nothing -> pure Proxy
      Just {} -> refute (pure DistinctIsn'tSupported)
  (argsOrderBy, joins) <-
    runWriterT (traverse fromAnnOrderByItemG (maybe [] toList orders))
  -- Any object-relation joins that we generated, we record their
  -- generated names into a mapping.
  let argsExistingJoins =
        M.fromList (mapMaybe unfurledObjectTableAlias (toList joins))
  pure
    Args
      { argsJoins = toList (fmap unfurledJoin joins)
      , argsOrderBy = NE.nonEmpty argsOrderBy
      , ..
      }
  where
    Ir.SelectArgs { _saWhere = mannBoolExp
                  , _saLimit = mlimit
                  , _saOffset = moffset
                  , _saDistinct = mdistinct
                  , _saOrderBy = orders
                  } = selectArgsG

-- | Produce a valid ORDER BY construct, telling about any joins
-- needed on the side.
fromAnnOrderByItemG ::
     Ir.AnnOrderByItemG 'BigQuery Expression -> WriterT (Seq UnfurledJoin) (ReaderT EntityAlias FromIr) OrderBy
fromAnnOrderByItemG Ir.OrderByItemG {obiType, obiColumn, obiNulls} = do
  orderByFieldName <- unfurlAnnOrderByElement obiColumn
  let morderByOrder =
        obiType
  let orderByNullsOrder =
        fromMaybe NullsAnyOrder obiNulls
  case morderByOrder of
    Just orderByOrder -> pure OrderBy {..}
    Nothing           -> refute (pure NoOrderSpecifiedInOrderBy)

-- | Unfurl the nested set of object relations (tell'd in the writer)
-- that are terminated by field name (Ir.AOCColumn and
-- Ir.AOCArrayAggregation).
unfurlAnnOrderByElement ::
     Ir.AnnOrderByElement 'BigQuery Expression -> WriterT (Seq UnfurledJoin) (ReaderT EntityAlias FromIr) FieldName
unfurlAnnOrderByElement =
  \case
    Ir.AOCColumn pgColumnInfo -> lift (fromPGColumnInfo pgColumnInfo)
    Ir.AOCObjectRelation Rql.RelInfo {riMapping = mapping, riRTable = tableName} annBoolExp annOrderByElementG -> do
      selectFrom <- lift (lift (fromQualifiedTable tableName))
      joinAliasEntity <-
        lift (lift (generateEntityAlias (ForOrderAlias (tableNameText tableName))))
      joinOn <- lift (fromMappingFieldNames joinAliasEntity mapping)
      whereExpression <-
        lift (local (const (fromAlias selectFrom)) (fromAnnBoolExp annBoolExp))
      tell
        (pure
           UnfurledJoin
             { unfurledJoin =
                 Join
                   { joinSource =
                       JoinSelect
                         Select
                           {selectCardinality = One, selectFinalWantedFields = Nothing,  selectGroupBy = mempty
                           , selectTop = NoTop
                           , selectProjections = NE.fromList [StarProjection]
                           , selectFrom
                           , selectJoins = []
                           , selectWhere = Where ([whereExpression])
                           , selectOrderBy = Nothing
                           , selectOffset = Nothing
                           }
                   , joinRightTable = fromAlias selectFrom
                   , joinAlias = joinAliasEntity
                   , joinOn
                   , joinProvenance = OrderByJoinProvenance
                   , joinFieldName = tableNameText tableName -- TODO: not needed.
                   , joinExtractPath = Nothing
                   }
             , unfurledObjectTableAlias = Just (tableName, joinAliasEntity)
             })
      local (const joinAliasEntity) (unfurlAnnOrderByElement annOrderByElementG)
    Ir.AOCArrayAggregation Rql.RelInfo {riMapping = mapping, riRTable = tableName} annBoolExp annAggregateOrderBy -> do
      selectFrom <- lift (lift (fromQualifiedTable tableName))
      let alias = aggFieldName
      joinAlias <-
        lift (lift (generateEntityAlias (ForOrderAlias (tableNameText tableName))))
      joinOn <- lift (fromMappingFieldNames joinAlias mapping)
      innerJoinFields <-
        lift (fromMappingFieldNames (fromAlias selectFrom) mapping)
      whereExpression <-
        lift (local (const (fromAlias selectFrom)) (fromAnnBoolExp annBoolExp))
      aggregate <-
        lift
          (local
             (const (fromAlias selectFrom))
             (case annAggregateOrderBy of
                Ir.AAOCount -> pure (CountAggregate StarCountable)
                Ir.AAOOp text pgColumnInfo -> do
                  fieldName <- fromPGColumnInfo pgColumnInfo
                  pure (OpAggregate text (ColumnExpression fieldName))))
      tell
        (pure
           (UnfurledJoin
              { unfurledJoin =
                  Join
                    { joinSource =
                        JoinSelect
                          Select
                            {selectCardinality = One, selectFinalWantedFields = Nothing,  selectTop = NoTop
                            , selectProjections =
                                AggregateProjection
                                  Aliased
                                    { aliasedThing = aggregate
                                    , aliasedAlias = alias
                                    } :|
                                -- These are group by'd below in selectGroupBy.
                                map
                                  (\(fieldName', _) ->
                                     FieldNameProjection
                                       Aliased
                                         { aliasedThing = fieldName'
                                         , aliasedAlias = fieldName fieldName'
                                         })
                                  innerJoinFields
                            , selectFrom
                            , selectJoins = []
                            , selectWhere = Where [whereExpression]
                            , selectOrderBy = Nothing
                            , selectOffset = Nothing
                            -- This group by corresponds to the field name projections above.
                            , selectGroupBy = map fst innerJoinFields
                            }
                    , joinRightTable = fromAlias selectFrom
                    , joinProvenance = OrderByJoinProvenance
                    , joinAlias = joinAlias
                    , joinOn
                    , joinFieldName = tableNameText tableName -- TODO: not needed.
                    , joinExtractPath = Nothing
                    }
              , unfurledObjectTableAlias = Nothing
              }))
      pure
        FieldName
          {fieldNameEntity = entityAliasText joinAlias, fieldName = alias}

--------------------------------------------------------------------------------
-- Conversion functions

tableNameText :: TableName -> Text
tableNameText (TableName {tableName=qname}) = qname

-- | This is really the start where you query the base table,
-- everything else is joins attached to it.
fromQualifiedTable :: TableName -> FromIr From
fromQualifiedTable (TableName {tableNameSchema=schemaName,tableName=qname}) = do
  alias <- generateEntityAlias (TableTemplate qname)
  pure
    (FromQualifiedTable
       (Aliased
          { aliasedThing =
              TableName {tableName = qname, tableNameSchema = schemaName}
          , aliasedAlias = entityAliasText alias
          }))

fromAnnBoolExp ::
     Ir.GBoolExp 'BigQuery (Ir.AnnBoolExpFld 'BigQuery Expression)
  -> ReaderT EntityAlias FromIr Expression
fromAnnBoolExp = traverse fromAnnBoolExpFld >=> fromGBoolExp

fromAnnBoolExpFld ::
     Ir.AnnBoolExpFld 'BigQuery Expression -> ReaderT EntityAlias FromIr Expression
fromAnnBoolExpFld =
  \case
    Ir.AVColumn pgColumnInfo opExpGs -> do
      expression <- fmap ColumnExpression (fromPGColumnInfo pgColumnInfo)
      expressions <- traverse (lift . fromOpExpG expression) opExpGs
      pure (AndExpression expressions)
    Ir.AVRelationship Rql.RelInfo {riMapping = mapping, riRTable = table} annBoolExp -> do
      selectFrom <- lift (fromQualifiedTable table)
      foreignKeyConditions <- fromMapping selectFrom mapping
      whereExpression <-
        local (const (fromAlias selectFrom)) (fromAnnBoolExp annBoolExp)
      pure
        (ExistsExpression
           Select
             {selectCardinality = One, selectFinalWantedFields = Nothing,  selectGroupBy = mempty
             , selectOrderBy = Nothing
             , selectProjections =
                 NE.fromList
                   [ ExpressionProjection
                       (Aliased
                          { aliasedThing = trueExpression
                          , aliasedAlias = existsFieldName
                          })
                   ]
             , selectFrom
             , selectJoins = mempty
             , selectWhere = Where (foreignKeyConditions <> [whereExpression])
             , selectTop = NoTop
             , selectOffset = Nothing
             })

fromPGColumnInfo :: Rql.ColumnInfo 'BigQuery -> ReaderT EntityAlias FromIr FieldName
fromPGColumnInfo Rql.ColumnInfo {pgiColumn = ColumnName pgCol} = do
  EntityAlias {entityAliasText} <- ask
  pure
    (FieldName
       {fieldName = pgCol, fieldNameEntity = entityAliasText})

fromGExists :: Ir.GExists 'BigQuery Expression -> ReaderT EntityAlias FromIr Select
fromGExists Ir.GExists {_geTable, _geWhere} = do
  selectFrom <- lift (fromQualifiedTable _geTable)
  whereExpression <-
    local (const (fromAlias selectFrom)) (fromGBoolExp _geWhere)
  pure
    Select
      {selectCardinality = One, selectFinalWantedFields = Nothing,  selectGroupBy = mempty
      , selectOrderBy = Nothing
      , selectProjections =
          NE.fromList
            [ ExpressionProjection
                (Aliased
                   { aliasedThing = trueExpression
                   , aliasedAlias = existsFieldName
                   })
            ]
      , selectFrom
      , selectJoins = mempty
      , selectWhere = Where [whereExpression]
      , selectTop = NoTop
      , selectOffset = Nothing
      }

--------------------------------------------------------------------------------
-- Sources of projected fields
--
-- Because in the IR, a field projected can be a foreign object, we
-- have to both generate a projection AND on the side generate a join.
--
-- So a @FieldSource@ couples the idea of the projected thing and the
-- source of it (via 'Aliased').

data FieldSource
  = ExpressionFieldSource (Aliased Expression)
  | JoinFieldSource (Aliased Join)
  | AggregateFieldSource Text (NonEmpty (Aliased Aggregate))
  | ArrayAggFieldSource (Aliased ArrayAgg)
  deriving (Eq, Show)



-- Example:
--
-- @
-- Track_aggregate {
--   aggregate {
--     count(columns: AlbumId)
--     foo: count(columns: AlbumId)
--     max {
--       AlbumId
--       TrackId
--     }
--   }
-- }
-- @
--
-- field =
-- @
-- TAFAgg
--   [ ( FieldName {getFieldNameTxt = "count"}
--     , AFCount (CTSimple [PGCol {getPGColTxt = "AlbumId"}]))
--   , ( FieldName {getFieldNameTxt = "foo"}
--     , AFCount (CTSimple [PGCol {getPGColTxt = "AlbumId"}]))
--   , ( FieldName {getFieldNameTxt = "max"}
--     , AFOp
--         (AggregateOp
--            { _aoOp = "max"
--            , _aoFields =
--                [ ( FieldName {getFieldNameTxt = "AlbumId"}
--                  , CFCol (PGCol {getPGColTxt = "AlbumId"}))
--                , ( FieldName {getFieldNameTxt = "TrackId"}
--                  , CFCol (PGCol {getPGColTxt = "TrackId"}))
--                ]
--            }))
--   ]
-- @
--
-- should produce:
--
-- SELECT COUNT(`t_Track1`.`AlbumId`) AS `count`,
--        COUNT(`t_Track1`.`AlbumId`) AS `foo`,
--        struct(max(`t_Track1`.`AlbumId`) AS `AlbumId`, max(`t_Track1`.`TrackId`) as TrackId) as `max`
-- FROM chinook.`Track` AS `t_Track1`
--
fromTableAggregateFieldG ::
     Args
  -> Top
  -> StringifyNumbers
  -> (Rql.FieldName, Ir.TableAggregateFieldG 'BigQuery (Const Void) Expression)
  -> ReaderT EntityAlias FromIr FieldSource
fromTableAggregateFieldG args permissionBasedTop stringifyNumbers (Rql.FieldName name, field) =
  case field of
    Ir.TAFAgg (aggregateFields :: [(Rql.FieldName, Ir.AggregateField 'BigQuery)]) ->
      case NE.nonEmpty aggregateFields of
        Nothing -> refute (pure NoAggregatesMustBeABug)
        Just fields -> do
          aggregates <-
            traverse
              (\(fieldName, aggregateField) -> do
                 fmap
                   (\aliasedThing ->
                      Aliased {aliasedAlias = Rql.getFieldNameTxt fieldName, ..})
                   (fromAggregateField aggregateField))
              fields
          pure (AggregateFieldSource name aggregates)
    Ir.TAFExp text ->
      pure
        (ExpressionFieldSource
           Aliased
             { aliasedThing = BigQuery.ValueExpression (StringValue text)
             , aliasedAlias = name
             })
    Ir.TAFNodes _ (fields :: [(Rql.FieldName, Ir.AnnFieldG 'BigQuery (Const Void) Expression)]) -> do
      fieldSources <-
        traverse
          (fromAnnFieldsG (argsExistingJoins args) stringifyNumbers)
          fields
      arrayAggProjections <-
        NE.nonEmpty (concatMap (toList . fieldSourceProjections) fieldSources)
          `onNothing` refute (pure NoProjectionFields)
      globalTop <- lift getGlobalTop
      pure
        (ArrayAggFieldSource
           Aliased
             { aliasedThing =
                 ArrayAgg
                   { arrayAggProjections
                   , arrayAggOrderBy = argsOrderBy args
                   , arrayAggTop = globalTop <> argsTop args <> permissionBasedTop
                   }
             , aliasedAlias = name
             })

fromAggregateField :: Ir.AggregateField 'BigQuery -> ReaderT EntityAlias FromIr Aggregate
fromAggregateField aggregateField =
  case aggregateField of
    Ir.AFExp text -> pure (TextAggregate text)
    Ir.AFCount countType -> CountAggregate <$> case countType of
      StarCountable               -> pure StarCountable
      NonNullFieldCountable names -> NonNullFieldCountable <$> traverse fromPGCol names
      DistinctCountable     names -> DistinctCountable     <$> traverse fromPGCol names
    Ir.AFOp Ir.AggregateOp {_aoOp = op, _aoFields = fields} -> do
      fs <- NE.nonEmpty fields `onNothing` refute (pure MalformedAgg)
      args <-
        traverse
          (\(Rql.FieldName fieldName, pgColFld) -> do
             expression' <-
               case pgColFld of
                 Ir.CFCol pgCol _columnType -> fmap ColumnExpression (fromPGCol pgCol)
                 Ir.CFExp text              -> pure (ValueExpression (StringValue text))
             pure (fieldName, expression'))
          fs
      pure (OpAggregates op args)

-- | The main sources of fields, either constants, fields or via joins.
fromAnnFieldsG ::
     Map TableName EntityAlias
  -> StringifyNumbers
  -> (Rql.FieldName, Ir.AnnFieldG 'BigQuery (Const Void) Expression)
  -> ReaderT EntityAlias FromIr FieldSource
fromAnnFieldsG existingJoins stringifyNumbers (Rql.FieldName name, field) =
  case field of
    Ir.AFColumn annColumnField -> do
      expression <- fromAnnColumnField stringifyNumbers annColumnField
      pure
        (ExpressionFieldSource
           Aliased {aliasedThing = expression, aliasedAlias = name})
    Ir.AFExpression text ->
      pure
        (ExpressionFieldSource
           Aliased
             { aliasedThing = BigQuery.ValueExpression (StringValue text)
             , aliasedAlias = name
             })
    Ir.AFObjectRelation objectRelationSelectG ->
      fmap
        (\aliasedThing ->
           JoinFieldSource (Aliased {aliasedThing, aliasedAlias = name}))
        (fromObjectRelationSelectG existingJoins objectRelationSelectG)
    Ir.AFArrayRelation arraySelectG ->
      fmap
        (\aliasedThing ->
           JoinFieldSource (Aliased {aliasedThing, aliasedAlias = name}))
        (fromArraySelectG arraySelectG)
    -- this will be gone once the code which collects remote joins from the IR
    -- emits a modified IR where remote relationships can't be reached
    Ir.AFRemote _ ->
      pure
        (ExpressionFieldSource
           Aliased
             { aliasedThing = BigQuery.ValueExpression (StringValue "null: remote field selected")
             , aliasedAlias = name
             })
    -- TODO: implement this
    Ir.AFDBRemote _ -> error "FIXME"


-- | Here is where we project a field as a column expression. If
-- number stringification is on, then we wrap it in a
-- 'ToStringExpression' so that it's casted when being projected.
fromAnnColumnField ::
     StringifyNumbers
  -> Ir.AnnColumnField 'BigQuery Expression
  -> ReaderT EntityAlias FromIr Expression
fromAnnColumnField _stringifyNumbers annColumnField = do
  fieldName <- fromPGCol pgCol
  if asText || False -- TOOD: (Rql.isScalarColumnWhere Psql.isBigNum typ && stringifyNumbers == StringifyNumbers)
     then pure (ToStringExpression (ColumnExpression fieldName))
     else case caseBoolExpMaybe of
       Nothing -> pure (ColumnExpression fieldName)
       Just ex -> do
           ex' <- (traverse fromAnnBoolExpFld >=> fromGBoolExp) (coerce ex)
           pure (ConditionalProjection ex' fieldName)
  where
    Ir.AnnColumnField { _acfInfo = Rql.ColumnInfo{pgiColumn=pgCol,pgiType=_typ}
                      , _acfAsText = asText :: Bool
                      , _acfOp = _ :: Maybe (Ir.ColumnOp 'BigQuery) -- TODO: What's this?
                      , _acfCaseBoolExpression = caseBoolExpMaybe :: Maybe (Ir.AnnColumnCaseBoolExp 'BigQuery Expression)
                      } = annColumnField

-- | This is where a field name "foo" is resolved to a fully qualified
-- field name [table].[foo]. The table name comes from EntityAlias in
-- the ReaderT.
fromPGCol :: ColumnName -> ReaderT EntityAlias FromIr FieldName
fromPGCol (ColumnName txt) = do
  EntityAlias {entityAliasText} <- ask
  pure (FieldName {fieldName = txt, fieldNameEntity = entityAliasText})

fieldSourceProjections :: FieldSource -> NonEmpty Projection
fieldSourceProjections =
  \case
    ExpressionFieldSource aliasedExpression ->
      pure (ExpressionProjection aliasedExpression)
    JoinFieldSource aliasedJoin ->
      NE.fromList
        -- Here we're producing all join fields needed later for
        -- Haskell-native joining.  They will be removed by upstream
        -- code.
        ([ FieldNameProjection
           (Aliased {aliasedThing = right, aliasedAlias = fieldNameText right})
         | (_left, right) <- joinOn join'
         ] <>
         -- Below:
         -- When we're doing an array-aggregate, e.g.
         --
         -- query MyQuery {
         --   hasura_Artist {
         --     albums_aggregate {
         --       aggregate {
         --         count
         --       }
         --     }
         --   }
         -- }
         --
         -- we're going to do a join on the albums table, and that
         -- join query will produce a single-row result. Therefore we
         -- can grab the whole entity as a STRUCT-typed object. See
         -- also the docs for 'fromArrayRelationSelectG' and for
         -- 'fromArrayAggregateSelectG'.
         case joinProvenance join' of
           ArrayJoinProvenance fields ->
             pure
               (ArrayEntityProjection
                  (joinAlias join')
                  aliasedJoin
                    { aliasedThing =
                        fmap
                          (\name ->
                             FieldName
                               { fieldName = name
                               , fieldNameEntity =
                                   entityAliasText (joinAlias join')
                               })
                          fields
                    , aliasedAlias = aliasedAlias aliasedJoin
                    })
           ObjectJoinProvenance fields ->
             pure
               (EntityProjection
                  aliasedJoin
                    { aliasedThing =
                        fmap
                          (\name ->
                             ( FieldName
                                 { fieldName = name
                                 , fieldNameEntity =
                                     entityAliasText (joinAlias join')
                                 }
                             , NoOrigin))
                          fields
                    , aliasedAlias = aliasedAlias aliasedJoin
                    })
           ArrayAggregateJoinProvenance fields ->
             pure
               (EntityProjection
                  aliasedJoin
                    { aliasedThing =
                        fmap
                          (\(name, fieldOrigin) ->
                             ( FieldName
                                 { fieldName = name
                                 , fieldNameEntity =
                                     entityAliasText (joinAlias join')
                                 }
                             , fieldOrigin))
                          fields
                    , aliasedAlias = aliasedAlias aliasedJoin
                    })
           _ -> [])
      where join' = aliasedThing aliasedJoin
    AggregateFieldSource name aggregates ->
      pure
        (AggregateProjections
           (Aliased {aliasedThing = aggregates, aliasedAlias = name}))
    ArrayAggFieldSource arrayAgg -> pure (ArrayAggProjection arrayAgg)
  where
    fieldNameText FieldName {fieldName} = fieldName

fieldSourceJoin :: FieldSource -> Maybe Join
fieldSourceJoin =
  \case
    JoinFieldSource aliasedJoin -> pure (aliasedThing aliasedJoin)
    ExpressionFieldSource {}    -> Nothing
    AggregateFieldSource {}     -> Nothing
    ArrayAggFieldSource {}      -> Nothing

--------------------------------------------------------------------------------
-- Joins

-- | Produce the join for an object relation. We produce a normal
-- select, but then include join fields. Then downstream, the
-- DataLoader will execute the lhs select and rhs join in separate
-- server queries, then do a Haskell-native join on the join fields.
--
-- See also 'fromArrayRelationSelectG' for similar example.
fromObjectRelationSelectG ::
     Map TableName EntityAlias
  -> Ir.ObjectRelationSelectG 'BigQuery (Const Void) Expression
  -> ReaderT EntityAlias FromIr Join
-- We're not using existingJoins at the moment, which was used to
-- avoid re-joining on the same table twice.
fromObjectRelationSelectG _existingJoins annRelationSelectG = do
  selectFrom <- lift (fromQualifiedTable tableFrom)
  let entityAlias :: EntityAlias = fromAlias selectFrom
  fieldSources <-
    local
      (const entityAlias)
      (traverse (fromAnnFieldsG mempty LeaveNumbersAlone) fields)
  selectProjections <-
    NE.nonEmpty (concatMap (toList . fieldSourceProjections) fieldSources)
      `onNothing` refute (pure NoProjectionFields)
  joinFieldName <- lift (fromRelName aarRelationshipName)
  joinAlias <-
    lift (generateEntityAlias (ObjectRelationTemplate joinFieldName))
  filterExpression <- local (const entityAlias) (fromAnnBoolExp tableFilter)
  innerJoinFields <- fromMappingFieldNames (fromAlias selectFrom) mapping
  joinOn <-
    fromMappingFieldNames joinAlias mapping
  let joinFieldProjections =
        map
          (\(fieldName', _) ->
             FieldNameProjection
               Aliased
                 { aliasedThing = fieldName'
                 , aliasedAlias = fieldName fieldName'
                 })
          innerJoinFields
  let selectFinalWantedFields = pure (fieldTextNames fields)
  pure
    Join
      { joinAlias
      , joinSource =
          JoinSelect
            Select
              { selectCardinality = One
              , selectFinalWantedFields
              , selectGroupBy = mempty
              , selectOrderBy = Nothing
              , selectTop = NoTop
              , selectProjections =
                  NE.fromList joinFieldProjections <> selectProjections
              , selectFrom
              , selectJoins = mapMaybe fieldSourceJoin fieldSources
              , selectWhere = Where [filterExpression]
              , selectOffset = Nothing
              }
      , joinOn
      , joinRightTable = fromAlias selectFrom
      , joinProvenance =
          ObjectJoinProvenance
            (fromMaybe [] selectFinalWantedFields) -- TODO: OK?
      -- Above: Needed by DataLoader to determine the type of
      -- Haskell-native join to perform.
      , joinFieldName
      , joinExtractPath = Nothing
      }
  where
    Ir.AnnObjectSelectG { _aosFields = fields :: Ir.AnnFieldsG 'BigQuery (Const Void) Expression
                        , _aosTableFrom = tableFrom :: TableName
                        , _aosTableFilter = tableFilter :: Ir.AnnBoolExp 'BigQuery Expression
                        } = annObjectSelectG
    Ir.AnnRelationSelectG { aarRelationshipName
                          , aarColumnMapping = mapping :: HashMap ColumnName ColumnName
                          , aarAnnSelect = annObjectSelectG :: Ir.AnnObjectSelectG 'BigQuery (Const Void) Expression
                          } = annRelationSelectG

-- We're not using existingJoins at the moment, which was used to
-- avoid re-joining on the same table twice.
_lookupTableFrom ::
     Map TableName EntityAlias
  -> TableName
  -> FromIr (Either EntityAlias From)
_lookupTableFrom existingJoins tableFrom = do
  case M.lookup tableFrom existingJoins of
    Just entityAlias -> pure (Left entityAlias)
    Nothing          -> fmap Right (fromQualifiedTable tableFrom)

fromArraySelectG :: Ir.ArraySelectG 'BigQuery (Const Void) Expression -> ReaderT EntityAlias FromIr Join
fromArraySelectG =
  \case
    Ir.ASSimple arrayRelationSelectG ->
      fromArrayRelationSelectG arrayRelationSelectG
    Ir.ASAggregate arrayAggregateSelectG ->
      fromArrayAggregateSelectG arrayAggregateSelectG

-- | Produce the join for an array aggregate relation. We produce a
-- normal select, but then include join fields. Then downstream, the
-- DataLoader will execute the lhs select and rhs join in separate
-- server queries, then do a Haskell-native join on the join fields.
--
-- See also 'fromArrayRelationSelectG' for similar example.
fromArrayAggregateSelectG ::
     Ir.AnnRelationSelectG 'BigQuery (Ir.AnnAggregateSelectG 'BigQuery (Const Void) Expression)
  -> ReaderT EntityAlias FromIr Join
fromArrayAggregateSelectG annRelationSelectG = do
  joinFieldName <- lift (fromRelName aarRelationshipName)
  select <- do
    lhsEntityAlias <- ask
    lift (fromSelectAggregate (pure (lhsEntityAlias, mapping)) annSelectG)
  alias <- lift (generateEntityAlias (ArrayAggregateTemplate joinFieldName))
  joinOn <- fromMappingFieldNames alias mapping
  innerJoinFields <-
    fromMappingFieldNames (fromAlias (selectFrom select)) mapping
  let joinFieldProjections =
        map
          (\(fieldName', _) ->
             FieldNameProjection
               Aliased
                 { aliasedThing = fieldName'
                 , aliasedAlias = fieldName fieldName'
                 })
          innerJoinFields
  let projections =
          (selectProjections select <> NE.fromList joinFieldProjections)
      joinSelect =
          select
            { selectWhere = selectWhere select
            , selectGroupBy = map fst innerJoinFields
            , selectProjections = projections
            }
  pure
    Join
      { joinAlias = alias
      , joinSource = JoinSelect joinSelect
      , joinRightTable = fromAlias (selectFrom select)
      , joinOn
      , joinProvenance =
          ArrayAggregateJoinProvenance $
            mapMaybe (\p -> (, aggregateProjectionsFieldOrigin p) <$> projectionAlias p) . toList . selectProjections $ select
         -- Above: Needed by DataLoader to determine the type of
         -- Haskell-native join to perform.
      , joinFieldName
      , joinExtractPath = Nothing
      }
  where
    Ir.AnnRelationSelectG { aarRelationshipName
                          , aarColumnMapping = mapping :: HashMap ColumnName ColumnName
                          , aarAnnSelect = annSelectG
                          } = annRelationSelectG

-- | Produce a join for an array relation.
--
-- Array relations in PG/MSSQL are expressed using LEFT OUTER JOIN
-- LATERAL or OUTER APPLY, which are essentially producing for each
-- row on the left an array of the result from the right. Which is
-- absolutely what you want for the array relationship.
--
-- BigQuery doesn't support that. Therefore we are instead performing
-- one big array aggregation, for ALL rows in the table - there is no
-- join occurring on the left-hand-side table, grouped by join
-- fields. The data-loader will perform the LHS query and the RHS query
-- separately.
--
-- What we do have is a GROUP BY and make sure that the join fields
-- are included in the output. Finally, in the
-- DataLoader.Plan/DataLoader.Execute, we implement a Haskell-native
-- join of the left-hand-side table and the right-hand-side table.
--
-- Data looks like:
--
--     join_field_a | join_field_b | aggFieldName (array type)
--     1            | 1            | [ { x: 1, y: 2 }, ... ]
--     1            | 2            | [ { x: 1, y: 2 }, ... ]
--
-- etc.
--
-- We want to produce a query that looks like:
--
--     SELECT artist_other_id,  -- For joining.
--
--            array_agg(struct(album_self_id, title)) as aggFieldName
--
--            -- ^ Aggregating the actual data.
--
--     FROM (SELECT *,  -- Get everything, plus the row number:
--
--                  ROW_NUMBER() OVER(PARTITION BY artist_other_id) artist_album_index
--
--           FROM hasura.Album
--           ORDER BY album_self_id ASC
--
--           -- ^ Order by here is important for stable results.  Any
--           order by clauses for the album should appear here, NOT IN
--           THE ARRAY_AGG.
--
--           )
--
--           AS indexed_album
--
--     WHERE artist_album_index > 1
--     -- ^ Here is where offsetting occurs.
--
--     GROUP BY artist_other_id
--     -- ^ Group by for joining.
--
--     ORDER BY artist_other_id;
--     ^ Ordering for the artist table should appear here.

fromArrayRelationSelectG ::
     Ir.ArrayRelationSelectG 'BigQuery (Const Void) Expression
  -> ReaderT EntityAlias FromIr Join
fromArrayRelationSelectG annRelationSelectG = do
  select <- lift (fromSelectRows annSelectG) -- Take the original select.
  joinFieldName <- lift (fromRelName aarRelationshipName)
  alias <- lift (generateEntityAlias (ArrayRelationTemplate joinFieldName))
  indexAlias <- lift (generateEntityAlias IndexTemplate)
  joinOn <- fromMappingFieldNames alias mapping
  innerJoinFields <-
    fromMappingFieldNames (fromAlias (selectFrom select)) mapping
  let joinFieldProjections =
        map
          (\(fieldName', _) ->
             FieldNameProjection
               Aliased
                 { aliasedThing = fieldName'
                 , aliasedAlias = fieldName fieldName'
                 })
          innerJoinFields
      joinSelect =
          Select
            { selectCardinality = One
            , selectFinalWantedFields = selectFinalWantedFields select
            , selectTop = NoTop
            , selectProjections =
                NE.fromList joinFieldProjections <>
                pure
                  (ArrayAggProjection
                     Aliased
                       { aliasedThing =
                           ArrayAgg
                             { arrayAggProjections =
                                 fmap
                                   aliasToFieldProjection
                                   (selectProjections select)
                             , arrayAggOrderBy = Nothing
                             , arrayAggTop = selectTop select
                               -- The sub-select takes care of caring about global top.
                               --
                               -- This handles the LIMIT need.
                             }
                       , aliasedAlias = aggFieldName
                       })
            , selectFrom =
                FromSelect
                  (Aliased
                     { aliasedAlias = coerce (fromAlias (selectFrom select))
                     , aliasedThing =
                         Select
                           { selectProjections =
                               selectProjections select <>
                               NE.fromList joinFieldProjections <>
                               pure
                                 (WindowProjection
                                    (Aliased
                                       { aliasedAlias = unEntityAlias indexAlias
                                       , aliasedThing =
                                           RowNumberOverPartitionBy
                                             -- The row numbers start from 1.
                                             (NE.fromList
                                                (map fst innerJoinFields))
                                             (selectOrderBy select)
                                             -- Above: Having the order by
                                             -- in here ensures that the
                                             -- row numbers are ordered by
                                             -- this ordering. Below, we
                                             -- order again for the
                                             -- general row order. Both
                                             -- are needed!
                                       }))
                           , selectFrom = selectFrom select
                           , selectJoins = selectJoins select
                           , selectWhere = selectWhere select
                           , selectOrderBy = selectOrderBy select
                             -- Above: This orders the rows themselves. In
                             -- the RowNumberOverPartitionBy, we also set
                             -- a row order for the calculation of the
                             -- indices. Both are needed!
                           , selectOffset = Nothing
                           , selectFinalWantedFields =
                               selectFinalWantedFields select
                           , selectCardinality = Many
                           , selectTop = NoTop
                           , selectGroupBy = mempty
                           }
                     })
            , selectWhere =
                case selectOffset select of
                  Nothing -> mempty
                  Just offset ->
                    Where
                      [ OpExpression
                          MoreOp
                          (ColumnExpression FieldName {fieldNameEntity = coerce (fromAlias (selectFrom select)), fieldName = unEntityAlias indexAlias})
                          offset
                      ]
            , selectOrderBy = Nothing -- Not needed.
            , selectJoins = mempty
            , selectOffset = Nothing
          -- This group by corresponds to the field name projections above. E.g. artist_other_id
            , selectGroupBy = map (fst) innerJoinFields
            }
  pure
    Join
      { joinAlias = alias
      , joinSource = JoinSelect joinSelect
      , joinRightTable = fromAlias (selectFrom select)
      , joinOn
      , joinProvenance =
          ArrayJoinProvenance
            (if True
               then (fromMaybe [] (selectFinalWantedFields select))
               else (mapMaybe
                       projectionAlias
                       (toList (selectProjections select))))
      -- Above: Needed by DataLoader to determine the type of
      -- Haskell-native join to perform.
      , joinFieldName
      , joinExtractPath = Just aggFieldName
      }
  where
    Ir.AnnRelationSelectG { aarRelationshipName
                          , aarColumnMapping = mapping :: HashMap ColumnName ColumnName
                          , aarAnnSelect = annSelectG
                          } = annRelationSelectG

-- | For entity projections, convert any entity aliases to their field
-- names. TODO: Add an explanation for this.
aliasToFieldProjection :: Projection -> Projection
aliasToFieldProjection =
  \case
    EntityProjection Aliased {aliasedAlias = name, aliasedThing = fields} ->
      EntityProjection
        Aliased
          { aliasedAlias = name
          , aliasedThing =
              fmap
                (\(FieldName {..}, origin) -> (FieldName {fieldNameEntity = name, ..}, origin))
                fields
          }
    p -> p

fromRelName :: Rql.RelName -> FromIr Text
fromRelName relName =
  pure (Rql.relNameToTxt relName)

-- | The context given by the reader is of the previous/parent
-- "remote" table. The WHERE that we're generating goes in the child,
-- "local" query. The @From@ passed in as argument is the local table.
--
-- We should hope to see e.g. "post.category = category.id" for a
-- local table of post and a remote table of category.
--
-- The left/right columns in @HashMap ColumnName ColumnName@ corresponds
-- to the left/right of @select ... join ...@. Therefore left=remote,
-- right=local in this context.
fromMapping ::
     From
  -> HashMap ColumnName ColumnName
  -> ReaderT EntityAlias FromIr [Expression]
fromMapping localFrom =
  traverse
    (\(remotePgCol, localPgCol) -> do
       localFieldName <- local (const (fromAlias localFrom)) (fromPGCol localPgCol)
       remoteFieldName <- fromPGCol remotePgCol
       pure
         (EqualExpression
            (ColumnExpression localFieldName)
            (ColumnExpression remoteFieldName))) .
  HM.toList

fromMappingFieldNames ::
     EntityAlias
  -> HashMap ColumnName ColumnName
  -> ReaderT EntityAlias FromIr [(FieldName,FieldName)]
fromMappingFieldNames localFrom =
  traverse
    (\(remotePgCol, localPgCol) -> do
       localFieldName <- local (const localFrom) (fromPGCol localPgCol)
       remoteFieldName <- fromPGCol remotePgCol
       pure
         ((,)
            (localFieldName)
            (remoteFieldName))) .
  HM.toList

--------------------------------------------------------------------------------
-- Basic SQL expression types

fromOpExpG :: Expression -> Ir.OpExpG 'BigQuery Expression -> FromIr Expression
fromOpExpG expression op =
  case op of
    Ir.ANISNULL      -> pure (IsNullExpression expression)
    Ir.ANISNOTNULL   -> pure (IsNotNullExpression expression)
    Ir.AEQ False val -> pure (nullableBoolEquality expression val)
    Ir.AEQ True val  -> pure (EqualExpression expression val)
    Ir.ANE False val -> pure (nullableBoolInequality expression val)
    Ir.ANE True val  -> pure (NotEqualExpression expression val)
    Ir.AGT val       -> pure (OpExpression MoreOp expression val)
    Ir.ALT val       -> pure (OpExpression LessOp expression val)
    Ir.AGTE val      -> pure (OpExpression MoreOrEqualOp expression val)
    Ir.ALTE val      -> pure (OpExpression LessOrEqualOp expression val)
    Ir.ACast _casts  -> refute (pure (UnsupportedOpExpG op)) -- mkCastsExp casts
    Ir.AIN _val      -> refute (pure (UnsupportedOpExpG op)) -- S.BECompareAny S.SEQ lhs val
    Ir.ANIN _val     -> refute (pure (UnsupportedOpExpG op)) -- S.BENot $ S.BECompareAny S.SEQ lhs val
    Ir.ALIKE _val    -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SLIKE lhs val
    Ir.ANLIKE _val   -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SNLIKE lhs val
    Ir.CEQ _rhsCol   -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SEQ lhs $ mkQCol rhsCol
    Ir.CNE _rhsCol   -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SNE lhs $ mkQCol rhsCol
    Ir.CGT _rhsCol   -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SGT lhs $ mkQCol rhsCol
    Ir.CLT _rhsCol   -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SLT lhs $ mkQCol rhsCol
    Ir.CGTE _rhsCol  -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SGTE lhs $ mkQCol rhsCol
    Ir.CLTE _rhsCol  -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SLTE lhs $ mkQCol rhsCol
    -- These are new as of 2021-02-18 to this API. Not sure what to do with them at present, marking as unsupported.

nullableBoolEquality :: Expression -> Expression -> Expression
nullableBoolEquality x y =
  OrExpression
    [ EqualExpression x y
    , AndExpression [IsNullExpression x, IsNullExpression y]
    ]

nullableBoolInequality :: Expression -> Expression -> Expression
nullableBoolInequality x y =
  OrExpression
    [ NotEqualExpression x y
    , AndExpression [IsNotNullExpression x, IsNullExpression y]
    ]

fromGBoolExp :: Ir.GBoolExp 'BigQuery Expression -> ReaderT EntityAlias FromIr Expression
fromGBoolExp =
  \case
    Ir.BoolAnd expressions ->
      fmap AndExpression (traverse fromGBoolExp expressions)
    Ir.BoolOr expressions ->
      fmap OrExpression (traverse fromGBoolExp expressions)
    Ir.BoolNot expression -> fmap NotExpression (fromGBoolExp expression)
    Ir.BoolExists gExists -> fmap ExistsExpression (fromGExists gExists)
    Ir.BoolFld expression -> pure expression

--------------------------------------------------------------------------------
-- Misc combinators

trueExpression :: Expression
trueExpression = ValueExpression (BoolValue True)

--------------------------------------------------------------------------------
-- Constants

aggFieldName :: Text
aggFieldName = "agg"

existsFieldName :: Text
existsFieldName = "exists_placeholder"

--------------------------------------------------------------------------------
-- Name generation

data NameTemplate
  = ArrayRelationTemplate Text
  | ArrayAggregateTemplate Text
  | ObjectRelationTemplate Text
  | TableTemplate Text
  | ForOrderAlias Text
  | IndexTemplate

generateEntityAlias :: NameTemplate -> FromIr EntityAlias
generateEntityAlias template = do
  FromIr
    (modify'
       (\FromIrState {..} ->
          FromIrState {indices = M.insertWith (+) prefix start indices, ..}))
  i <- FromIr (gets indices)
  pure (EntityAlias (prefix <> tshow (fromMaybe start (M.lookup prefix i))))
  where
    start = 1
    prefix = T.take 20 rendered
    rendered =
      case template of
        ArrayRelationTemplate sample  -> "ar_" <> sample
        ArrayAggregateTemplate sample -> "aa_" <> sample
        ObjectRelationTemplate sample -> "or_" <> sample
        TableTemplate sample          -> "t_" <> sample
        ForOrderAlias sample          -> "order_" <> sample
        IndexTemplate                 -> "idx"

fromAlias :: From -> EntityAlias
fromAlias (FromQualifiedTable Aliased {aliasedAlias}) = EntityAlias aliasedAlias
fromAlias (FromSelect Aliased {aliasedAlias})         = EntityAlias aliasedAlias

fieldTextNames :: Ir.AnnFieldsG 'BigQuery (Const Void) Expression -> [Text]
fieldTextNames = fmap (\(Rql.FieldName name, _) -> name)

unEntityAlias :: EntityAlias -> Text
unEntityAlias (EntityAlias t) = t

--------------------------------------------------------------------------------
-- Global limit support

getGlobalTop :: FromIr Top
getGlobalTop =
  FromIr
    (asks
       (\FromIrReader {config = FromIrConfig {globalSelectLimit}} ->
          globalSelectLimit))
