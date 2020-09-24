{-# LANGUAGE ApplicativeDo #-}

-- | Translate from the DML to the TSql dialect.

module Hasura.SQL.Tsql.FromIr
  ( fromSelectRows
  , fromSelectAggregate
  , Error(..)
  , runFromIr
  , FromIr
  ) where

import           Control.Monad
import           Control.Monad.Validate
import           Data.Foldable
import           Data.HashMap.Strict (HashMap)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.Maybe
import qualified Database.ODBC.SQLServer as Odbc
import qualified Hasura.RQL.DML.Select.Types as Ir
import qualified Hasura.RQL.Types.BoolExp as Ir
import qualified Hasura.RQL.Types.Column as Ir
import qualified Hasura.RQL.Types.Common as Ir
import qualified Hasura.SQL.DML as Sql
import           Hasura.SQL.Tsql.Types as Tsql
import qualified Hasura.SQL.Types as Sql
import           Prelude

--------------------------------------------------------------------------------
-- Types

data Error
  = FromTypeUnsupported (Ir.SelectFromG Sql.SQLExp)
  | MalformedAgg
  | FieldTypeUnsupportedForNow (Ir.AnnFieldG Sql.SQLExp)
  | AggTypeUnsupportedForNow (Ir.TableAggregateFieldG Sql.SQLExp)
  | NoProjectionFields
  | NoAggregatesMustBeABug
  | UnsupportedArraySelect (Ir.ArraySelectG Sql.SQLExp)
  deriving (Show, Eq)

newtype FromIr a = FromIr { runFromIr :: Validate (NonEmpty Error) a}
  deriving (Functor, Applicative, Monad)

--------------------------------------------------------------------------------
-- Top-level exported functions

fromSelectRows :: Ir.AnnSelectG (Ir.AnnFieldsG Sql.SQLExp) Sql.SQLExp -> FromIr Tsql.Select
fromSelectRows annSelectG
  -- Here is a spot where the from'd thing binds a scope that the
  -- order/where will be related to.
 = do
  selectFrom <-
    case _asnFrom of
      Ir.FromTable qualifiedObject -> fromQualifiedTable qualifiedObject
      _ -> FromIr (refute (pure (FromTypeUnsupported _asnFrom)))
  fieldSources <- traverse fromAnnFieldsG _asnFields
  filterExpression <- fromAnnBoolExp permFilter
  selectProjections <-
    case NE.nonEmpty (concatMap (toList . fieldSourceProjections) fieldSources) of
      Nothing -> FromIr (refute (pure NoProjectionFields))
      Just ne -> pure ne
  pure
    Select
      { selectTop =
          case mPermLimit of
            Nothing -> uncommented NoTop
            Just limit ->
              Commented
                { commentedComment = pure DueToPermission
                , commentedThing = Top limit
                }
      , selectProjections
      , selectFrom
      , selectJoins = mapMaybe fieldSourceJoin fieldSources
      , selectWhere = ExpressionWhere filterExpression
      }
  where
    Ir.AnnSelectG {_asnFields, _asnFrom, _asnPerm, _asnArgs, _asnStrfyNum} =
      annSelectG
    Ir.TablePerm {_tpLimit = mPermLimit, _tpFilter = permFilter} = _asnPerm

fromSelectAggregate ::
     Ir.AnnSelectG [(Ir.FieldName, Ir.TableAggregateFieldG Sql.SQLExp)] Sql.SQLExp
  -> FromIr Tsql.Select
fromSelectAggregate annSelectG = do
  selectFrom <-
    case _asnFrom of
      Ir.FromTable qualifiedObject -> fromQualifiedTable qualifiedObject
      _ -> FromIr (refute (pure (FromTypeUnsupported _asnFrom)))
  fieldSources <- traverse fromTableAggregateFieldG _asnFields
  filterExpression <- fromAnnBoolExp permFilter
  selectProjections <-
    case NE.nonEmpty (concatMap (toList . fieldSourceProjections) fieldSources) of
      Nothing -> FromIr (refute (pure NoProjectionFields))
      Just ne -> pure ne
  pure
    Select
      { selectTop =
          case mPermLimit of
            Nothing -> uncommented NoTop
            Just limit ->
              Commented
                { commentedComment = pure DueToPermission
                , commentedThing = Top limit
                }
      , selectProjections
      , selectFrom
      , selectJoins = mapMaybe fieldSourceJoin fieldSources
      , selectWhere = ExpressionWhere filterExpression
      }
  where
    Ir.AnnSelectG {_asnFields, _asnFrom, _asnPerm, _asnArgs, _asnStrfyNum} =
      annSelectG
    Ir.TablePerm {_tpLimit = mPermLimit, _tpFilter = permFilter} = _asnPerm

--------------------------------------------------------------------------------
-- Conversion functions

fromAnnBoolExp :: Ir.GBoolExp (Ir.AnnBoolExpFld Sql.SQLExp) -> FromIr Expression
fromAnnBoolExp = traverse fromAnnBoolExpFld >=> fromGBoolExp

fromAnnBoolExpFld :: Ir.AnnBoolExpFld Sql.SQLExp -> FromIr Expression
fromAnnBoolExpFld =
  \case
    Ir.AVCol pgColumnInfo opExpGs -> do
      expression <- fromPGColumnInfo pgColumnInfo
      expressions <- traverse (fromOpExpG expression) opExpGs
      pure (AndExpression expressions)

-- TODO: Question: how do we associate/confirm that this column name
-- associates to an entity listed in the From list.
--
-- Example:
--
-- select x from a, b where f > 1;
--
-- We should have an explicit mapping from included entities to field
-- names of that entity.
--
fromPGColumnInfo :: Ir.PGColumnInfo -> FromIr Expression
fromPGColumnInfo info = do
  name <- fromPGColumnName info
  pure (ColumnExpression name)

fromGExists :: Ir.GExists Expression -> FromIr Select
fromGExists Ir.GExists {_geTable, _geWhere} = do
  -- Here is a spot where the from'd thing binds a scope that the
  -- order/where will be related to.
  -- But the scope here is quite short.
  selectFrom <- fromQualifiedTable _geTable
  whereExpression <- fromGBoolExp _geWhere
  pure
    Select
      {selectProjections =
          NE.fromList
            [ ExpressionProjection
                (unaliased trueExpression)
            ]
      , selectFrom
      , selectJoins = mempty
      , selectWhere = ExpressionWhere whereExpression
      , selectTop = uncommented NoTop
      }

fromQualifiedTable :: Sql.QualifiedObject Sql.TableName -> FromIr From
fromQualifiedTable qualifiedObject =
  pure
    (FromQualifiedTable
       (unaliased
          (Qualified
             { qualifiedThing = TableName {tableNameText = qname}
             , qualifiedSchemaName =
                 Just (SchemaName {schemaNameParts = [schemaName]})
             })))
  where
    Sql.QualifiedObject { qSchema = Sql.SchemaName schemaName
                         -- TODO: Consider many x.y.z. in schema name.
                        , qName = Sql.TableName qname
                        } = qualifiedObject

--------------------------------------------------------------------------------
-- Sources of projected fields

data FieldSource
  = ExpressionFieldSource (Aliased Expression)
  | ColumnFieldSource (Aliased FieldName)
  | JoinFieldSource (Aliased Join)
  | AggregateFieldSource (NonEmpty (Aliased Aggregate))
  deriving (Eq, Show)

fromTableAggregateFieldG ::
     (Ir.FieldName, Ir.TableAggregateFieldG Sql.SQLExp) -> FromIr FieldSource
fromTableAggregateFieldG (Ir.FieldName name, field) =
  case field of
    Ir.TAFAgg (aggregateFields :: [(Ir.FieldName, Ir.AggregateField)]) ->
      case NE.nonEmpty aggregateFields of
        Nothing -> FromIr (refute (pure NoAggregatesMustBeABug))
        Just fields -> do
          aggregates <-
            traverse
              (\(fieldName, aggregateField) -> do
                 fmap
                   (\aliasedThing ->
                      Aliased
                        { aliasedAlias =
                            Just
                              (Alias {aliasText = Ir.getFieldNameTxt fieldName})
                        , ..
                        })
                   (fromAggregateField aggregateField))
              fields
          pure (AggregateFieldSource aggregates)
    Ir.TAFExp text ->
      pure
        (ExpressionFieldSource
           Aliased
             { aliasedThing = Tsql.ValueExpression (Odbc.TextValue text)
             , aliasedAlias = Just (Alias {aliasText = name})
             })
    Ir.TAFNodes {} -> FromIr (refute (pure (AggTypeUnsupportedForNow field)))

fromAggregateField :: Ir.AggregateField -> FromIr Aggregate
fromAggregateField aggregateField =
  case aggregateField of
    Ir.AFExp text -> pure (TextAggregate text)
    Ir.AFCount countType ->
      fmap
        CountAggregate
        (case countType of
           Sql.CTStar -> pure StarCountable
           Sql.CTSimple fields ->
             case NE.nonEmpty fields of
               Nothing -> FromIr (refute (pure MalformedAgg))
               Just fields' -> do
                 fields'' <- traverse fromPGCol fields'
                 pure (NonNullFieldCountable fields'')
           Sql.CTDistinct fields ->
             case NE.nonEmpty fields of
               Nothing -> FromIr (refute (pure MalformedAgg))
               Just fields' -> do
                 fields'' <- traverse fromPGCol fields'
                 pure (DistinctCountable fields''))
    Ir.AFOp Ir.AggregateOp{_aoOp,_aoFields} ->
      error "Ir.AFOp Ir.AggregateOp"

-- | The main sources of fields, either constants, fields or via joins.
fromAnnFieldsG :: (Ir.FieldName, Ir.AnnFieldG Sql.SQLExp) -> FromIr FieldSource
fromAnnFieldsG (Ir.FieldName name, field) =
  case field of
    Ir.AFColumn annColumnField -> do
      fieldName <- fromAnnColumnField annColumnField
      pure
        (ColumnFieldSource
           Aliased
             { aliasedThing = fieldName
             , aliasedAlias = Just (Alias {aliasText = name})
             })
    Ir.AFExpression text ->
      pure
        (ExpressionFieldSource
           Aliased
             { aliasedThing = Tsql.ValueExpression (Odbc.TextValue text)
             , aliasedAlias = Just (Alias {aliasText = name})
             })
    Ir.AFObjectRelation objectRelationSelectG ->
      fmap
        (\aliasedThing ->
           JoinFieldSource
             (Aliased
                {aliasedThing, aliasedAlias = Just (Alias {aliasText = name})}))
        (fromObjectRelationSelectG objectRelationSelectG)
    Ir.AFArrayRelation arraySelectG ->
      fmap
        (\aliasedThing ->
           JoinFieldSource
             (Aliased
                {aliasedThing, aliasedAlias = Just (Alias {aliasText = name})}))
        (fromArraySelectG arraySelectG)
    -- Vamshi said to ignore these three for now:
    Ir.AFNodeId {} -> FromIr (refute (pure (FieldTypeUnsupportedForNow field)))
    Ir.AFRemote {} -> FromIr (refute (pure (FieldTypeUnsupportedForNow field)))
    Ir.AFComputedField {} ->
      FromIr (refute (pure (FieldTypeUnsupportedForNow field)))

fromAnnColumnField :: Ir.AnnColumnField -> FromIr FieldName
fromAnnColumnField annColumnField = fromPGColumnName pgColumnInfo
  where
    Ir.AnnColumnField { _acfInfo = pgColumnInfo
                      , _acfAsText = asText
                      , _acfOp = op
                      } = annColumnField

fromPGColumnName :: Ir.PGColumnInfo -> FromIr FieldName
fromPGColumnName Ir.PGColumnInfo{pgiColumn = pgCol} =
  fromPGCol pgCol

fromPGCol :: Sql.PGCol -> FromIr FieldName
fromPGCol pgCol =
  pure (FieldName (Sql.getPGColTxt pgCol))

fieldSourceProjections :: FieldSource -> NonEmpty Projection
fieldSourceProjections =
  \case
    ExpressionFieldSource aliasedExpression ->
      pure (ExpressionProjection aliasedExpression)
    ColumnFieldSource name -> pure (FieldNameProjection name)
    JoinFieldSource aliasedJoin ->
      pure (FieldNameProjection (fmap joinFieldName aliasedJoin))
    AggregateFieldSource aggregates ->
      fmap AggregateProjection aggregates

fieldSourceJoin :: FieldSource -> Maybe Join
fieldSourceJoin =
  \case
    JoinFieldSource aliasedJoin -> pure (aliasedThing aliasedJoin)
    ExpressionFieldSource {} -> Nothing
    ColumnFieldSource {} -> Nothing
    AggregateFieldSource {} -> Nothing

--------------------------------------------------------------------------------
-- Joins

-- TODO: field mappings.
fromObjectRelationSelectG :: Ir.ObjectRelationSelectG Sql.SQLExp -> FromIr Join
fromObjectRelationSelectG annRelationSelectG = do
  fieldSources <- traverse fromAnnFieldsG fields
  selectFrom <- fromQualifiedTable tableFrom
  filterExpression <- fromAnnBoolExp tableFilter
  selectProjections <-
    case NE.nonEmpty (concatMap (toList . fieldSourceProjections) fieldSources) of
      Nothing -> FromIr (refute (pure NoProjectionFields))
      Just ne -> pure ne
  fieldName <- fromRelName aarRelationshipName
  pure
    Join
      { joinFieldName = fieldName
      , joinSelect =
          Select
            { selectTop = uncommented NoTop
            , selectProjections
            , selectFrom
            , selectJoins = mapMaybe fieldSourceJoin fieldSources
            , selectWhere = ExpressionWhere filterExpression
            }
      }
  where
    Ir.AnnObjectSelectG { _aosFields = fields :: Ir.AnnFieldsG Sql.SQLExp
                        , _aosTableFrom = tableFrom :: Sql.QualifiedTable
                        , _aosTableFilter = tableFilter :: Ir.AnnBoolExp Sql.SQLExp
                        } = annObjectSelectG
    Ir.AnnRelationSelectG { aarRelationshipName
                          , aarColumnMapping = mapping :: HashMap Sql.PGCol Sql.PGCol
                          , aarAnnSelect = annObjectSelectG :: Ir.AnnObjectSelectG Sql.SQLExp
                          } = annRelationSelectG

fromArraySelectG :: Ir.ArraySelectG Sql.SQLExp -> FromIr Join
fromArraySelectG =
  \case
    Ir.ASSimple arrayRelationSelectG ->
      fromArrayRelationSelectG arrayRelationSelectG
    Ir.ASAggregate arrayAggregateSelectG ->
      error "Ir.ASAggregate arrayAggregateSelectG"
    select@Ir.ASConnection {} ->
      FromIr (refute (pure (UnsupportedArraySelect select)))

-- TODO: field mappings.
fromArrayRelationSelectG :: Ir.ArrayRelationSelectG Sql.SQLExp -> FromIr Join
fromArrayRelationSelectG annRelationSelectG = do
  fieldName <- fromRelName aarRelationshipName
  select <- fromSelectRows annSelectG
  pure Join {joinFieldName = fieldName, joinSelect = select}
  where
    Ir.AnnRelationSelectG { aarRelationshipName
                          , aarColumnMapping = mapping :: HashMap Sql.PGCol Sql.PGCol
                          , aarAnnSelect = annSelectG
                          } = annRelationSelectG

fromRelName :: Ir.RelName -> FromIr FieldName
fromRelName relName =
  pure (FieldName (Ir.relNameToTxt relName))

--------------------------------------------------------------------------------
-- Basic SQL expression types

fromOpExpG :: Expression -> Ir.OpExpG Sql.SQLExp -> FromIr Expression
fromOpExpG expression =
  \case
    Ir.ANISNULL -> pure (IsNullExpression expression)

fromSQLExp :: Sql.SQLExp -> FromIr Expression
fromSQLExp =
  \case
    Sql.SENull -> pure (ValueExpression Odbc.NullValue)

fromGBoolExp :: Ir.GBoolExp Expression -> FromIr Expression
fromGBoolExp =
  \case
    Ir.BoolAnd expressions ->
      fmap AndExpression (traverse fromGBoolExp expressions)
    Ir.BoolOr expressions ->
      fmap OrExpression (traverse fromGBoolExp expressions)
    Ir.BoolNot expression -> fmap NotExpression (fromGBoolExp expression)
    Ir.BoolExists gExists -> fmap SelectExpression (fromGExists gExists)
    Ir.BoolFld expression -> pure expression

--------------------------------------------------------------------------------
-- Comments

uncommented :: a -> Commented a
uncommented a = Commented {commentedComment = Nothing, commentedThing = a}

unaliased :: a -> Aliased a
unaliased a = Aliased {aliasedAlias = Nothing, aliasedThing = a}

--------------------------------------------------------------------------------
-- Misc combinators

trueExpression :: Expression
trueExpression = ValueExpression (Odbc.BoolValue True)
