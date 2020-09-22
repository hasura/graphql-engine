{-# LANGUAGE ApplicativeDo #-}

-- | Translate from the DML to the TSql dialect.

module Hasura.SQL.Tsql.FromIr
  ( fromSelectFields
  , Error(..)
  , runFromIr
  , FromIr
  ) where

import           Control.Monad
import           Control.Monad.Validate
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Database.ODBC.SQLServer as Odbc
import qualified Hasura.RQL.DML.Select.Types as Ir
import qualified Hasura.RQL.Types.BoolExp as Ir
import qualified Hasura.RQL.Types.Common as Ir
import qualified Hasura.RQL.Types.Column as Ir
import qualified Hasura.SQL.DML as Sql
import           Hasura.SQL.Tsql.Types as Tsql
import qualified Hasura.SQL.Types as Sql
import           Prelude

--------------------------------------------------------------------------------
-- Types

data Error
  = FromTypeUnsupported (Ir.SelectFromG Sql.SQLExp)
  | FieldTypeUnsupported (Ir.AnnFieldG Sql.SQLExp)
  deriving (Show, Eq)

newtype FromIr a = FromIr { runFromIr :: Validate (NonEmpty Error) a}
  deriving (Functor, Applicative, Monad)

--------------------------------------------------------------------------------
-- Conversion functions

fromSelectFields :: Ir.AnnSelectG (Ir.AnnFieldsG Sql.SQLExp) Sql.SQLExp -> FromIr Tsql.Select
fromSelectFields annSelectG = do
  fields <- traverse fromProjection _asnFields
  selectFrom <-
    case _asnFrom of
      Ir.FromTable qualifiedObject -> fromQualifiedTable qualifiedObject
      _ -> FromIr (refute (pure (FromTypeUnsupported _asnFrom)))
  filterExpression <- fromAnnBoolExp permFilter
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
      , selectProjections = NE.fromList fields
      , selectFrom
      , selectWhere = ExpressionWhere filterExpression
      }
  where
    Ir.AnnSelectG {_asnFields, _asnFrom, _asnPerm, _asnArgs, _asnStrfyNum} =
      annSelectG
    Ir.TablePerm {_tpLimit = mPermLimit, _tpFilter = permFilter} = _asnPerm

fromAnnBoolExp :: Ir.GBoolExp (Ir.AnnBoolExpFld Sql.SQLExp) -> FromIr Expression
fromAnnBoolExp = traverse fromAnnBoolExpFld >=> fromGBoolExp

fromAnnBoolExpFld :: Ir.AnnBoolExpFld Sql.SQLExp -> FromIr Expression
fromAnnBoolExpFld =
  \case
    Ir.AVCol pgColumnInfo opExpGs -> do
      expression <- fromPGColumnInfo pgColumnInfo
      expressions <- traverse (fromOpExpG expression) opExpGs
      pure (AndExpression expressions)

fromPGColumnInfo :: Ir.PGColumnInfo -> FromIr Expression
fromPGColumnInfo Ir.PGColumnInfo{pgiColumn = pgCol} =
  pure (ColumnExpression (Sql.getPGColTxt pgCol))

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

fromGExists :: Ir.GExists Expression -> FromIr Select
fromGExists Ir.GExists {_geTable, _geWhere} = do
  selectFrom <- fromQualifiedTable _geTable
  whereExpression <- fromGBoolExp _geWhere
  pure
    Select
      { selectProjections =
          NE.fromList
            [ ExpressionProjection
                (unaliased trueExpression)
            ]
      , selectFrom
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

fromProjection :: (Ir.FieldName, Ir.AnnFieldG Sql.SQLExp) -> FromIr Projection
fromProjection (Ir.FieldName name, field) =
  case field of
    _ -> do
      value <-
        case field of
          Ir.AFExpression text ->
            pure (Tsql.ValueExpression (Odbc.TextValue text))
          _ -> FromIr (refute (pure (FieldTypeUnsupported field)))
      pure
        (ExpressionProjection
           Aliased
             { aliasedThing = value
             , aliasedAlias = Just (Alias {aliasText = name})
             })

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
