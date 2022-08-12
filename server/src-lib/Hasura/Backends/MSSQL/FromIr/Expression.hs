-- | This module translates the IR of boolean expressions into TSQL boolean
-- expressions.
--
-- Boolean expressions typically arise from permissions and where-clause
-- filters.
module Hasura.Backends.MSSQL.FromIr.Expression
  ( fromGBoolExp,
  )
where

import Control.Monad.Validate
import Data.HashMap.Strict qualified as HM
import Hasura.Backends.MSSQL.FromIr
  ( Error (UnsupportedOpExpG),
    FromIr,
    NameTemplate (TableTemplate),
    generateAlias,
  )
import Hasura.Backends.MSSQL.FromIr.Constants (existsFieldName, trueExpression)
import Hasura.Backends.MSSQL.Instances.Types ()
import Hasura.Backends.MSSQL.Types.Internal as TSQL
import Hasura.Prelude
import Hasura.RQL.IR qualified as IR
import Hasura.RQL.Types.Column qualified as IR
import Hasura.RQL.Types.Relationships.Local qualified as IR
import Hasura.SQL.Backend

-- | Translate boolean expressions into TSQL 'Expression's.
--
-- The `IR.AnnBoolExpFld` references fields and columns. The entity (e.g. table)
-- that binds these columns is supplied in the `ReaderT EntityAlias`
-- environment, such that the columns can be referred to unambiguously.
fromGBoolExp ::
  IR.GBoolExp 'MSSQL (IR.AnnBoolExpFld 'MSSQL Expression) ->
  ReaderT EntityAlias FromIr Expression
fromGBoolExp =
  \case
    IR.BoolAnd expressions ->
      fmap AndExpression (traverse fromGBoolExp expressions)
    IR.BoolOr expressions ->
      fmap OrExpression (traverse fromGBoolExp expressions)
    IR.BoolNot expression ->
      fmap NotExpression (fromGBoolExp expression)
    IR.BoolExists gExists ->
      fromGExists gExists
    IR.BoolField expression ->
      fromAnnBoolExpFld expression
  where
    fromGExists :: IR.GExists 'MSSQL (IR.AnnBoolExpFld 'MSSQL Expression) -> ReaderT EntityAlias FromIr Expression
    fromGExists IR.GExists {_geTable, _geWhere} = do
      selectFrom <- lift (aliasQualifiedTable _geTable)
      scopedTo selectFrom $ do
        whereExpression <- fromGBoolExp _geWhere
        pure $
          ExistsExpression $
            emptySelect
              { selectOrderBy = Nothing,
                selectProjections =
                  [ ExpressionProjection
                      ( Aliased
                          { aliasedThing = trueExpression,
                            aliasedAlias = existsFieldName
                          }
                      )
                  ],
                selectFrom = Just selectFrom,
                selectJoins = mempty,
                selectWhere = Where [whereExpression],
                selectTop = NoTop,
                selectFor = NoFor,
                selectOffset = Nothing
              }

-- | Translate boolean expressions into TSQL 'Expression's.
--
-- The `IR.AnnBoolExpFld` references fields and columns. The entity (e.g. table)
-- that binds these columns is supplied in the `ReaderT EntityAlias`
-- environment, such that the columns can be referred to unambiguously.
fromAnnBoolExpFld ::
  IR.AnnBoolExpFld 'MSSQL Expression ->
  ReaderT EntityAlias FromIr Expression
fromAnnBoolExpFld =
  \case
    IR.AVColumn columnInfo opExpGs -> do
      expressions <- traverse (fromOpExpG columnInfo) opExpGs
      pure (AndExpression expressions)
    IR.AVRelationship IR.RelInfo {riMapping = mapping, riRTable = table} annBoolExp -> do
      selectFrom <- lift (aliasQualifiedTable table)
      mappingExpression <- translateMapping selectFrom mapping
      whereExpression <- scopedTo selectFrom (fromGBoolExp annBoolExp)
      pure
        ( ExistsExpression
            emptySelect
              { selectOrderBy = Nothing,
                selectProjections =
                  [ ExpressionProjection
                      ( Aliased
                          { aliasedThing = trueExpression,
                            aliasedAlias = existsFieldName
                          }
                      )
                  ],
                selectFrom = Just selectFrom,
                selectJoins = mempty,
                selectWhere = Where (mappingExpression <> [whereExpression]),
                selectTop = NoTop,
                selectFor = NoFor,
                selectOffset = Nothing
              }
        )
  where
    -- Translate a relationship field mapping into column equality comparisons.
    translateMapping ::
      From ->
      HashMap ColumnName ColumnName ->
      ReaderT EntityAlias FromIr [Expression]
    translateMapping localFrom =
      traverse
        ( \(remoteColumn, localColumn) -> do
            localFieldName <- scopedTo localFrom (fromColumn localColumn)
            remoteFieldName <- fromColumn remoteColumn
            pure
              ( OpExpression
                  TSQL.EQ'
                  (ColumnExpression localFieldName)
                  (ColumnExpression remoteFieldName)
              )
        )
        . HM.toList

-- | Scope a translation action to the table bound in a FROM clause.
scopedTo :: From -> ReaderT EntityAlias FromIr a -> ReaderT EntityAlias FromIr a
scopedTo from = local (const (fromAlias from))

-- | Translate a column reference occurring in a boolean expression into an
-- equivalent 'Expression'.
--
-- Different text types support different operators. Therefore we cast some text
-- types to "varchar(max)", which supports the most operators.
fromColumnInfo :: IR.ColumnInfo 'MSSQL -> ReaderT EntityAlias FromIr Expression
fromColumnInfo IR.ColumnInfo {ciColumn = column, ciType} = do
  fieldName <- TSQL.columnNameToFieldName column <$> ask
  if shouldCastToVarcharMax ciType
    then pure (CastExpression (ColumnExpression fieldName) WvarcharType DataLengthMax)
    else pure (ColumnExpression fieldName)
  where
    shouldCastToVarcharMax :: IR.ColumnType 'MSSQL -> Bool
    shouldCastToVarcharMax typ =
      typ == IR.ColumnScalar TextType || typ == IR.ColumnScalar WtextType

-- | Get FieldSource from a TAFExp type table aggregate field
fromColumn :: ColumnName -> ReaderT EntityAlias FromIr FieldName
fromColumn column = columnNameToFieldName column <$> ask

-- | Translate a single `IR.OpExpG` operation on a column into an expression.
fromOpExpG :: IR.ColumnInfo 'MSSQL -> IR.OpExpG 'MSSQL Expression -> ReaderT EntityAlias FromIr Expression
fromOpExpG columnInfo op = do
  column <- fromColumnInfo columnInfo
  case op of
    IR.ANISNULL -> pure $ TSQL.IsNullExpression column
    IR.ANISNOTNULL -> pure $ TSQL.IsNotNullExpression column
    IR.AEQ False val -> pure $ nullableBoolEquality column val
    IR.AEQ True val -> pure $ OpExpression TSQL.EQ' column val
    IR.ANE False val -> pure $ nullableBoolInequality column val
    IR.ANE True val -> pure $ OpExpression TSQL.NEQ' column val
    IR.AGT val -> pure $ OpExpression TSQL.GT column val
    IR.ALT val -> pure $ OpExpression TSQL.LT column val
    IR.AGTE val -> pure $ OpExpression TSQL.GTE column val
    IR.ALTE val -> pure $ OpExpression TSQL.LTE column val
    IR.AIN val -> pure $ OpExpression TSQL.IN column val
    IR.ANIN val -> pure $ OpExpression TSQL.NIN column val
    IR.ALIKE val -> pure $ OpExpression TSQL.LIKE column val
    IR.ANLIKE val -> pure $ OpExpression TSQL.NLIKE column val
    IR.ABackendSpecific o -> case o of
      ASTContains val -> pure $ TSQL.STOpExpression TSQL.STContains column val
      ASTCrosses val -> pure $ TSQL.STOpExpression TSQL.STCrosses column val
      ASTEquals val -> pure $ TSQL.STOpExpression TSQL.STEquals column val
      ASTIntersects val -> pure $ TSQL.STOpExpression TSQL.STIntersects column val
      ASTOverlaps val -> pure $ TSQL.STOpExpression TSQL.STOverlaps column val
      ASTTouches val -> pure $ TSQL.STOpExpression TSQL.STTouches column val
      ASTWithin val -> pure $ TSQL.STOpExpression TSQL.STWithin column val
    -- As of March 2021, only geometry/geography casts are supported
    IR.ACast _casts -> refute (pure (UnsupportedOpExpG op)) -- mkCastsExp casts

    -- We do not yet support column names in permissions
    IR.CEQ _rhsCol -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SEQ lhs $ mkQCol rhsCol
    IR.CNE _rhsCol -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SNE lhs $ mkQCol rhsCol
    IR.CGT _rhsCol -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SGT lhs $ mkQCol rhsCol
    IR.CLT _rhsCol -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SLT lhs $ mkQCol rhsCol
    IR.CGTE _rhsCol -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SGTE lhs $ mkQCol rhsCol
    IR.CLTE _rhsCol -> refute (pure (UnsupportedOpExpG op)) -- S.BECompare S.SLTE lhs $ mkQCol rhsCol

nullableBoolEquality :: Expression -> Expression -> Expression
nullableBoolEquality x y =
  OrExpression
    [ OpExpression TSQL.EQ' x y,
      AndExpression [IsNullExpression x, IsNullExpression y]
    ]

nullableBoolInequality :: Expression -> Expression -> Expression
nullableBoolInequality x y =
  OrExpression
    [ OpExpression TSQL.NEQ' x y,
      AndExpression [IsNotNullExpression x, IsNullExpression y]
    ]

aliasQualifiedTable :: TableName -> FromIr From
aliasQualifiedTable schemadTableName@(TableName {tableName}) = do
  alias <- generateAlias (TableTemplate tableName)
  pure
    ( FromQualifiedTable
        ( Aliased
            { aliasedThing = schemadTableName,
              aliasedAlias = alias
            }
        )
    )
