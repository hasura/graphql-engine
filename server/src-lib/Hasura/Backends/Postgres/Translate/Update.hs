-- | Postgres Translate Update
--
-- Translates IR update to Postgres-specific SQL UPDATE statements.
module Hasura.Backends.Postgres.Translate.Update
  ( mkUpdateCTE,
    UpdateCTE (..),
  )
where

import Data.HashMap.Strict qualified as HashMap
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Types
import Hasura.Backends.Postgres.Translate.BoolExp
import Hasura.Backends.Postgres.Translate.Insert
import Hasura.Backends.Postgres.Translate.Returning
import Hasura.Backends.Postgres.Types.Update
import Hasura.Base.Error (QErr)
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.Update
import Hasura.RQL.IR.Update.Batch
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Session (UserInfo)
import Hasura.SQL.Types

data UpdateCTE
  = -- | Used for /update_table/ and /update_table_by_pk/.
    Update S.TopLevelCTE
  | -- | Used for /update_table_many/.
    MultiUpdate [S.TopLevelCTE]
  deriving (Show)

-- | Create the update CTE.
mkUpdateCTE ::
  forall pgKind m.
  (Backend ('Postgres pgKind), MonadIO m, MonadError QErr m) =>
  UserInfo ->
  AnnotatedUpdate ('Postgres pgKind) ->
  m UpdateCTE
mkUpdateCTE userInfo (AnnotatedUpdateG tn permFltr chk updateVariant _ columnsInfo _tCase _validateInput) =
  case updateVariant of
    SingleBatch update -> do
      updateExp <- translateUpdate update
      pure $ Update updateExp
    MultipleBatches updates -> do
      updateExps <- traverse translateUpdate updates
      pure $ MultiUpdate updateExps
  where
    mkWhere :: AnnBoolExp ('Postgres pgKind) S.SQLExp -> m (Maybe S.WhereFrag)
    mkWhere annBoolExp = do
      boolExp <- toSQLBoolExp userInfo (S.QualTable tn) $ andAnnBoolExps permFltr $ annBoolExp
      pure
        $ Just
        . S.WhereFrag
        . S.simplifyBoolExp
        $ boolExp

    checkConstraint :: m (Maybe S.RetExp)
    checkConstraint = do
      boolExp <- toSQLBoolExp userInfo (S.QualTable tn) chk
      pure
        $ Just
        $ S.RetExp
          [ S.selectStar,
            asCheckErrorExtractor
              . insertCheckConstraint
              $ boolExp
          ]

    translateUpdate :: UpdateBatch ('Postgres pgKind) UpdateOpExpression S.SQLExp -> m S.TopLevelCTE
    translateUpdate UpdateBatch {..} = do
      whereExp <- mkWhere _ubWhere
      checkExp <- checkConstraint
      pure
        $ S.CTEUpdate
          S.SQLUpdate
            { upTable = tn,
              upSet =
                S.SetExp $ map (expandOperator columnsInfo) (HashMap.toList _ubOperations),
              upFrom = Nothing,
              upWhere = whereExp,
              upRet = checkExp
            }

expandOperator :: [ColumnInfo ('Postgres pgKind)] -> (PGCol, UpdateOpExpression S.SQLExp) -> S.SetExpItem
expandOperator infos (column, op) = S.SetExpItem
  $ (column,)
  $ case op of
    UpdateSet e -> e
    UpdateInc e -> S.mkSQLOpExp S.incOp identifier (asNum e)
    UpdateAppend e -> S.mkSQLOpExp S.jsonbConcatOp identifier (asJSON e)
    UpdatePrepend e -> S.mkSQLOpExp S.jsonbConcatOp (asJSON e) identifier
    UpdateDeleteKey e -> S.mkSQLOpExp S.jsonbDeleteOp identifier (asText e)
    UpdateDeleteElem e -> S.mkSQLOpExp S.jsonbDeleteOp identifier (asInt e)
    UpdateDeleteAtPath a -> S.mkSQLOpExp S.jsonbDeleteAtPathOp identifier (asArray a)
  where
    identifier = S.SEIdentifier $ toIdentifier column
    asInt e = S.SETyAnn e S.intTypeAnn
    asText e = S.SETyAnn e S.textTypeAnn
    asJSON e = S.SETyAnn e S.jsonbTypeAnn
    asArray a = S.SETyAnn (S.SEArray a) S.textArrTypeAnn
    asNum e = S.SETyAnn e
      $ case find (\info -> ciColumn info == column) infos <&> ciType of
        Just (ColumnScalar s) -> S.mkTypeAnn $ CollectableTypeScalar s
        _ -> S.numericTypeAnn
