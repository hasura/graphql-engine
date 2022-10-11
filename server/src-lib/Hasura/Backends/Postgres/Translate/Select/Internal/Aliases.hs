-- | Stuff gutted from Translate.Select
module Hasura.Backends.Postgres.Translate.Select.Internal.Aliases
  ( mkAggregateOrderByAlias,
    mkAnnOrderByAlias,
    mkArrayRelationAlias,
    mkArrayRelationSourcePrefix,
    mkBaseTableAlias,
    mkBaseTableIdentifier,
    contextualizeBaseTableColumn,
    mkComputedFieldTableIdentifier,
    mkObjectRelationTableAlias,
    mkOrderByFieldName,
  )
where

import Control.Monad.Writer.Strict
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.Text.Extended
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Types
import Hasura.Backends.Postgres.Translate.Types
import Hasura.Prelude
import Hasura.RQL.IR.Select
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.Relationships.Local
import Hasura.SQL.Backend

-- | Generate alias for order by extractors
mkAnnOrderByAlias ::
  TableIdentifier -> FieldName -> SimilarArrayFields -> AnnotatedOrderByElement ('Postgres pgKind) v -> S.ColumnAlias
mkAnnOrderByAlias tablePrefix parAls similarFields = \case
  AOCColumn pgColumnInfo ->
    let pgColumn = ciColumn pgColumnInfo
        obColAls = contextualizeBaseTableColumn tablePrefix pgColumn
     in obColAls
  -- "pfx.or.relname"."pfx.ob.or.relname.rest" AS "pfx.ob.or.relname.rest"
  AOCObjectRelation relInfo _ rest ->
    let rn = riName relInfo
        relPfx = mkObjectRelationTableAlias tablePrefix rn
        ordByFldName = mkOrderByFieldName rn
        nesAls = mkAnnOrderByAlias relPfx ordByFldName mempty rest
     in nesAls
  AOCArrayAggregation relInfo _ aggOrderBy ->
    let rn = riName relInfo
        arrPfx =
          mkArrayRelationSourcePrefix tablePrefix parAls similarFields $
            mkOrderByFieldName rn
        obAls = S.tableIdentifierToColumnAlias arrPfx <> "." <> mkAggregateOrderByAlias aggOrderBy
     in S.toColumnAlias obAls
  AOCComputedField cfOrderBy ->
    let fieldName = fromComputedField $ _cfobName cfOrderBy
     in case _cfobOrderByElement cfOrderBy of
          CFOBEScalar _ -> S.tableIdentifierToColumnAlias $ mkComputedFieldTableIdentifier tablePrefix fieldName
          CFOBETableAggregation _ _ aggOrderBy ->
            let cfPfx = mkComputedFieldTableIdentifier tablePrefix fieldName
                obAls = S.tableIdentifierToColumnAlias cfPfx <> "." <> mkAggregateOrderByAlias aggOrderBy
             in S.toColumnAlias obAls

mkObjectRelationTableAlias :: TableIdentifier -> RelName -> TableIdentifier
mkObjectRelationTableAlias pfx relName =
  pfx <> TableIdentifier (".or." <> relNameToTxt relName)

mkComputedFieldTableIdentifier :: TableIdentifier -> FieldName -> TableIdentifier
mkComputedFieldTableIdentifier pfx fldAls =
  pfx <> TableIdentifier ".cf." <> TableIdentifier (getFieldNameTxt fldAls)

mkBaseTableIdentifier :: TableIdentifier -> TableIdentifier
mkBaseTableIdentifier pfx = pfx <> TableIdentifier ".base"

mkBaseTableAlias :: S.TableAlias -> S.TableAlias
mkBaseTableAlias pfx = pfx <> ".base"

contextualizeBaseTableColumn :: TableIdentifier -> PGCol -> S.ColumnAlias
contextualizeBaseTableColumn pfx pgColumn =
  S.tableIdentifierToColumnAlias pfx <> ".pg." <> S.mkColumnAlias (getPGColTxt pgColumn)

mkAggregateOrderByAlias :: AnnotatedAggregateOrderBy ('Postgres pgKind) -> S.ColumnAlias
mkAggregateOrderByAlias =
  (S.toColumnAlias . Identifier) . \case
    AAOCount -> "count"
    AAOOp opText col -> opText <> "." <> getPGColTxt (ciColumn col)

mkOrderByFieldName :: ToTxt a => a -> FieldName
mkOrderByFieldName name =
  FieldName $ toTxt name <> "." <> "order_by"

mkArrayRelationSourcePrefix ::
  TableIdentifier ->
  FieldName ->
  HM.HashMap FieldName [FieldName] ->
  FieldName ->
  TableIdentifier
mkArrayRelationSourcePrefix parentSourcePrefix parentFieldName similarFieldsMap fieldName =
  mkArrayRelationTableIdentifier parentSourcePrefix parentFieldName $
    HM.lookupDefault [fieldName] fieldName similarFieldsMap

mkArrayRelationTableIdentifier :: TableIdentifier -> FieldName -> [FieldName] -> TableIdentifier
mkArrayRelationTableIdentifier pfx parAls flds =
  pfx <> TableIdentifier ".ar." <> TableIdentifier uniqArrRelAls
  where
    uniqArrRelAls = mkUniqArrayRelationAlias parAls flds

mkArrayRelationAlias ::
  FieldName ->
  HM.HashMap FieldName [FieldName] ->
  FieldName ->
  S.TableAlias
mkArrayRelationAlias parentFieldName similarFieldsMap fieldName =
  S.mkTableAlias $
    mkUniqArrayRelationAlias parentFieldName $
      HM.lookupDefault [fieldName] fieldName similarFieldsMap

-- array relationships are not grouped, so have to be prefixed by
-- parent's alias
mkUniqArrayRelationAlias :: FieldName -> [FieldName] -> Text
mkUniqArrayRelationAlias parAls flds =
  let sortedFields = sort flds
   in getFieldNameTxt parAls <> "."
        <> T.intercalate "." (map getFieldNameTxt sortedFields)
