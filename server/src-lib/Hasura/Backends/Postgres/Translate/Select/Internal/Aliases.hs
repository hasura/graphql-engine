-- | Stuff gutted from Translate.Select
module Hasura.Backends.Postgres.Translate.Select.Internal.Aliases
  ( mkAggregateOrderByAlias,
    mkAnnOrderByAlias,
    mkArrayRelationAlias,
    mkArrayRelationSourcePrefix,
    mkBaseTableAlias,
    mkBaseTableColumnAlias,
    mkComputedFieldTableAlias,
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
import Hasura.RQL.Types

-- | Generate alias for order by extractors
mkAnnOrderByAlias ::
  Identifier -> FieldName -> SimilarArrayFields -> AnnotatedOrderByElement ('Postgres pgKind) v -> S.Alias
mkAnnOrderByAlias pfx parAls similarFields = \case
  AOCColumn pgColumnInfo ->
    let pgColumn = ciColumn pgColumnInfo
        obColAls = mkBaseTableColumnAlias pfx pgColumn
     in S.Alias obColAls
  -- "pfx.or.relname"."pfx.ob.or.relname.rest" AS "pfx.ob.or.relname.rest"
  AOCObjectRelation relInfo _ rest ->
    let rn = riName relInfo
        relPfx = mkObjectRelationTableAlias pfx rn
        ordByFldName = mkOrderByFieldName rn
        nesAls = mkAnnOrderByAlias relPfx ordByFldName mempty rest
     in nesAls
  AOCArrayAggregation relInfo _ aggOrderBy ->
    let rn = riName relInfo
        arrPfx =
          mkArrayRelationSourcePrefix pfx parAls similarFields $
            mkOrderByFieldName rn
        obAls = arrPfx <> Identifier "." <> toIdentifier (mkAggregateOrderByAlias aggOrderBy)
     in S.Alias obAls
  AOCComputedField cfOrderBy ->
    let fieldName = fromComputedField $ _cfobName cfOrderBy
     in case _cfobOrderByElement cfOrderBy of
          CFOBEScalar _ -> S.Alias $ mkComputedFieldTableAlias pfx fieldName
          CFOBETableAggregation _ _ aggOrderBy ->
            let cfPfx = mkComputedFieldTableAlias pfx fieldName
                obAls = cfPfx <> Identifier "." <> toIdentifier (mkAggregateOrderByAlias aggOrderBy)
             in S.Alias obAls

-- array relationships are not grouped, so have to be prefixed by
-- parent's alias
mkUniqArrayRelationAlias :: FieldName -> [FieldName] -> Identifier
mkUniqArrayRelationAlias parAls flds =
  let sortedFields = sort flds
   in Identifier $
        getFieldNameTxt parAls <> "."
          <> T.intercalate "." (map getFieldNameTxt sortedFields)

mkArrayRelationTableAlias :: Identifier -> FieldName -> [FieldName] -> Identifier
mkArrayRelationTableAlias pfx parAls flds =
  pfx <> Identifier ".ar." <> uniqArrRelAls
  where
    uniqArrRelAls = mkUniqArrayRelationAlias parAls flds

mkObjectRelationTableAlias :: Identifier -> RelName -> Identifier
mkObjectRelationTableAlias pfx relName =
  pfx <> Identifier ".or." <> toIdentifier relName

mkComputedFieldTableAlias :: Identifier -> FieldName -> Identifier
mkComputedFieldTableAlias pfx fldAls =
  pfx <> Identifier ".cf." <> toIdentifier fldAls

mkBaseTableAlias :: Identifier -> Identifier
mkBaseTableAlias pfx =
  pfx <> Identifier ".base"

mkBaseTableColumnAlias :: Identifier -> PGCol -> Identifier
mkBaseTableColumnAlias pfx pgColumn =
  pfx <> Identifier ".pg." <> toIdentifier pgColumn

mkAggregateOrderByAlias :: AnnotatedAggregateOrderBy ('Postgres pgKind) -> S.Alias
mkAggregateOrderByAlias =
  (S.Alias . Identifier) . \case
    AAOCount -> "count"
    AAOOp opText col -> opText <> "." <> getPGColTxt (ciColumn col)

mkOrderByFieldName :: ToTxt a => a -> FieldName
mkOrderByFieldName name =
  FieldName $ toTxt name <> "." <> "order_by"

mkArrayRelationSourcePrefix ::
  Identifier ->
  FieldName ->
  HM.HashMap FieldName [FieldName] ->
  FieldName ->
  Identifier
mkArrayRelationSourcePrefix parentSourcePrefix parentFieldName similarFieldsMap fieldName =
  mkArrayRelationTableAlias parentSourcePrefix parentFieldName $
    HM.lookupDefault [fieldName] fieldName similarFieldsMap

mkArrayRelationAlias ::
  FieldName ->
  HM.HashMap FieldName [FieldName] ->
  FieldName ->
  S.Alias
mkArrayRelationAlias parentFieldName similarFieldsMap fieldName =
  S.Alias $
    mkUniqArrayRelationAlias parentFieldName $
      HM.lookupDefault [fieldName] fieldName similarFieldsMap
