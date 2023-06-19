{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.IR.Select.Lenses
  ( asnArgs,
    asnFields,
    asnFrom,
    asnNamingConvention,
    asnPerm,
    asnStrfyNum,
    saDistinct,
    saLimit,
    saOffset,
    saOrderBy,
    saWhere,
    aarAnnSelect,
    aarColumnMapping,
    aarRelationshipName,
    aarNullable,
    anosSupportsNestedObjects,
    anosColumn,
    anosFields,
    aosFields,
    aosTarget,
    aosTargetFilter,
    csXRelay,
    csPrimaryKeyColumns,
    csSplit,
    csSlice,
    csSelect,
    gbgFields,
    gbgKeys,
    insertFunctionArg,
    mkAnnColumnField,
    mkAnnColumnFieldAsText,
    traverseSourceRelationshipSelection,
    _AOCColumn,
    _AOCObjectRelation,
    _AOCArrayAggregation,
    _AOCComputedField,
    _AFArrayRelation,
    _AFColumn,
    _AFComputedField,
    _AFExpression,
    _AFNodeId,
    _AFObjectRelation,
    _AFRemote,
    _TAFAgg,
    _TAFNodes,
    _TAFExp,
    _GBFGroupKey,
    _GBFAggregate,
    _GBFNodes,
    _GBFExp,
    _ConnectionTypename,
    _ConnectionPageInfo,
    _ConnectionEdges,
    _EdgeTypename,
    _EdgeCursor,
    _EdgeNode,
  )
where

import Control.Lens.TH (makeLenses, makePrisms)
import Hasura.RQL.IR.Select

-- Lenses

$(makeLenses ''AnnSelectG)
$(makeLenses ''SelectArgsG)
$(makeLenses ''AnnObjectSelectG)
$(makeLenses ''AnnNestedObjectSelectG)
$(makeLenses ''ConnectionSelect)
$(makeLenses ''AnnRelationSelectG)
$(makeLenses ''GroupByG)

$(makePrisms ''AnnotatedOrderByElement)
$(makePrisms ''AnnFieldG)
$(makePrisms ''TableAggregateFieldG)
$(makePrisms ''GroupByField)
$(makePrisms ''ConnectionField)
$(makePrisms ''EdgeField)
