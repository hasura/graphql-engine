-- | This module contains worker definitions pertaining to generating 'Select'
-- AST nodes.
--
-- NOTE: 'generateSQLSelect' is mutually recursive with
-- 'connectionToSelectWith', thus these two cohabitate in this module.
module Hasura.Backends.Postgres.Translate.Select.Internal.GenerateSelect
  ( generateSQLSelect,
    generateSQLSelectFromArrayNode,
    connectionToSelectWith,
  )
where

import Data.HashMap.Strict qualified as HM
import Data.List.NonEmpty qualified as NE
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Types
import Hasura.Backends.Postgres.Translate.Select.Internal.Aliases (mkBaseTableAlias)
import Hasura.Backends.Postgres.Translate.Select.Internal.Helpers
  ( cursorIdentifier,
    cursorsSelectAliasIdentifier,
    endCursorIdentifier,
    hasNextPageIdentifier,
    hasPreviousPageIdentifier,
    mkFirstElementExp,
    mkLastElementExp,
    pageInfoSelectAliasIdentifier,
    startCursorIdentifier,
  )
import Hasura.Backends.Postgres.Translate.Types
import Hasura.Prelude
import Hasura.RQL.IR.Select (ConnectionSlice (SliceFirst, SliceLast))

generateSQLSelect ::
  -- | Pre join condition
  S.BoolExp ->
  SelectSource ->
  SelectNode ->
  S.Select
generateSQLSelect joinCondition selectSource selectNode =
  S.mkSelect
    { S.selExtr = [S.Extractor e $ Just a | (a, e) <- HM.toList extractors],
      S.selFrom = Just $ S.FromExp [joinedFrom],
      S.selOrderBy = nodeOrderBy,
      S.selLimit = S.LimitExp . S.intToSQLExp <$> _ssLimit nodeSlicing,
      S.selOffset = S.OffsetExp . S.int64ToSQLExp <$> _ssOffset nodeSlicing,
      S.selDistinct = nodeDistinctOn
    }
  where
    SelectSource sourcePrefix fromItem whereExp sortAndSlice = selectSource
    SelectNode extractors joinTree = selectNode
    JoinTree objectRelations arrayRelations arrayConnections computedFields = joinTree
    ApplySortingAndSlicing
      (baseOrderBy, baseSlicing, baseDistinctOn)
      (nodeOrderBy, nodeSlicing, nodeDistinctOn) = applySortingAndSlicing sortAndSlice

    -- this is the table which is aliased as "sourcePrefix.base"
    baseSelect =
      S.mkSelect
        { S.selExtr = [S.Extractor (S.SEStar Nothing) Nothing],
          S.selFrom = Just $ S.FromExp [fromItem],
          S.selWhere = Just $ injectJoinCond joinCondition whereExp,
          S.selOrderBy = baseOrderBy,
          S.selLimit = S.LimitExp . S.intToSQLExp <$> _ssLimit baseSlicing,
          S.selOffset = S.OffsetExp . S.int64ToSQLExp <$> _ssOffset baseSlicing,
          S.selDistinct = baseDistinctOn
        }
    baseSelectAlias = mkBaseTableAlias sourcePrefix
    baseFromItem = S.mkSelFromItem baseSelect baseSelectAlias

    injectJoinCond ::
      S.BoolExp -> -- Join condition
      S.BoolExp -> -- Where condition
      S.WhereFrag -- New where frag
    injectJoinCond joinCond whereCond =
      S.WhereFrag $ S.simplifyBoolExp $ S.BEBin S.AndOp joinCond whereCond

    -- function to create a joined from item from two from items
    leftOuterJoin current new =
      S.FIJoin $
        S.JoinExpr current S.LeftOuter new $
          S.JoinOn $ S.BELit True

    -- this is the from eexp for the final select
    joinedFrom :: S.FromItem
    joinedFrom =
      foldl' leftOuterJoin baseFromItem $
        map objectRelationToFromItem (HM.toList objectRelations)
          <> map arrayRelationToFromItem (HM.toList arrayRelations)
          <> map arrayConnectionToFromItem (HM.toList arrayConnections)
          <> map computedFieldToFromItem (HM.toList computedFields)

    objectRelationToFromItem ::
      (ObjectRelationSource, SelectNode) -> S.FromItem
    objectRelationToFromItem (objectRelationSource, node) =
      let ObjectRelationSource _ colMapping objectSelectSource = objectRelationSource
          alias = S.toTableAlias $ _ossPrefix objectSelectSource
          source = objectSelectSourceToSelectSource objectSelectSource
          select = generateSQLSelect (mkJoinCond baseSelectAlias colMapping) source node
       in S.mkLateralFromItem select alias

    arrayRelationToFromItem ::
      (ArrayRelationSource, MultiRowSelectNode) -> S.FromItem
    arrayRelationToFromItem (arrayRelationSource, arraySelectNode) =
      let ArrayRelationSource _ colMapping source = arrayRelationSource
          alias = S.toTableAlias $ _ssPrefix source
          select =
            generateSQLSelectFromArrayNode source arraySelectNode $
              mkJoinCond baseSelectAlias colMapping
       in S.mkLateralFromItem select alias

    arrayConnectionToFromItem ::
      (ArrayConnectionSource, MultiRowSelectNode) -> S.FromItem
    arrayConnectionToFromItem (arrayConnectionSource, arraySelectNode) =
      let selectWith = connectionToSelectWith baseSelectAlias arrayConnectionSource arraySelectNode
          alias = S.toTableAlias $ _ssPrefix $ _acsSource arrayConnectionSource
       in S.FISelectWith (S.Lateral True) selectWith alias

    computedFieldToFromItem ::
      (ComputedFieldTableSetSource, MultiRowSelectNode) -> S.FromItem
    computedFieldToFromItem (computedFieldTableSource, node) =
      let ComputedFieldTableSetSource _ source = computedFieldTableSource
          internalSelect = generateSQLSelect (S.BELit True) source $ _mrsnSelectNode node
          alias = S.toTableAlias $ _ssPrefix source
          select =
            S.mkSelect
              { S.selExtr = _mrsnTopExtractors node,
                S.selFrom = Just $ S.FromExp [S.mkSelFromItem internalSelect alias]
              }
       in S.mkLateralFromItem select alias

generateSQLSelectFromArrayNode ::
  SelectSource ->
  MultiRowSelectNode ->
  S.BoolExp ->
  S.Select
generateSQLSelectFromArrayNode selectSource arraySelectNode joinCondition =
  S.mkSelect
    { S.selExtr = topExtractors,
      S.selFrom = Just $ S.FromExp [selectFrom]
    }
  where
    MultiRowSelectNode topExtractors selectNode = arraySelectNode
    selectFrom =
      S.mkSelFromItem
        (generateSQLSelect joinCondition selectSource selectNode)
        $ S.toTableAlias $ _ssPrefix selectSource

mkJoinCond :: S.TableAlias -> HashMap PGCol PGCol -> S.BoolExp
mkJoinCond baseTablepfx colMapn =
  foldl' (S.BEBin S.AndOp) (S.BELit True) $
    flip map (HM.toList colMapn) $ \(lCol, rCol) ->
      S.BECompare S.SEQ (S.mkQIdenExp baseTablepfx lCol) (S.mkSIdenExp rCol)

connectionToSelectWith ::
  S.TableAlias ->
  ArrayConnectionSource ->
  MultiRowSelectNode ->
  S.SelectWithG S.Select
connectionToSelectWith baseSelectAlias arrayConnectionSource arraySelectNode =
  let extractionSelect =
        S.mkSelect
          { S.selExtr = topExtractors,
            S.selFrom = Just $ S.FromExp [S.FIIdentifier finalSelectIdentifier]
          }
   in S.SelectWith fromBaseSelections extractionSelect
  where
    ArrayConnectionSource _ columnMapping maybeSplit maybeSlice selectSource =
      arrayConnectionSource
    MultiRowSelectNode topExtractors selectNode = arraySelectNode
    baseSelectIdentifier = Identifier "__base_select"
    splitSelectIdentifier = Identifier "__split_select"
    sliceSelectIdentifier = Identifier "__slice_select"
    finalSelectIdentifier = Identifier "__final_select"

    rowNumberIdentifier = Identifier "__row_number"
    rowNumberExp = S.SEUnsafe "(row_number() over (partition by 1))"
    startRowNumberIdentifier = Identifier "__start_row_number"
    endRowNumberIdentifier = Identifier "__end_row_number"

    startCursorExp = mkFirstElementExp $ S.SEIdentifier cursorIdentifier
    endCursorExp = mkLastElementExp $ S.SEIdentifier cursorIdentifier

    startRowNumberExp = mkFirstElementExp $ S.SEIdentifier rowNumberIdentifier
    endRowNumberExp = mkLastElementExp $ S.SEIdentifier rowNumberIdentifier

    fromBaseSelections =
      let joinCond = mkJoinCond baseSelectAlias columnMapping
          baseSelectFrom =
            S.mkSelFromItem
              (generateSQLSelect joinCond selectSource selectNode)
              $ S.toTableAlias $ _ssPrefix selectSource
          select =
            S.mkSelect
              { S.selExtr =
                  [ S.selectStar,
                    S.Extractor rowNumberExp $ Just $ S.toColumnAlias rowNumberIdentifier
                  ],
                S.selFrom = Just $ S.FromExp [baseSelectFrom]
              }
       in (S.toTableAlias baseSelectIdentifier, select) : fromSplitSelection

    mkStarSelect fromIdentifier =
      S.mkSelect
        { S.selExtr = [S.selectStar],
          S.selFrom = Just $ S.FromExp [S.FIIdentifier fromIdentifier]
        }

    fromSplitSelection = case maybeSplit of
      Nothing -> fromSliceSelection baseSelectIdentifier
      Just splitBool ->
        let select =
              (mkStarSelect baseSelectIdentifier) {S.selWhere = Just $ S.WhereFrag splitBool}
         in (S.toTableAlias splitSelectIdentifier, select) : fromSliceSelection splitSelectIdentifier

    fromSliceSelection prevSelect = case maybeSlice of
      Nothing -> fromFinalSelect prevSelect
      Just slice ->
        let select = case slice of
              SliceFirst limit ->
                (mkStarSelect prevSelect)
                  { S.selLimit = (Just . S.LimitExp . S.intToSQLExp) limit
                  }
              SliceLast limit ->
                let mkRowNumberOrderBy obType =
                      let orderByItem =
                            S.OrderByItem (S.SEIdentifier rowNumberIdentifier) (Just obType) Nothing
                       in S.OrderByExp $ orderByItem NE.:| []

                    sliceLastSelect =
                      (mkStarSelect prevSelect)
                        { S.selLimit = (Just . S.LimitExp . S.intToSQLExp) limit,
                          S.selOrderBy = Just $ mkRowNumberOrderBy S.OTDesc
                        }
                    sliceLastSelectFrom =
                      S.mkSelFromItem sliceLastSelect $ S.toTableAlias sliceSelectIdentifier
                 in S.mkSelect
                      { S.selExtr = [S.selectStar],
                        S.selFrom = Just $ S.FromExp [sliceLastSelectFrom],
                        S.selOrderBy = Just $ mkRowNumberOrderBy S.OTAsc
                      }
         in (S.toTableAlias sliceSelectIdentifier, select) : fromFinalSelect sliceSelectIdentifier

    fromFinalSelect prevSelect =
      let select = mkStarSelect prevSelect
       in (S.toTableAlias finalSelectIdentifier, select) : fromCursorSelection

    fromCursorSelection =
      let extrs =
            [ S.Extractor startCursorExp $ Just $ S.toColumnAlias startCursorIdentifier,
              S.Extractor endCursorExp $ Just $ S.toColumnAlias endCursorIdentifier,
              S.Extractor startRowNumberExp $ Just $ S.toColumnAlias startRowNumberIdentifier,
              S.Extractor endRowNumberExp $ Just $ S.toColumnAlias endRowNumberIdentifier
            ]
          select =
            S.mkSelect
              { S.selExtr = extrs,
                S.selFrom = Just $ S.FromExp [S.FIIdentifier finalSelectIdentifier]
              }
       in (S.toTableAlias cursorsSelectAliasIdentifier, select) : fromPageInfoSelection

    fromPageInfoSelection =
      let hasPrevPage =
            S.SEBool $
              S.mkExists (S.FIIdentifier baseSelectIdentifier) $
                S.BECompare S.SLT (S.SEIdentifier rowNumberIdentifier) $
                  S.SESelect $
                    S.mkSelect
                      { S.selFrom = Just $ S.FromExp [S.FIIdentifier cursorsSelectAliasIdentifier],
                        S.selExtr = [S.Extractor (S.SEIdentifier startRowNumberIdentifier) Nothing]
                      }
          hasNextPage =
            S.SEBool $
              S.mkExists (S.FIIdentifier baseSelectIdentifier) $
                S.BECompare S.SGT (S.SEIdentifier rowNumberIdentifier) $
                  S.SESelect $
                    S.mkSelect
                      { S.selFrom = Just $ S.FromExp [S.FIIdentifier cursorsSelectAliasIdentifier],
                        S.selExtr = [S.Extractor (S.SEIdentifier endRowNumberIdentifier) Nothing]
                      }

          select =
            S.mkSelect
              { S.selExtr =
                  [ S.Extractor hasPrevPage $ Just $ S.toColumnAlias hasPreviousPageIdentifier,
                    S.Extractor hasNextPage $ Just $ S.toColumnAlias hasNextPageIdentifier
                  ]
              }
       in pure (S.toTableAlias pageInfoSelectAliasIdentifier, select)
