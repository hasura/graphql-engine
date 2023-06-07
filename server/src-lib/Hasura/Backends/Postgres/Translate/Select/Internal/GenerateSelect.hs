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

import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.List.NonEmpty qualified as NE
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Types
import Hasura.Backends.Postgres.SQL.Types qualified as S
import Hasura.Backends.Postgres.Translate.Select.Internal.Aliases (mkBaseTableAlias)
import Hasura.Backends.Postgres.Translate.Select.Internal.Helpers
  ( cursorIdentifier,
    cursorsSelectAlias,
    cursorsSelectAliasIdentifier,
    endCursorIdentifier,
    hasNextPageIdentifier,
    hasPreviousPageIdentifier,
    mkFirstElementExp,
    mkLastElementExp,
    pageInfoSelectAlias,
    startCursorIdentifier,
  )
import Hasura.Backends.Postgres.Translate.Types
import Hasura.Prelude
import Hasura.RQL.IR.Select (ConnectionSlice (SliceFirst, SliceLast))
import Hasura.RQL.Types.Relationships.Local (Nullable (..))

generateSQLSelect ::
  -- | Pre join condition for lateral joins
  S.BoolExp ->
  SelectSource ->
  SelectNode ->
  S.Select
generateSQLSelect joinCondition selectSource selectNode =
  S.mkSelect
    { S.selExtr =
        case [S.Extractor e $ Just a | (a, e) <- InsOrdHashMap.toList extractors] of
          -- If the select list is empty we will generated code which looks like this:
          -- > SELECT FROM ...
          -- This works for postgres, but not for cockroach, which expects a non-empty
          -- select list. So we add a dummy value:
          [] -> S.dummySelectList -- SELECT 1 FROM ...
          exts -> exts,
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
    -- This is why 'SelectSource{sourcePrefix}' needs to be a TableAlias!
    baseSelectAlias = mkBaseTableAlias (S.toTableAlias sourcePrefix)
    baseSelectIdentifier = S.tableAliasToIdentifier baseSelectAlias
    baseFromItem = S.mkSelFromItem baseSelect baseSelectAlias

    injectJoinCond ::
      S.BoolExp -> -- Join condition
      S.BoolExp -> -- Where condition
      S.WhereFrag -- New where frag
    injectJoinCond joinCond whereCond =
      S.WhereFrag $ S.simplifyBoolExp $ S.BEBin S.AndOp joinCond whereCond

    -- function to create a joined from item from two from items
    leftOuterJoin current (new, joinType) =
      S.FIJoin
        $ S.JoinExpr current joinType new
        $ S.JoinOn
        $ S.BELit True

    -- this is the from eexp for the final select
    joinedFrom :: S.FromItem
    joinedFrom =
      foldl' leftOuterJoin baseFromItem
        $ map objectRelationToFromItem (HashMap.toList objectRelations)
        <> map arrayRelationToFromItem (HashMap.toList arrayRelations)
        <> map arrayConnectionToFromItem (HashMap.toList arrayConnections)
        <> map computedFieldToFromItem (HashMap.toList computedFields)

    objectRelationToFromItem ::
      (ObjectRelationSource, SelectNode) -> (S.FromItem, S.JoinType)
    objectRelationToFromItem (objectRelationSource, node) =
      let ObjectRelationSource
            { _orsRelationMapping = colMapping,
              _orsSelectSource = objectSelectSource,
              _orsNullable = nullable
            } = objectRelationSource
          alias = S.toTableAlias $ _ossPrefix objectSelectSource
          source = objectSelectSourceToSelectSource objectSelectSource
          select = generateSQLSelect (mkJoinCond baseSelectIdentifier colMapping) source node
          joinType = case nullable of
            Nullable -> S.LeftOuter
            NotNullable -> S.Inner
       in (S.mkLateralFromItem select alias, joinType)

    arrayRelationToFromItem ::
      (ArrayRelationSource, MultiRowSelectNode) -> (S.FromItem, S.JoinType)
    arrayRelationToFromItem (arrayRelationSource, arraySelectNode) =
      let ArrayRelationSource _ colMapping source = arrayRelationSource
          alias = S.toTableAlias $ _ssPrefix source
          select =
            generateSQLSelectFromArrayNode source arraySelectNode
              $ mkJoinCond baseSelectIdentifier colMapping
       in (S.mkLateralFromItem select alias, S.LeftOuter)

    arrayConnectionToFromItem ::
      (ArrayConnectionSource, MultiRowSelectNode) -> (S.FromItem, S.JoinType)
    arrayConnectionToFromItem (arrayConnectionSource, arraySelectNode) =
      let selectWith = connectionToSelectWith baseSelectAlias arrayConnectionSource arraySelectNode
          alias = S.toTableAlias $ _ssPrefix $ _acsSource arrayConnectionSource
       in (S.FISelectWith (S.Lateral True) selectWith alias, S.LeftOuter)

    computedFieldToFromItem ::
      (ComputedFieldTableSetSource, MultiRowSelectNode) -> (S.FromItem, S.JoinType)
    computedFieldToFromItem (computedFieldTableSource, node) =
      let ComputedFieldTableSetSource _ source = computedFieldTableSource
          internalSelect = generateSQLSelect (S.BELit True) source $ _mrsnSelectNode node
          alias = S.toTableAlias $ _ssPrefix source
          select =
            S.mkSelect
              { S.selExtr = _mrsnTopExtractors node,
                S.selFrom = Just $ S.FromExp [S.mkSelFromItem internalSelect alias]
              }
       in (S.mkLateralFromItem select alias, S.LeftOuter)

-- | Create a select query
generateSQLSelectFromArrayNode ::
  SelectSource ->
  MultiRowSelectNode ->
  S.BoolExp ->
  S.Select
generateSQLSelectFromArrayNode selectSource (MultiRowSelectNode topExtractors selectNode) joinCondition =
  S.mkSelect
    { S.selExtr = topExtractors,
      S.selFrom =
        Just
          $ S.FromExp
            [ S.mkSelFromItem
                (generateSQLSelect joinCondition selectSource selectNode)
                $ S.toTableAlias
                $ _ssPrefix selectSource
            ]
    }

mkJoinCond :: S.TableIdentifier -> HashMap PGCol PGCol -> S.BoolExp
mkJoinCond baseTablepfx colMapn =
  foldl' (S.BEBin S.AndOp) (S.BELit True)
    $ flip map (HashMap.toList colMapn)
    $ \(lCol, rCol) ->
      S.BECompare S.SEQ (S.mkQIdenExp baseTablepfx lCol) (S.mkSIdenExp rCol)

connectionToSelectWith ::
  S.TableAlias ->
  ArrayConnectionSource ->
  MultiRowSelectNode ->
  S.SelectWithG S.Select
connectionToSelectWith rootSelectAlias arrayConnectionSource arraySelectNode =
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

    rootSelectIdentifier = S.tableAliasToIdentifier rootSelectAlias

    baseSelectAlias = S.mkTableAlias "__base_select"
    baseSelectIdentifier = S.tableAliasToIdentifier baseSelectAlias

    splitSelectAlias = S.mkTableAlias "__split_select"
    splitSelectIdentifier = S.tableAliasToIdentifier splitSelectAlias

    sliceSelectAlias = S.mkTableAlias "__slice_select"
    sliceSelectIdentifier = S.tableAliasToIdentifier sliceSelectAlias

    finalSelectAlias = S.mkTableAlias "__final_select"
    finalSelectIdentifier = S.tableAliasToIdentifier finalSelectAlias

    rowNumberIdentifier = Identifier "__row_number"
    rowNumberExp = S.SEUnsafe "(row_number() over (partition by 1))"
    startRowNumberIdentifier = Identifier "__start_row_number"
    endRowNumberIdentifier = Identifier "__end_row_number"

    startCursorExp = mkFirstElementExp $ S.SEIdentifier cursorIdentifier
    endCursorExp = mkLastElementExp $ S.SEIdentifier cursorIdentifier

    startRowNumberExp = mkFirstElementExp $ S.SEIdentifier rowNumberIdentifier
    endRowNumberExp = mkLastElementExp $ S.SEIdentifier rowNumberIdentifier

    fromBaseSelections =
      let joinCond = mkJoinCond rootSelectIdentifier columnMapping
          baseSelectFrom =
            S.mkSelFromItem
              (generateSQLSelect joinCond selectSource selectNode)
              $ S.toTableAlias
              $ _ssPrefix selectSource
          select =
            S.mkSelect
              { S.selExtr =
                  [ S.selectStar,
                    S.Extractor rowNumberExp $ Just $ S.toColumnAlias rowNumberIdentifier
                  ],
                S.selFrom = Just $ S.FromExp [baseSelectFrom]
              }
       in (baseSelectAlias, select) : fromSplitSelection

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
         in (splitSelectAlias, select) : fromSliceSelection splitSelectIdentifier

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
                      S.mkSelFromItem sliceLastSelect $ sliceSelectAlias
                 in S.mkSelect
                      { S.selExtr = [S.selectStar],
                        S.selFrom = Just $ S.FromExp [sliceLastSelectFrom],
                        S.selOrderBy = Just $ mkRowNumberOrderBy S.OTAsc
                      }
         in (sliceSelectAlias, select) : fromFinalSelect sliceSelectIdentifier

    fromFinalSelect prevSelect =
      let select = mkStarSelect prevSelect
       in (finalSelectAlias, select) : fromCursorSelection

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
       in (cursorsSelectAlias, select) : fromPageInfoSelection

    fromPageInfoSelection =
      let hasPrevPage =
            S.SEBool
              $ S.mkExists (S.FIIdentifier baseSelectIdentifier)
              $ S.BECompare S.SLT (S.SEIdentifier rowNumberIdentifier)
              $ S.SESelect
              $ S.mkSelect
                { S.selFrom = Just $ S.FromExp [S.FIIdentifier cursorsSelectAliasIdentifier],
                  S.selExtr = [S.Extractor (S.SEIdentifier startRowNumberIdentifier) Nothing]
                }
          hasNextPage =
            S.SEBool
              $ S.mkExists (S.FIIdentifier baseSelectIdentifier)
              $ S.BECompare S.SGT (S.SEIdentifier rowNumberIdentifier)
              $ S.SESelect
              $ S.mkSelect
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
       in pure (pageInfoSelectAlias, select)
