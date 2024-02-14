-- | This module contains worker definitions pertaining to generating 'Select'
-- AST nodes.
--
-- NOTE: 'generateSQLSelect' is mutually recursive with
-- 'connectionToSelectWith', thus these two cohabitate in this module.
module Hasura.Backends.Postgres.Translate.Select.Internal.GenerateSelect
  ( PostgresGenerateSQLSelect,
    generateSQLSelect,
    generateSQLSelectFromArrayNode,
    connectionToSelectWith,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.HashSet qualified as HashSet
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
import Hasura.RQL.Types.BackendType (PostgresKind (..))
import Hasura.RQL.Types.Relationships.Local (Nullable (..))

class PostgresGenerateSQLSelect (pgKind :: PostgresKind) where
  generateSQLSelect :: S.BoolExp -> SelectSource -> SelectNode -> S.Select

instance PostgresGenerateSQLSelect 'Vanilla where
  generateSQLSelect = defaultGenerateSQLSelect @('Vanilla) id

instance PostgresGenerateSQLSelect 'Citus where
  generateSQLSelect = defaultGenerateSQLSelect @('Citus) id

instance PostgresGenerateSQLSelect 'Cockroach where
  generateSQLSelect = defaultGenerateSQLSelect @('Cockroach) applyCockroachDistinctOnWorkaround

-- This function rewrites a select statement that uses DISTINCT ON where
-- the DISTINCT ON references CASE ... END expressions to a nested select
-- to work around a Cockroach deficiency where the query fails if these are present.
-- The issue has been reported to Cockroach here: https://github.com/cockroachdb/cockroach/issues/107516
--
-- We are rewriting SQL of the form
-- ```
-- SELECT DISTINCT ON (<exprA>, <exprB>, ...) <extractors>
-- FROM table
-- ORDER BY <exprA>, <exprB>, ...
-- OFFSET <offset>
-- LIMIT <limit>
-- ```
-- into
-- ```
-- SELECT DISTINCT ON ("_hasura_cr_do_wa_1", "_hasura_cr_do_wa_2", ...) *
-- FROM (
--   SELECT
--     <exprA> AS "_hasura_cr_do_wa_1",
--     <exprB> AS "_hasura_cr_do_wa_2",
--     <extractors>
--   FROM table
-- ) AS "_hasura_cockroach_distinct_on_workaround"
-- ORDER BY "_hasura_cr_do_wa_1", "_hasura_cr_do_wa_2", ...
-- OFFSET <offset>
-- LIMIT <limit>
-- ```
applyCockroachDistinctOnWorkaround :: S.Select -> S.Select
applyCockroachDistinctOnWorkaround select =
  case S.selDistinct select of
    Just (S.DistinctOn distinctExps)
      | any (\expr -> isCondExp expr || isReferencingExtractorCondExp expr) distinctExps ->
          let orderByExps = maybe [] getOrderByExps $ S.selOrderBy select
              -- Create extractors for all expressions used in order by and distinct
              -- that are not direct references to existing extractors in the select
              replacementExtractors =
                HashSet.fromList (distinctExps <> orderByExps)
                  & HashSet.toList
                  & zip [1 ..]
                  & mapMaybe
                    ( \((index :: Int), expr) ->
                        if isReferencingExtractor expr
                          then -- If the distinct or order by expression is already referencing an extractor, then we don't need to generate
                          -- an extractor to hold the expression to use it in the wrapping select. It is already available as the existing
                          -- extractor.
                            Nothing
                          else Just (expr, (S.mkColumnAlias $ "_hasura_cr_do_wa_" <> tshow index))
                    )
              innerSelect =
                S.mkSelFromItem
                  select
                    { S.selExtr = ((\(expr, alias) -> S.Extractor expr (Just alias)) <$> replacementExtractors) <> S.selExtr select,
                      S.selOrderBy = Nothing,
                      S.selDistinct = Nothing,
                      S.selLimit = Nothing,
                      S.selOffset = Nothing
                    }
                  (S.mkTableAlias "_hasura_cockroach_distinct_on_workaround")
              rewrittenOrderBy =
                S.selOrderBy select
                  <&> \(S.OrderByExp items) -> S.OrderByExp $ replaceOrderByItem replacementExtractors <$> items
           in S.mkSelect
                { S.selExtr = [S.Extractor (S.SEStar Nothing) Nothing],
                  S.selFrom = Just $ S.FromExp [innerSelect],
                  S.selOrderBy = rewrittenOrderBy,
                  S.selDistinct = Just (S.DistinctOn $ replaceExp replacementExtractors <$> distinctExps),
                  S.selLimit = S.selLimit select,
                  S.selOffset = S.selOffset select
                }
    _ -> select
  where
    -- Is the expression a CASE ... END expression?
    isCondExp :: S.SQLExp -> Bool
    isCondExp = \case S.SECond {} -> True; _ -> False

    -- Is the expression referencing an Extractor from the Select that is a CASE ... END expression?
    isReferencingExtractorCondExp :: S.SQLExp -> Bool
    isReferencingExtractorCondExp = \case
      S.SEIdentifier identifier -> HashSet.member identifier condExtractorIdentifiers
      _ -> False

    -- Is the expression referencing an Extractor from the Select?
    isReferencingExtractor :: S.SQLExp -> Bool
    isReferencingExtractor = \case
      S.SEIdentifier identifier -> HashSet.member identifier extractorIdentifiers
      _ -> False

    condExtractorIdentifiers :: HashSet S.Identifier
    condExtractorIdentifiers =
      HashSet.fromList $ flip mapMaybe (S.selExtr select) $ \case
        (S.Extractor (S.SECond {}) alias) -> S.toIdentifier <$> alias
        _ -> Nothing

    extractorIdentifiers :: HashSet S.Identifier
    extractorIdentifiers = HashSet.fromList $ mapMaybe (\(S.Extractor _ alias) -> S.toIdentifier <$> alias) (S.selExtr select)

    getOrderByExps :: S.OrderByExp -> [S.SQLExp]
    getOrderByExps (S.OrderByExp items) = toList $ S.oExpression <$> items

    replaceExp :: [(S.SQLExp, S.ColumnAlias)] -> S.SQLExp -> S.SQLExp
    replaceExp replacementExtractors expr =
      maybe expr S.mkSIdenExp $ lookup expr replacementExtractors

    replaceOrderByItem :: [(S.SQLExp, S.ColumnAlias)] -> S.OrderByItem -> S.OrderByItem
    replaceOrderByItem replacementExtractors orderByItem =
      orderByItem {S.oExpression = replaceExp replacementExtractors (S.oExpression orderByItem)}

defaultGenerateSQLSelect ::
  forall pgKind.
  (PostgresGenerateSQLSelect pgKind) =>
  -- Function to post-process the SelectNode SELECT and the base table SELECT
  (S.Select -> S.Select) ->
  -- | Pre join condition for lateral joins
  S.BoolExp ->
  SelectSource ->
  SelectNode ->
  S.Select
defaultGenerateSQLSelect selectRewriter joinCondition selectSource selectNode =
  selectRewriter
    $ S.mkSelect
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
      selectRewriter
        $ S.mkSelect
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
          select = generateSQLSelect @pgKind (mkJoinCond baseSelectIdentifier colMapping) source node
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
            generateSQLSelectFromArrayNode @pgKind source arraySelectNode
              $ mkJoinCond baseSelectIdentifier colMapping
       in (S.mkLateralFromItem select alias, S.LeftOuter)

    arrayConnectionToFromItem ::
      (ArrayConnectionSource, MultiRowSelectNode) -> (S.FromItem, S.JoinType)
    arrayConnectionToFromItem (arrayConnectionSource, arraySelectNode) =
      let selectWith = connectionToSelectWith @pgKind baseSelectAlias arrayConnectionSource arraySelectNode
          alias = S.toTableAlias $ _ssPrefix $ _acsSource arrayConnectionSource
       in (S.FISelectWith (S.Lateral True) selectWith alias, S.LeftOuter)

    computedFieldToFromItem ::
      (ComputedFieldTableSetSource, MultiRowSelectNode) -> (S.FromItem, S.JoinType)
    computedFieldToFromItem (computedFieldTableSource, node) =
      let ComputedFieldTableSetSource _ source = computedFieldTableSource
          internalSelect = generateSQLSelect @pgKind (S.BELit True) source $ _mrsnSelectNode node
          alias = S.toTableAlias $ _ssPrefix source
          select =
            S.mkSelect
              { S.selExtr = _mrsnTopExtractors node,
                S.selFrom = Just $ S.FromExp [S.mkSelFromItem internalSelect alias]
              }
       in (S.mkLateralFromItem select alias, S.LeftOuter)

-- | Create a select query
generateSQLSelectFromArrayNode ::
  forall pgKind.
  (PostgresGenerateSQLSelect pgKind) =>
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
                (generateSQLSelect @pgKind joinCondition selectSource selectNode)
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
  forall pgKind.
  (PostgresGenerateSQLSelect pgKind) =>
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
              (generateSQLSelect @pgKind joinCond selectSource selectNode)
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
