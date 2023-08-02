-- | Stuff gutted from Translate.Select
module Hasura.Backends.Postgres.Translate.Select.Internal.JoinTree
  ( withWriteJoinTree,
    withWriteObjectRelation,
    withWriteArrayRelation,
    withWriteArrayConnection,
    withWriteComputedFieldTableSet,
  )
where

import Control.Monad.Writer.Strict
import Data.HashMap.Strict qualified as HashMap
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.Translate.Types
import Hasura.Prelude

-- | This is the lowest level function which deals with @MonadWriter SelectWriter@, which contains @JoinTree@ whose
-- purpose is to essentially create the selection tree across relationships.
--
-- Each type of relationship uses a different kind of update function; see
-- 'withWriteObjectRelation', 'withWriteArrayRelation', 'withWriteArrayConnection',
-- and 'withWriteComputedFieldTableSet'.
--
-- See the definition of 'JoinTree' for details before diving further
-- (particularly its components and Monoid instance).
withWriteJoinTree ::
  (MonadWriter SelectWriter m) =>
  (JoinTree -> b -> JoinTree) ->
  m (a, b) ->
  m a
withWriteJoinTree joinTreeUpdater action =
  pass $ do
    (out, result) <- action
    let fromSelectWriter =
          mapJoinTree (`joinTreeUpdater` result)
    pure (out, fromSelectWriter)

-- | change the `JoinTree` inside a `SelectWriter`
mapJoinTree :: (JoinTree -> JoinTree) -> SelectWriter -> SelectWriter
mapJoinTree f sw = sw {_swJoinTree = f (_swJoinTree sw)}

withWriteObjectRelation ::
  (MonadWriter SelectWriter m) =>
  m
    ( ObjectRelationSource,
      InsOrdHashMap S.ColumnAlias S.SQLExp,
      a
    ) ->
  m a
withWriteObjectRelation action =
  withWriteJoinTree updateJoinTree $ do
    (source, nodeExtractors, out) <- action
    pure (out, (source, nodeExtractors))
  where
    updateJoinTree joinTree (source, nodeExtractors) =
      let selectNode = SelectNode nodeExtractors joinTree
       in mempty {_jtObjectRelations = HashMap.singleton source selectNode}

withWriteArrayRelation ::
  (MonadWriter SelectWriter m) =>
  m
    ( ArrayRelationSource,
      S.Extractor,
      InsOrdHashMap S.ColumnAlias S.SQLExp,
      a
    ) ->
  m a
withWriteArrayRelation action =
  withWriteJoinTree updateJoinTree $ do
    (source, topExtractor, nodeExtractors, out) <- action
    pure (out, (source, topExtractor, nodeExtractors))
  where
    updateJoinTree joinTree (source, topExtractor, nodeExtractors) =
      let arraySelectNode =
            MultiRowSelectNode [topExtractor]
              $ SelectNode nodeExtractors joinTree
       in mempty {_jtArrayRelations = HashMap.singleton source arraySelectNode}

withWriteArrayConnection ::
  (MonadWriter SelectWriter m) =>
  m
    ( ArrayConnectionSource,
      S.Extractor,
      InsOrdHashMap S.ColumnAlias S.SQLExp,
      a
    ) ->
  m a
withWriteArrayConnection action =
  withWriteJoinTree updateJoinTree $ do
    (source, topExtractor, nodeExtractors, out) <- action
    pure (out, (source, topExtractor, nodeExtractors))
  where
    updateJoinTree joinTree (source, topExtractor, nodeExtractors) =
      let arraySelectNode =
            MultiRowSelectNode [topExtractor]
              $ SelectNode nodeExtractors joinTree
       in mempty {_jtArrayConnections = HashMap.singleton source arraySelectNode}

withWriteComputedFieldTableSet ::
  (MonadWriter SelectWriter m) =>
  m
    ( ComputedFieldTableSetSource,
      S.Extractor,
      InsOrdHashMap S.ColumnAlias S.SQLExp,
      a
    ) ->
  m a
withWriteComputedFieldTableSet action =
  withWriteJoinTree updateJoinTree $ do
    (source, topExtractor, nodeExtractors, out) <- action
    pure (out, (source, topExtractor, nodeExtractors))
  where
    updateJoinTree joinTree (source, topExtractor, nodeExtractors) =
      let selectNode = MultiRowSelectNode [topExtractor] $ SelectNode nodeExtractors joinTree
       in mempty {_jtComputedFieldTableSets = HashMap.singleton source selectNode}
