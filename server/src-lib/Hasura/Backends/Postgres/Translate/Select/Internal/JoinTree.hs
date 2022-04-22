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
import Data.HashMap.Strict qualified as HM
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.Translate.Types
import Hasura.Prelude

-- | This is the lowest level function which deals with @MonadWriter JoinTree@, whose
-- purpose is to essentially create the selection tree across relationships.
--
-- Each type of relationship uses a different kind of update function; see
-- 'withWriteObjectRelation', 'withWriteArrayRelation', 'withWriteArrayConnection',
-- and 'withWriteComputedFieldTableSet'.
--
-- See the definition of 'JoinTree' for details before diving further
-- (particularly its components and Monoid instance).
withWriteJoinTree ::
  (MonadWriter JoinTree m) =>
  (JoinTree -> b -> JoinTree) ->
  m (a, b) ->
  m a
withWriteJoinTree joinTreeUpdater action =
  pass $ do
    (out, result) <- action
    let fromJoinTree joinTree =
          joinTreeUpdater joinTree result
    pure (out, fromJoinTree)

withWriteObjectRelation ::
  (MonadWriter JoinTree m) =>
  m
    ( ObjectRelationSource,
      HM.HashMap S.Alias S.SQLExp,
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
       in mempty {_jtObjectRelations = HM.singleton source selectNode}

withWriteArrayRelation ::
  (MonadWriter JoinTree m) =>
  m
    ( ArrayRelationSource,
      S.Extractor,
      HM.HashMap S.Alias S.SQLExp,
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
            MultiRowSelectNode [topExtractor] $
              SelectNode nodeExtractors joinTree
       in mempty {_jtArrayRelations = HM.singleton source arraySelectNode}

withWriteArrayConnection ::
  (MonadWriter JoinTree m) =>
  m
    ( ArrayConnectionSource,
      S.Extractor,
      HM.HashMap S.Alias S.SQLExp,
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
            MultiRowSelectNode [topExtractor] $
              SelectNode nodeExtractors joinTree
       in mempty {_jtArrayConnections = HM.singleton source arraySelectNode}

withWriteComputedFieldTableSet ::
  (MonadWriter JoinTree m) =>
  m
    ( ComputedFieldTableSetSource,
      S.Extractor,
      HM.HashMap S.Alias S.SQLExp,
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
       in mempty {_jtComputedFieldTableSets = HM.singleton source selectNode}
