module Hasura.GraphQL.Execute.RemoteJoin
  ( RemoteJoins
  , RemoteJoinMap
  , FieldPath(..)
  , appendPath
  , getRemoteJoins
  , getRemoteJoinsSelect
  , getRemoteJoinsAggregateSelect
  , getRemoteJoinsConnectionSelect
  , getRemoteJoinsMutationOutput
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict      as Map
import qualified Data.HashSet             as HS
import qualified Data.List.NonEmpty       as NE

import           Control.Lens

import           Hasura.GraphQL.Context
import           Hasura.RQL.IR.RemoteJoin
import           Hasura.RQL.IR.Returning
import           Hasura.RQL.IR.Select
import           Hasura.RQL.Types

{- Note: [Remote Joins Architecture]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 Unparsed Incoming GraphQL  +------------------------------+
--------------------------> | Parsing of the GraphQL query |-----+
                            +------------------------------+     |
                                                                 | DB Query and remote joins (if any)
                                                                 |
                                                                 V
+----------------------------------+  SQL query response  +----------------------------+
|  Traverse the DB response to     | <------------------- |  Execution of the DB query |
|  get the values of the arguments |                      +----------------------------+
|   of the remote field            |
+----------------------------------+
             |
             | Remote field arguments
             V
+--------------------------+  Remote schema response   +----------------------------------------+
| Query the remote schema  | ------------------------> | Replace the remote join fields in      |
| with the remote field    |                           | the SQL query response (JSON) with     |
| arguments to the remote  |                           | the response obtained from the remote  |
| field configured in the  |                           | schema at appropriate places.          |
| remote join.             |                           +----------------------------------------+
+--------------------------+
-}



-- remote joins info

-- | Path to the remote join field in query response JSON from Postgres.
newtype FieldPath = FieldPath {unFieldPath :: [FieldName]}
  deriving (Show, Eq, Semigroup, Monoid, Hashable)

appendPath :: FieldName -> FieldPath -> FieldPath
appendPath fieldName = FieldPath . (<> [fieldName]) . unFieldPath


type RemoteJoins b = NE.NonEmpty (FieldPath, NE.NonEmpty (RemoteJoin b))
type RemoteJoinMap b = Map.HashMap FieldPath (NE.NonEmpty (RemoteJoin b))


-- extract remote joins out of a root field

getRemoteJoins
  :: Backend b
  => QueryDB b u
  -> Maybe (RemoteJoins b)
getRemoteJoins = \case
  QDBMultipleRows s -> snd $ getRemoteJoinsSelect s
  QDBSingleRow    s -> snd $ getRemoteJoinsSelect s
  QDBAggregation  s -> snd $ getRemoteJoinsAggregateSelect s
  QDBConnection   s -> snd $ getRemoteJoinsConnectionSelect s

-- | Traverse through 'AnnSimpleSel' and collect remote join fields (if any).
getRemoteJoinsSelect
  :: Backend b
  => AnnSimpleSelG b u
  -> (AnnSimpleSelG b u, Maybe (RemoteJoins b))
getRemoteJoinsSelect =
  second mapToNonEmpty . flip runState mempty . transformSelect mempty

-- | Traverse through @'AnnAggregateSelect' and collect remote join fields (if any).
getRemoteJoinsAggregateSelect
  :: Backend b
  => AnnAggregateSelectG b u
  -> (AnnAggregateSelectG b u, Maybe (RemoteJoins b))
getRemoteJoinsAggregateSelect =
  second mapToNonEmpty . flip runState mempty . transformAggregateSelect mempty

-- | Traverse through @'ConnectionSelect' and collect remote join fields (if any).
getRemoteJoinsConnectionSelect
  :: Backend b
  => ConnectionSelect b u
  -> (ConnectionSelect b u, Maybe (RemoteJoins b))
getRemoteJoinsConnectionSelect =
  second mapToNonEmpty . flip runState mempty . transformConnectionSelect mempty

-- | Traverse through 'MutationOutput' and collect remote join fields (if any)
getRemoteJoinsMutationOutput
  :: Backend b
  => MutationOutputG b u
  -> (MutationOutputG b u, Maybe (RemoteJoins b))
getRemoteJoinsMutationOutput =
  second mapToNonEmpty . flip runState mempty . transformMutationOutput mempty
  where
    transformMutationOutput path = \case
      MOutMultirowFields mutationFields ->
        MOutMultirowFields <$> transfromMutationFields mutationFields
      MOutSinglerowObject annFields ->
        MOutSinglerowObject <$> transformAnnFields path annFields
      where
        transfromMutationFields fields =
          forM fields $ \(fieldName, field') -> do
          let fieldPath = appendPath fieldName path
          (fieldName,) <$> case field' of
            MCount         -> pure MCount
            MExp t         -> pure $ MExp t
            MRet annFields -> MRet <$> transformAnnFields fieldPath annFields


-- local helpers

transformSelect
  :: Backend b
  => FieldPath
  -> AnnSimpleSelG b u
  -> State (RemoteJoinMap b) (AnnSimpleSelG b u)
transformSelect path sel = do
  let fields = _asnFields sel
  -- Transform selects in array, object and computed fields
  transformedFields <- transformAnnFields path fields
  pure sel{_asnFields = transformedFields}

transformAggregateSelect
  :: Backend b
  => FieldPath
  -> AnnAggregateSelectG b u
  -> State (RemoteJoinMap b) (AnnAggregateSelectG b u)
transformAggregateSelect path sel = do
  let aggFields = _asnFields sel
  transformedFields <- forM aggFields $ \(fieldName, aggField) ->
    (fieldName,) <$> case aggField of
      TAFAgg agg           -> pure $ TAFAgg agg
      TAFNodes x annFields -> TAFNodes x <$> transformAnnFields (appendPath fieldName path) annFields
      TAFExp t             -> pure $ TAFExp t
  pure sel{_asnFields = transformedFields}

transformConnectionSelect
  :: Backend b
  => FieldPath
  -> ConnectionSelect b u
  -> State (RemoteJoinMap b) (ConnectionSelect b u)
transformConnectionSelect path ConnectionSelect{..} = do
  let connectionFields = _asnFields _csSelect
  transformedFields <- forM connectionFields $ \(fieldName, field) ->
    (fieldName,) <$> case field of
      ConnectionTypename t  -> pure $ ConnectionTypename t
      ConnectionPageInfo p  -> pure $ ConnectionPageInfo p
      ConnectionEdges edges -> ConnectionEdges <$> transformEdges (appendPath fieldName path) edges
  let select = _csSelect{_asnFields = transformedFields}
  pure $ ConnectionSelect _csXRelay _csPrimaryKeyColumns _csSplit _csSlice select
  where
    transformEdges edgePath edgeFields =
      forM edgeFields $ \(fieldName, edgeField) ->
      (fieldName,) <$> case edgeField of
        EdgeTypename t -> pure $ EdgeTypename t
        EdgeCursor -> pure EdgeCursor
        EdgeNode annFields ->
          EdgeNode <$> transformAnnFields (appendPath fieldName edgePath) annFields

transformObjectSelect
  :: Backend b
  => FieldPath
  -> AnnObjectSelectG b u
  -> State (RemoteJoinMap b) (AnnObjectSelectG b u)
transformObjectSelect path sel = do
  let fields = _aosFields sel
  transformedFields <- transformAnnFields path fields
  pure sel{_aosFields = transformedFields}

transformAnnFields
  :: forall b u
   . Backend b
  => FieldPath
  -> AnnFieldsG b u
  -> State (RemoteJoinMap b) (AnnFieldsG b u)
transformAnnFields path fields = do
  let pgColumnFields = map fst $ getFields _AFColumn fields
      remoteSelects = getFields (_AFRemote . _2) fields
      remoteJoins = flip map remoteSelects $ \(fieldName, remoteSelect) ->
        let RemoteSelect argsMap selSet hasuraColumns remoteFields rsi = remoteSelect
            hasuraColumnL = toList hasuraColumns
            hasuraColumnFields = HS.fromList $ map (fromCol @b . pgiColumn) hasuraColumnL
            phantomColumns = filter ((`notElem` pgColumnFields) . fromCol @b . pgiColumn) hasuraColumnL
        in RemoteJoin fieldName argsMap selSet hasuraColumnFields remoteFields rsi phantomColumns

  transformedFields <- forM fields $ \(fieldName, field') -> do
    let fieldPath = appendPath fieldName path
    (fieldName,) <$> case field' of
      AFNodeId x qt pkeys -> pure $ AFNodeId x qt pkeys
      AFColumn c -> pure $ AFColumn c
      AFObjectRelation annRel ->
        AFObjectRelation <$> transformAnnRelation annRel (transformObjectSelect fieldPath)
      AFArrayRelation (ASSimple annRel) ->
        AFArrayRelation . ASSimple <$> transformAnnRelation annRel (transformSelect fieldPath)
      AFArrayRelation (ASAggregate aggRel) ->
        AFArrayRelation . ASAggregate <$> transformAnnAggregateRelation fieldPath aggRel
      AFArrayRelation (ASConnection annRel) ->
        AFArrayRelation . ASConnection <$> transformArrayConnection fieldPath annRel
      AFComputedField x computedField ->
        AFComputedField x <$> case computedField of
          CFSScalar _ _       -> pure computedField
          CFSTable jas annSel -> CFSTable jas <$> transformSelect fieldPath annSel
      AFRemote x rs -> pure $ AFRemote x rs
      AFExpression t     -> pure $ AFExpression t

  case NE.nonEmpty remoteJoins of
    Nothing -> pure transformedFields
    Just nonEmptyRemoteJoins -> do
      let phantomColumns = map (\ci -> (fromCol @b $ pgiColumn ci, AFColumn $ AnnColumnField ci False Nothing Nothing)) $
                           concatMap _rjPhantomFields remoteJoins
      modify (Map.insert path nonEmptyRemoteJoins)
      pure $ transformedFields <> phantomColumns
    where
      getFields f = mapMaybe (sequence . second (^? f))

      transformAnnRelation annRel f = do
        let annSel = aarAnnSelect annRel
        transformedSel <- f annSel
        pure annRel{aarAnnSelect = transformedSel}

      transformAnnAggregateRelation fieldPath annRel = do
        let annSel = aarAnnSelect annRel
        transformedSel <- transformAggregateSelect fieldPath annSel
        pure annRel{aarAnnSelect = transformedSel}

      transformArrayConnection fieldPath annRel = do
        let connectionSelect = aarAnnSelect annRel
        transformedConnectionSelect <- transformConnectionSelect fieldPath connectionSelect
        pure annRel{aarAnnSelect = transformedConnectionSelect}

mapToNonEmpty :: RemoteJoinMap backend -> Maybe (RemoteJoins backend)
mapToNonEmpty = NE.nonEmpty . Map.toList
