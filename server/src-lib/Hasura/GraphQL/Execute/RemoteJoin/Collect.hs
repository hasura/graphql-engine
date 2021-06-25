module Hasura.GraphQL.Execute.RemoteJoin.Collect
  ( RemoteJoins
  , RemoteJoinMap
  , FieldPath(..)
  , appendPath
  , getRemoteJoins
  , getRemoteJoinsSelect
  , getRemoteJoinsMutationDB
  , getRemoteJoinsActionQuery
  , getRemoteJoinsActionMutation
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                     as Map
import qualified Data.HashSet                            as HS
import qualified Data.List.NonEmpty                      as NE

import           Control.Lens

import           Hasura.GraphQL.Execute.RemoteJoin.Types
import           Hasura.RQL.IR
import           Hasura.RQL.IR.Returning
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

-- | Collects remote joins from the AST and also adds the necessary join fields
getRemoteJoins
  :: Backend b
  => QueryDB b r u
  -> (QueryDB b (Const Void) u, Maybe RemoteJoins)
getRemoteJoins = \case
  QDBMultipleRows s -> first QDBMultipleRows $ getRemoteJoinsSelect s
  QDBSingleRow    s -> first QDBSingleRow    $ getRemoteJoinsSelect s
  QDBAggregation  s -> first QDBAggregation  $ getRemoteJoinsAggregateSelect s
  QDBConnection   s -> first QDBConnection   $ getRemoteJoinsConnectionSelect s

-- | Traverse through 'AnnSimpleSel' and collect remote join fields (if any).
getRemoteJoinsSelect
  :: Backend b
  => AnnSimpleSelG b r u
  -> (AnnSimpleSelG b (Const Void) u, Maybe RemoteJoins)
getRemoteJoinsSelect =
  second mapToNonEmpty . flip runState mempty . transformSelect mempty

-- | Traverse through @'AnnAggregateSelect' and collect remote join fields (if any).
getRemoteJoinsAggregateSelect
  :: Backend b
  => AnnAggregateSelectG b r u
  -> (AnnAggregateSelectG b (Const Void) u, Maybe RemoteJoins)
getRemoteJoinsAggregateSelect =
  second mapToNonEmpty . flip runState mempty . transformAggregateSelect mempty

-- | Traverse through @'ConnectionSelect' and collect remote join fields (if any).
getRemoteJoinsConnectionSelect
  :: Backend b
  => ConnectionSelect b r u
  -> (ConnectionSelect b (Const Void) u, Maybe RemoteJoins)
getRemoteJoinsConnectionSelect =
  second mapToNonEmpty . flip runState mempty . transformConnectionSelect mempty

-- | Traverse through 'MutationOutput' and collect remote join fields (if any)
getRemoteJoinsMutationOutput
  :: Backend b
  => MutationOutputG b r u
  -> (MutationOutputG b (Const Void) u, Maybe RemoteJoins)
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

getRemoteJoinsAnnFields
  :: Backend b
  => AnnFieldsG b r u
  -> (AnnFieldsG b (Const Void) u, Maybe RemoteJoins)
getRemoteJoinsAnnFields =
  second mapToNonEmpty . flip runState mempty . transformAnnFields mempty

getRemoteJoinsMutationDB
  :: Backend b
  => MutationDB b r u
  -> (MutationDB b (Const Void) u, Maybe RemoteJoins)
getRemoteJoinsMutationDB = \case
  MDBInsert insert ->
    first MDBInsert $ getRemoteJoinsInsert insert
  MDBUpdate update ->
    first MDBUpdate $ getRemoteJoinsUpdate update
  MDBDelete delete ->
    first MDBDelete $ getRemoteJoinsDelete delete
  MDBFunction aggSelect select ->
    first (MDBFunction aggSelect) $ getRemoteJoinsSelect select
  where
    getRemoteJoinsInsert insert =
      let (output', remoteJoins) = getRemoteJoinsMutationOutput $ _aiOutput insert
      in (insert{ _aiOutput = output'}, remoteJoins)

    getRemoteJoinsUpdate update =
      let (output', remoteJoins) = getRemoteJoinsMutationOutput $ uqp1Output update
      in (update{ uqp1Output = output'}, remoteJoins)

    getRemoteJoinsDelete delete =
      let (output', remoteJoins) = getRemoteJoinsMutationOutput $ dqp1Output delete
      in (delete{ dqp1Output = output'}, remoteJoins)

getRemoteJoinsSyncAction
  :: (Backend b)
  => AnnActionExecution b r v
  -> (AnnActionExecution b (Const Void) v, Maybe RemoteJoins)
getRemoteJoinsSyncAction actionExecution =
  let (fields', remoteJoins) = getRemoteJoinsAnnFields $ _aaeFields actionExecution
  in (actionExecution { _aaeFields = fields' }, remoteJoins)

getRemoteJoinsActionQuery
  :: (Backend b)
  => ActionQuery b r v
  -> (ActionQuery b (Const Void) v, Maybe RemoteJoins)
getRemoteJoinsActionQuery = \case
  AQQuery sync ->
    first AQQuery $ getRemoteJoinsSyncAction sync
  AQAsync async ->
    first AQAsync $ getRemoteJoinsAsyncQuery async
  where
    getRemoteJoinsAsyncQuery async =
      let (fields', remoteJoins) =
            second mapToNonEmpty . flip runState mempty . transformAsyncFields mempty $
            _aaaqFields async
      in (async { _aaaqFields = fields' }, remoteJoins)

    transformAsyncFields path fields =
      forM fields $ \(fieldName, field) -> do
        let fieldPath = appendPath fieldName path
        (fieldName,) <$> case field of
          AsyncTypename t -> pure $ AsyncTypename t
          AsyncOutput outputFields ->
            AsyncOutput <$> transformAnnFields fieldPath outputFields
          AsyncId -> pure AsyncId
          AsyncCreatedAt -> pure AsyncCreatedAt
          AsyncErrors -> pure AsyncErrors

getRemoteJoinsActionMutation
  :: (Backend b)
  => ActionMutation b r v
  -> (ActionMutation b (Const Void) v, Maybe RemoteJoins)
getRemoteJoinsActionMutation = \case
  AMSync sync ->
    first AMSync $ getRemoteJoinsSyncAction sync
  AMAsync async -> (AMAsync async, Nothing)


transformSelect
  :: Backend b
  => FieldPath
  -> AnnSimpleSelG b r u
  -> State RemoteJoinMap (AnnSimpleSelG b (Const Void) u)
transformSelect path sel = do
  let fields = _asnFields sel
  -- Transform selects in array, object and computed fields
  transformedFields <- transformAnnFields path fields
  pure sel{_asnFields = transformedFields}

transformAggregateSelect
  :: Backend b
  => FieldPath
  -> AnnAggregateSelectG b r u
  -> State RemoteJoinMap (AnnAggregateSelectG b (Const Void) u)
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
  -> ConnectionSelect b r u
  -> State RemoteJoinMap (ConnectionSelect b (Const Void) u)
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
  -> AnnObjectSelectG b r u
  -> State RemoteJoinMap (AnnObjectSelectG b (Const Void) u)
transformObjectSelect path sel = do
  let fields = _aosFields sel
  transformedFields <- transformAnnFields path fields
  pure sel{_aosFields = transformedFields}

transformAnnFields
  :: forall b r u
   . Backend b
  => FieldPath
  -> AnnFieldsG b r u
  -> State RemoteJoinMap (AnnFieldsG b (Const Void) u)
transformAnnFields path fields = do

  -- TODO: Check for correctness. I think this entire function seems to be
  -- assuming that the column names will appear as is in the response from the
  -- server, which is incorrect as they can be aliased. Similarly, the phantom
  -- columns are being added without checking for overlap with aliases

  let pgColumnFields = HS.fromList $ map (pgiColumn . _acfInfo . snd) $
                       getFields _AFColumn fields
      remoteSelects = getFields (_AFRemote) fields
      remoteJoins = remoteSelects <&> \(fieldName, remoteSelect) ->
        let RemoteSelect argsMap selSet hasuraColumns remoteFields rsi = remoteSelect
            hasuraColumnFields = HS.map (fromCol @b . pgiColumn) hasuraColumns
            phantomColumns = HS.filter ((`notElem` pgColumnFields) . pgiColumn) hasuraColumns
        in (phantomColumns, RemoteJoin fieldName argsMap selSet hasuraColumnFields remoteFields rsi $
           map (fromCol @b . pgiColumn) $ toList phantomColumns)

  transformedFields <- forM fields $ \(fieldName, field') -> do
    let fieldPath = appendPath fieldName path
    (fieldName,) <$> case field' of
      AFNodeId x qt pkeys -> pure $ AFNodeId x qt pkeys
      AFColumn c -> pure $ AFColumn c
      AFObjectRelation annRel ->
        AFObjectRelation <$> transformAnnRelation (transformObjectSelect fieldPath) annRel
      AFArrayRelation (ASSimple annRel) ->
        AFArrayRelation . ASSimple <$> transformAnnRelation (transformSelect fieldPath) annRel
      AFArrayRelation (ASAggregate aggRel) ->
        AFArrayRelation . ASAggregate <$> transformAnnRelation (transformAggregateSelect fieldPath) aggRel
      AFArrayRelation (ASConnection annRel) ->
        AFArrayRelation . ASConnection <$> transformAnnRelation (transformConnectionSelect fieldPath) annRel
      AFComputedField x computedField ->
        AFComputedField x <$> case computedField of
          CFSScalar cfss cbe  -> pure $ CFSScalar cfss cbe
          CFSTable jas annSel -> CFSTable jas <$> transformSelect fieldPath annSel
      AFRemote rs -> pure $ AFRemote rs
      AFExpression t -> pure $ AFExpression t
      -- TODO: implement this
      AFDBRemote _ -> error "FIXME"

  case NE.nonEmpty remoteJoins of
    Nothing -> pure transformedFields
    Just nonEmptyRemoteJoins -> do
      let phantomColumns = map (\ci -> (fromCol @b $ pgiColumn ci, AFColumn $ AnnColumnField ci False Nothing Nothing)) $ toList $ HS.unions $ map fst $ remoteJoins
      modify (Map.insert path $ fmap snd nonEmptyRemoteJoins)
      pure $ transformedFields <> phantomColumns

  where
    getFields f = mapMaybe (sequence . second (^? f))

    transformAnnRelation f (AnnRelationSelectG name maps select) = do
      transformedSelect <- f select
      pure $ AnnRelationSelectG name maps transformedSelect


mapToNonEmpty :: RemoteJoinMap -> Maybe RemoteJoins
mapToNonEmpty = NE.nonEmpty . Map.toList
