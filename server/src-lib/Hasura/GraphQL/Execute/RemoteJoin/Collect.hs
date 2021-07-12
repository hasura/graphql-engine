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
import           Hasura.GraphQL.Parser.Column            (UnpreparedValue (..))
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
  :: (Backend b)
  => QueryDB b r (UnpreparedValue b)
  -> (QueryDB b (Const Void) (UnpreparedValue b), Maybe RemoteJoins)
getRemoteJoins = \case
  QDBMultipleRows s -> first QDBMultipleRows $ getRemoteJoinsSelect s
  QDBSingleRow    s -> first QDBSingleRow    $ getRemoteJoinsSelect s
  QDBAggregation  s -> first QDBAggregation  $ getRemoteJoinsAggregateSelect s
  QDBConnection   s -> first QDBConnection   $ getRemoteJoinsConnectionSelect s

-- | Traverse through 'AnnSimpleSel' and collect remote join fields (if any).
getRemoteJoinsSelect
  :: (Backend b)
  => AnnSimpleSelectG b r (UnpreparedValue b)
  -> (AnnSimpleSelectG b (Const Void) (UnpreparedValue b), Maybe RemoteJoins)
getRemoteJoinsSelect =
  second mapToNonEmpty . flip runState mempty . transformSelect mempty

-- | Traverse through @'AnnAggregateSelect' and collect remote join fields (if any).
getRemoteJoinsAggregateSelect
  :: (Backend b)
  => AnnAggregateSelectG b r (UnpreparedValue b)
  -> (AnnAggregateSelectG b (Const Void) (UnpreparedValue b), Maybe RemoteJoins)
getRemoteJoinsAggregateSelect =
  second mapToNonEmpty . flip runState mempty . transformAggregateSelect mempty

-- | Traverse through @'ConnectionSelect' and collect remote join fields (if any).
getRemoteJoinsConnectionSelect
  :: (Backend b)
  => ConnectionSelect b r (UnpreparedValue b)
  -> (ConnectionSelect b (Const Void) (UnpreparedValue b), Maybe RemoteJoins)
getRemoteJoinsConnectionSelect =
  second mapToNonEmpty . flip runState mempty . transformConnectionSelect mempty

-- | Traverse through 'MutationOutput' and collect remote join fields (if any)
getRemoteJoinsMutationOutput
  :: (Backend b)
  => MutationOutputG b r (UnpreparedValue b)
  -> (MutationOutputG b (Const Void) (UnpreparedValue b), Maybe RemoteJoins)
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
  :: (Backend b)
  => AnnFieldsG b r (UnpreparedValue b)
  -> (AnnFieldsG b (Const Void) (UnpreparedValue b), Maybe RemoteJoins)
getRemoteJoinsAnnFields =
  second mapToNonEmpty . flip runState mempty . transformAnnFields mempty

getRemoteJoinsMutationDB
  :: (Backend b)
  => MutationDB b r (UnpreparedValue b)
  -> (MutationDB b (Const Void) (UnpreparedValue b), Maybe RemoteJoins)
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
  => AnnActionExecution b r (UnpreparedValue b)
  -> (AnnActionExecution b (Const Void) (UnpreparedValue b), Maybe RemoteJoins)
getRemoteJoinsSyncAction actionExecution =
  let (fields', remoteJoins) = getRemoteJoinsAnnFields $ _aaeFields actionExecution
  in (actionExecution { _aaeFields = fields' }, remoteJoins)

getRemoteJoinsActionQuery
  :: (Backend b)
  => ActionQuery b r (UnpreparedValue b)
  -> (ActionQuery b (Const Void) (UnpreparedValue b), Maybe RemoteJoins)
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
  => ActionMutation b r (UnpreparedValue b)
  -> (ActionMutation b (Const Void) (UnpreparedValue b), Maybe RemoteJoins)
getRemoteJoinsActionMutation = \case
  AMSync sync   -> first AMSync $ getRemoteJoinsSyncAction sync
  AMAsync async -> (AMAsync async, Nothing)


transformSelect
  :: (Backend b)
  => FieldPath
  -> AnnSimpleSelectG b r (UnpreparedValue b)
  -> State RemoteJoinMap (AnnSimpleSelectG b (Const Void) (UnpreparedValue b))
transformSelect path sel = do
  let fields = _asnFields sel
  -- Transform selects in array, object and computed fields
  transformedFields <- transformAnnFields path fields
  pure sel{_asnFields = transformedFields}

transformAggregateSelect
  :: (Backend b)
  => FieldPath
  -> AnnAggregateSelectG b r (UnpreparedValue b)
  -> State RemoteJoinMap (AnnAggregateSelectG b (Const Void) (UnpreparedValue b))
transformAggregateSelect path sel = do
  let aggFields = _asnFields sel
  transformedFields <- forM aggFields $ \(fieldName, aggField) ->
    (fieldName,) <$> case aggField of
      TAFAgg agg           -> pure $ TAFAgg agg
      TAFNodes x annFields -> TAFNodes x <$> transformAnnFields (appendPath fieldName path) annFields
      TAFExp t             -> pure $ TAFExp t
  pure sel{_asnFields = transformedFields}

transformConnectionSelect
  :: (Backend b)
  => FieldPath
  -> ConnectionSelect b r (UnpreparedValue b)
  -> State RemoteJoinMap (ConnectionSelect b (Const Void) (UnpreparedValue b))
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
  :: (Backend b)
  => FieldPath
  -> AnnObjectSelectG b r (UnpreparedValue b)
  -> State RemoteJoinMap (AnnObjectSelectG b (Const Void) (UnpreparedValue b))
transformObjectSelect path sel = do
  let fields = _aosFields sel
  transformedFields <- transformAnnFields path fields
  pure sel{_aosFields = transformedFields}

transformAnnFields
  :: forall b r
   . (Backend b)
  => FieldPath
  -> AnnFieldsG b r (UnpreparedValue b)
  -> State RemoteJoinMap (AnnFieldsG b (Const Void) (UnpreparedValue b))
transformAnnFields path fields = do

  -- TODO: Check for correctness. I think this entire function seems to be
  -- assuming that the column names will appear as is in the response from the
  -- server, which is incorrect as they can be aliased. Similarly, the phantom
  -- columns are being added without checking for overlap with aliases

  let columnsInSelSet = HS.fromList $ map (pgiColumn . _acfInfo . snd) $ getFields _AFColumn fields
      scalarComputedFieldsInSelSet = HS.fromList $ map ((^. _2) . snd) $ getFields _AFComputedField fields
      remoteSelects = getFields (_AFRemote) fields
      remoteJoins = remoteSelects <&> \(fieldName, remoteSelect) ->
        let RemoteSelect argsMap selSet hasuraFields remoteFields rsi = remoteSelect
            hasuraFieldNames = HS.map dbJoinFieldToName hasuraFields

            -- See Note [Phantom fields in Remote Joins]
            fieldPresentInSelection = \case
              JoinColumn columnInfo -> HS.member (pgiColumn columnInfo) columnsInSelSet
              JoinComputedField computedFieldInfo -> HS.member (_scfName computedFieldInfo) scalarComputedFieldsInSelSet

            phantomFields = HS.filter (not . fieldPresentInSelection) hasuraFields
            phantomFieldNames = toList $ HS.map dbJoinFieldToName phantomFields
        in (phantomFields, RemoteJoin fieldName argsMap selSet hasuraFieldNames remoteFields rsi phantomFieldNames)

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
      AFComputedField x n computedField ->
        AFComputedField x n <$> case computedField of
          CFSScalar cfss cbe  -> pure $ CFSScalar cfss cbe
          CFSTable jas annSel -> CFSTable jas <$> transformSelect fieldPath annSel
      AFRemote rs -> pure $ AFRemote rs
      AFExpression t -> pure $ AFExpression t
      -- TODO: implement this
      AFDBRemote _ -> error "FIXME"

  case NE.nonEmpty remoteJoins of
    Nothing -> pure transformedFields
    Just nonEmptyRemoteJoins -> do
      let annotatedPhantomFields = (toList $ HS.unions $ map fst remoteJoins) <&> \phantomField ->
            (dbJoinFieldToName phantomField,) $ case phantomField of
              JoinColumn columnInfo               -> AFColumn $ AnnColumnField columnInfo False Nothing Nothing
              JoinComputedField computedFieldInfo -> mkScalarComputedFieldSelect computedFieldInfo

      modify (Map.insert path $ fmap snd nonEmptyRemoteJoins)
      pure $ transformedFields <> annotatedPhantomFields

  where
    getFields f = mapMaybe (sequence . second (^? f))

    transformAnnRelation f (AnnRelationSelectG name maps select) = do
      transformedSelect <- f select
      pure $ AnnRelationSelectG name maps transformedSelect

    mkScalarComputedFieldSelect :: ScalarComputedField b -> (AnnFieldG b (Const Void) (UnpreparedValue b))
    mkScalarComputedFieldSelect ScalarComputedField{..} =
      let functionArgs = flip FunctionArgsExp mempty
            $ functionArgsWithTableRowAndSession _scfTableArgument _scfSessionArgument
          fieldSelect = flip CFSScalar Nothing
            $ ComputedFieldScalarSelect _scfFunction functionArgs _scfType Nothing
      in AFComputedField _scfXField _scfName fieldSelect
      where
        functionArgsWithTableRowAndSession
          :: FunctionTableArgument
          -> Maybe FunctionSessionArgument
          -> [ArgumentExp b (UnpreparedValue b)]
        functionArgsWithTableRowAndSession  _              Nothing = [AETableRow Nothing] -- No session argument
        functionArgsWithTableRowAndSession  (FTAFirst)     _       = [AETableRow Nothing, AESession UVSession]
        functionArgsWithTableRowAndSession  (FTANamed _ 0) _       = [AETableRow Nothing, AESession UVSession] -- Index is 0 implies table argument is first
        functionArgsWithTableRowAndSession  _              _       = [AESession UVSession, AETableRow Nothing]


mapToNonEmpty :: RemoteJoinMap -> Maybe RemoteJoins
mapToNonEmpty = NE.nonEmpty . Map.toList
