module Hasura.GraphQL.Execute.RemoteJoin.Collect
  ( getRemoteJoins
  , getRemoteJoinsSelect
  , getRemoteJoinsMutationDB
  , getRemoteJoinsActionQuery
  , getRemoteJoinsActionMutation
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                     as Map
import qualified Data.List.NonEmpty                      as NE
import qualified Data.Text                               as T

import           Control.Lens
import           Data.Text.Extended

import qualified Hasura.SQL.AnyBackend                   as AB

import           Hasura.GraphQL.Execute.RemoteJoin.Types
import           Hasura.GraphQL.Parser.Column            (UnpreparedValue)
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

-- | Path to the remote join field in query response JSON from Postgres.
newtype FieldPath = FieldPath {unFieldPath :: [FieldName]}
  deriving (Show, Eq, Semigroup, Monoid, Hashable)

appendPath :: FieldName -> FieldPath -> FieldPath
appendPath fieldName = FieldPath . (<> [fieldName]) . unFieldPath

type RemoteJoinMap = Map.HashMap FieldPath (NE.NonEmpty (FieldName, RemoteJoin))

-- TODO: we should get rid of these following functions. They currently exist
-- because Collect module does not output the jointree structure, it instead
-- outputs RemoteJoinMap. The jointree structure is introduced to efficiently
-- traverse a response object to add replacement tokens. Instead of modifying
-- the Collect module, the `remoteJoinsToJoinTree` function is used to covert
-- between these structures. These functions are a bit dodgy to my liking
concatJoinTrees :: JoinTree a -> JoinTree a -> JoinTree a
concatJoinTrees (JoinTree ((f1, n1) NE.:| t1)) (JoinTree t2) =
  let merged = Map.unionWith concatJoinNodes (Map.fromList t1) (Map.fromList $ toList t2)
   in JoinTree $ case Map.lookup f1 merged of
        Nothing -> (f1, n1) NE.:| Map.toList merged
        Just n2 -> (f1, concatJoinNodes n1 n2) NE.:| Map.toList (Map.delete f1 merged)

concatJoinNodes :: JoinNode a -> JoinNode a -> JoinNode a
concatJoinNodes n1 n2 =
  case (n1, n2) of
    (_, Leaf b)      -> Leaf b
    (Leaf _, Tree b) -> Tree b
    (Tree a, Tree b) -> Tree $ concatJoinTrees a b

remoteJoinsToJoinTree :: RemoteJoinMap -> Maybe RemoteJoins
remoteJoinsToJoinTree joinMap =
  case NE.nonEmpty (map (toJoinTree . first unFieldPath) $ Map.toList joinMap) of
    Just (t1 NE.:| l) -> Just $ foldr concatJoinTrees t1 l
    Nothing           -> Nothing
  where
    toJoinTree (fieldPath, nonEmptyJoin) =
      JoinTree $ case fieldPath of
        []   -> fmap (second Leaf) nonEmptyJoin
        x:xs -> (x, Tree $ toJoinTree (xs, nonEmptyJoin)) NE.:| []

-- | Collects remote joins from the AST and also adds the necessary join fields
getRemoteJoins
  :: Backend b
  => QueryDB b (RemoteSelect UnpreparedValue) (UnpreparedValue b)
  -> (QueryDB b (Const Void) (UnpreparedValue b), Maybe RemoteJoins)
getRemoteJoins = \case
  QDBMultipleRows s -> first QDBMultipleRows $ getRemoteJoinsSelect s
  QDBSingleRow    s -> first QDBSingleRow    $ getRemoteJoinsSelect s
  QDBAggregation  s -> first QDBAggregation  $ getRemoteJoinsAggregateSelect s
  QDBConnection   s -> first QDBConnection   $ getRemoteJoinsConnectionSelect s

-- | Traverse through 'AnnSimpleSel' and collect remote join fields (if any).
getRemoteJoinsSelect
  :: Backend b
  => AnnSimpleSelG b (RemoteSelect UnpreparedValue) (UnpreparedValue b)
  -> (AnnSimpleSelG b (Const Void) (UnpreparedValue b), Maybe RemoteJoins)
getRemoteJoinsSelect =
  second remoteJoinsToJoinTree . flip runState mempty . transformSelect mempty

-- | Traverse through @'AnnAggregateSelect' and collect remote join fields (if any).
getRemoteJoinsAggregateSelect
  :: Backend b
  => AnnAggregateSelectG b (RemoteSelect UnpreparedValue) (UnpreparedValue b)
  -> (AnnAggregateSelectG b (Const Void) (UnpreparedValue b), Maybe RemoteJoins)
getRemoteJoinsAggregateSelect =
  second remoteJoinsToJoinTree . flip runState mempty . transformAggregateSelect mempty

-- | Traverse through @'ConnectionSelect' and collect remote join fields (if any).
getRemoteJoinsConnectionSelect
  :: Backend b
  => ConnectionSelect b (RemoteSelect UnpreparedValue) (UnpreparedValue b)
  -> (ConnectionSelect b (Const Void) (UnpreparedValue b), Maybe RemoteJoins)
getRemoteJoinsConnectionSelect =
  second remoteJoinsToJoinTree . flip runState mempty . transformConnectionSelect mempty

-- | Traverse through 'MutationOutput' and collect remote join fields (if any)
getRemoteJoinsMutationOutput
  :: Backend b
  => MutationOutputG b (RemoteSelect UnpreparedValue) (UnpreparedValue b)
  -> (MutationOutputG b (Const Void) (UnpreparedValue b), Maybe RemoteJoins)
getRemoteJoinsMutationOutput =
  second remoteJoinsToJoinTree . flip runState mempty . transformMutationOutput mempty
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
  => AnnFieldsG b (RemoteSelect UnpreparedValue) (UnpreparedValue b)
  -> (AnnFieldsG b (Const Void) (UnpreparedValue b), Maybe RemoteJoins)
getRemoteJoinsAnnFields =
  second remoteJoinsToJoinTree  . flip runState mempty . transformAnnFields mempty

getRemoteJoinsMutationDB
  :: Backend b
  => MutationDB b (RemoteSelect UnpreparedValue) (UnpreparedValue b)
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
  => AnnActionExecution b (RemoteSelect UnpreparedValue) (UnpreparedValue b)
  -> (AnnActionExecution b (Const Void) (UnpreparedValue b), Maybe RemoteJoins)
getRemoteJoinsSyncAction actionExecution =
  let (fields', remoteJoins) = getRemoteJoinsAnnFields $ _aaeFields actionExecution
  in (actionExecution { _aaeFields = fields' }, remoteJoins)

getRemoteJoinsActionQuery
  :: (Backend b)
  => ActionQuery b (RemoteSelect UnpreparedValue) (UnpreparedValue b)
  -> (ActionQuery b (Const Void) (UnpreparedValue b), Maybe RemoteJoins)
getRemoteJoinsActionQuery = \case
  AQQuery sync ->
    first AQQuery $ getRemoteJoinsSyncAction sync
  AQAsync async ->
    first AQAsync $ getRemoteJoinsAsyncQuery async
  where
    getRemoteJoinsAsyncQuery async =
      let (fields', remoteJoins) =
            second remoteJoinsToJoinTree . flip runState mempty . transformAsyncFields mempty $
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
  => ActionMutation b (RemoteSelect UnpreparedValue) (UnpreparedValue b)
  -> (ActionMutation b (Const Void) (UnpreparedValue b), Maybe RemoteJoins)
getRemoteJoinsActionMutation = \case
  AMSync sync ->
    first AMSync $ getRemoteJoinsSyncAction sync
  AMAsync async -> (AMAsync async, Nothing)


transformSelect
  :: Backend b
  => FieldPath
  -> AnnSimpleSelG b (RemoteSelect UnpreparedValue) (UnpreparedValue b)
  -> State RemoteJoinMap (AnnSimpleSelG b (Const Void) (UnpreparedValue b))
transformSelect path sel = do
  let fields = _asnFields sel
  -- Transform selects in array, object and computed fields
  transformedFields <- transformAnnFields path fields
  pure sel{_asnFields = transformedFields}

transformAggregateSelect
  :: Backend b
  => FieldPath
  -> AnnAggregateSelectG b (RemoteSelect UnpreparedValue) (UnpreparedValue b)
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
  :: Backend b
  => FieldPath
  -> ConnectionSelect b (RemoteSelect UnpreparedValue) (UnpreparedValue b)
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
  :: Backend b
  => FieldPath
  -> AnnObjectSelectG b (RemoteSelect UnpreparedValue) (UnpreparedValue b)
  -> State RemoteJoinMap (AnnObjectSelectG b (Const Void) (UnpreparedValue b))
transformObjectSelect path sel = do
  let fields = _aosFields sel
  transformedFields <- transformAnnFields path fields
  pure sel{_aosFields = transformedFields}

transformAnnFields
  :: forall b . Backend b
  => FieldPath
  -> AnnFieldsG b (RemoteSelect UnpreparedValue) (UnpreparedValue b)
  -> State RemoteJoinMap (AnnFieldsG b (Const Void) (UnpreparedValue b))
transformAnnFields path fields = do

  annotatedFields <- forM fields $ \(fieldName, field') -> do
    let fieldPath = appendPath fieldName path
    (fieldName,) <$> case field' of
      AFNodeId x qt pkeys -> pure $ (AFNodeId x qt pkeys, Nothing)
      AFColumn c -> pure $ (AFColumn c, Nothing)
      AFObjectRelation annRel ->
        (,Nothing) . AFObjectRelation <$> transformAnnRelation (transformObjectSelect fieldPath) annRel
      AFArrayRelation (ASSimple annRel) ->
        (,Nothing) . AFArrayRelation . ASSimple <$> transformAnnRelation (transformSelect fieldPath) annRel
      AFArrayRelation (ASAggregate aggRel) ->
        (,Nothing) . AFArrayRelation . ASAggregate <$> transformAnnRelation (transformAggregateSelect fieldPath) aggRel
      AFArrayRelation (ASConnection annRel) ->
        (,Nothing) . AFArrayRelation . ASConnection <$> transformAnnRelation (transformConnectionSelect fieldPath) annRel
      AFComputedField x computedField ->
        (,Nothing) . AFComputedField x <$> case computedField of
          CFSScalar cfss cbe  -> pure $ CFSScalar cfss cbe
          CFSTable jas annSel -> CFSTable jas <$> transformSelect fieldPath annSel
      -- we generate this so that the response has a key with the relationship,
      -- without which preserving the order of fields in the final response
      -- would require a lot of bookkeeping
      AFRemote remoteSelect -> do
        annotatedJoin <- case remoteSelect of

          RemoteSelectRemoteSchema RemoteSchemaSelect{..} -> do

            let annotatedJoinColumns =
                  Map.fromList $ flip map (toList _rselHasuraColumns) $ \columnInfo ->
                    let columnName = pgiColumn columnInfo
                    in (fromCol @b $ columnName, (columnInfo, getJoinColumnAlias columnName))
                phantomColumns =
                  flip Map.mapMaybe annotatedJoinColumns $ \(columnInfo, alias) ->
                    case alias of
                      JCSelected _ -> Nothing
                      JCPhantom a  -> Just (columnInfo, a)
                joinColumnAliases = fmap snd annotatedJoinColumns
                inputArgsToMap = Map.fromList . map (_rfaArgument &&& _rfaValue)
            pure $ ( phantomColumns
                   , RemoteJoinRemoteSchema $ RemoteSchemaJoin (inputArgsToMap _rselArgs)
                     _rselSelection joinColumnAliases _rselFieldCall _rselRemoteSchema
                   )
          RemoteSelectSource remoteSourceSelectAny ->
            AB.dispatchAnyBackend @Backend remoteSourceSelectAny
            \RemoteSourceSelect{..} -> do
              let (transformedSourceRelationship, sourceRelationshipJoins)
                    = getRemoteJoinsSourceRelation _rssSelection
                  annotatedJoinColumns =
                    flip fmap _rssJoinMapping $ \(columnInfo, rhsColumnType, rhsColumn) ->
                    let lhsColumnName = pgiColumn columnInfo
                     in ( columnInfo
                        , (getJoinColumnAlias lhsColumnName, rhsColumnType, rhsColumn)
                        )
                  phantomColumns =
                    flip Map.mapMaybe annotatedJoinColumns $ \(columnInfo, rest) ->
                    case rest ^. _1 of
                      JCSelected _ -> Nothing
                      JCPhantom a  -> Just (columnInfo, a)
                  sourceJoin = AB.mkAnyBackend $
                    RemoteSourceJoin _rssSourceName _rssSourceConfig
                    transformedSourceRelationship (fmap snd annotatedJoinColumns)
              pure $ (phantomColumns, RemoteJoinSource sourceJoin sourceRelationshipJoins)
        pure ( AFExpression "remote relationship placeholder"
             , Just annotatedJoin)
      AFExpression t ->
        pure $ (AFExpression t, Nothing)

  let transformedFields = map (fmap fst) annotatedFields
      remoteJoins = flip mapMaybe annotatedFields $
        \(fieldName, (_, rj)) -> (fieldName,) <$> rj
  case NE.nonEmpty remoteJoins of
    Nothing -> pure transformedFields
    Just nonEmptyRemoteJoins -> do
      let phantomColumns = flip map (Map.elems $ Map.unions $ map (fst . snd) remoteJoins) $
            \(ci, alias) -> (alias, AFColumn $ AnnColumnField ci False Nothing Nothing)
      modify $ Map.insert path $ fmap (second snd) nonEmptyRemoteJoins
      pure $ transformedFields <> phantomColumns

  where
    getFields f = mapMaybe (sequence . second (^? f))

    pgColumnFields = Map.fromList $
      [ (pgiColumn . _acfInfo $ annColumn, alias)
      | (alias, annColumn) <- getFields _AFColumn fields
      ]

    getJoinColumnAlias columnName =
      maybe (makeUniqueAlias columnName) JCSelected $
        Map.lookup columnName pgColumnFields

    longestAliasLength = maximum $ map (T.length . coerce . fst) fields
    makeUniqueAlias columnName =
      let columnNameTxt = toTxt columnName
      in JCPhantom $ FieldName $ columnNameTxt <> "_join_column"
         <> T.replicate (longestAliasLength - (T.length columnNameTxt) - 12) "_"

    transformAnnRelation f (AnnRelationSelectG name maps select) = do
      transformedSelect <- f select
      pure $ AnnRelationSelectG name maps transformedSelect

getRemoteJoinsSourceRelation
  :: Backend b
  => SourceRelationshipSelection b (RemoteSelect UnpreparedValue) UnpreparedValue
  -> (SourceRelationshipSelection b (Const Void) UnpreparedValue, Maybe RemoteJoins)
getRemoteJoinsSourceRelation =
  second remoteJoinsToJoinTree  . flip runState mempty . transformSourceRelation mempty
  where
    transformSourceRelation path = \case
      SourceRelationshipObject objectSelect ->
        SourceRelationshipObject <$> transformObjectSelect path objectSelect
      SourceRelationshipArray simpleSelect ->
        SourceRelationshipArray <$> transformSelect path simpleSelect
      SourceRelationshipArrayAggregate aggregateSelect ->
        SourceRelationshipArrayAggregate <$> transformAggregateSelect path aggregateSelect


