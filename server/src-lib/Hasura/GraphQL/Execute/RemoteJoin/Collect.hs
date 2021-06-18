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
concatJoinTrees ((f1, n1) NE.:| t1) t2 =
  let merged = Map.unionWith concatJoinNodes (Map.fromList t1) (Map.fromList $ toList t2)
   in case Map.lookup f1 merged of
        Nothing -> (f1, n1) NE.:| Map.toList merged
        Just n2 -> (f1, concatJoinNodes n1 n2) NE.:| Map.toList (Map.delete f1 merged)

concatJoinNodes :: JoinNode a -> JoinNode a -> JoinNode a
concatJoinNodes n1 n2 =
  case (n1, n2) of
    (_, Leaf b) -> Leaf b
    (Leaf _, Tree b) -> Tree b
    (Tree a, Tree b) -> Tree $ concatJoinTrees a b

remoteJoinsToJoinTree :: RemoteJoinMap -> Maybe RemoteJoins
remoteJoinsToJoinTree joinMap =
  case NE.nonEmpty (map (toJoinTree . first unFieldPath) $ Map.toList joinMap) of
    Just (t1 NE.:| l) -> Just $ foldr concatJoinTrees t1 l
    Nothing -> Nothing
  where
    toJoinTree (fieldPath, nonEmptyJoin) =
      case fieldPath of
        [] -> fmap (second Leaf) nonEmptyJoin
        x:xs -> (x, Tree $ toJoinTree (xs, nonEmptyJoin)) NE.:| []

-- | Collects remote joins from the AST and also adds the necessary join fields
getRemoteJoins
  :: Backend b
  => QueryDB b u
  -> (QueryDB b u, Maybe RemoteJoins)
getRemoteJoins = \case
  QDBMultipleRows s -> first QDBMultipleRows $ getRemoteJoinsSelect s
  QDBSingleRow    s -> first QDBSingleRow $ getRemoteJoinsSelect s
  QDBAggregation  s -> first QDBAggregation $ getRemoteJoinsAggregateSelect s
  QDBConnection   s -> first QDBConnection $ getRemoteJoinsConnectionSelect s

-- | Traverse through 'AnnSimpleSel' and collect remote join fields (if any).
getRemoteJoinsSelect
  :: Backend b
  => AnnSimpleSelG b u
  -> (AnnSimpleSelG b u, Maybe RemoteJoins)
getRemoteJoinsSelect =
  second remoteJoinsToJoinTree . flip runState mempty . transformSelect mempty

-- | Traverse through @'AnnAggregateSelect' and collect remote join fields (if any).
getRemoteJoinsAggregateSelect
  :: Backend b
  => AnnAggregateSelectG b u
  -> (AnnAggregateSelectG b u, Maybe RemoteJoins)
getRemoteJoinsAggregateSelect =
  second remoteJoinsToJoinTree . flip runState mempty . transformAggregateSelect mempty

-- | Traverse through @'ConnectionSelect' and collect remote join fields (if any).
getRemoteJoinsConnectionSelect
  :: Backend b
  => ConnectionSelect b u
  -> (ConnectionSelect b u, Maybe RemoteJoins)
getRemoteJoinsConnectionSelect =
  second remoteJoinsToJoinTree . flip runState mempty . transformConnectionSelect mempty

-- | Traverse through 'MutationOutput' and collect remote join fields (if any)
getRemoteJoinsMutationOutput
  :: Backend b
  => MutationOutputG b u
  -> (MutationOutputG b u, Maybe RemoteJoins)
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

transformSelect
  :: Backend b
  => FieldPath
  -> AnnSimpleSelG b u
  -> State RemoteJoinMap (AnnSimpleSelG b u)
transformSelect path sel = do
  let fields = _asnFields sel
  -- Transform selects in array, object and computed fields
  transformedFields <- transformAnnFields path fields
  pure sel{_asnFields = transformedFields}

transformAggregateSelect
  :: Backend b
  => FieldPath
  -> AnnAggregateSelectG b u
  -> State RemoteJoinMap (AnnAggregateSelectG b u)
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
  -> State RemoteJoinMap (ConnectionSelect b u)
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
  -> State RemoteJoinMap (AnnObjectSelectG b u)
transformObjectSelect path sel = do
  let fields = _aosFields sel
  transformedFields <- transformAnnFields path fields
  pure sel{_aosFields = transformedFields}

getRemoteJoinsAnnFields
  :: Backend b
  => AnnFieldsG b u
  -> (AnnFieldsG b u, Maybe RemoteJoins)
getRemoteJoinsAnnFields =
  second remoteJoinsToJoinTree . flip runState mempty . transformAnnFields mempty

transformAnnFields
  :: forall b u
   . Backend b
  => FieldPath
  -> AnnFieldsG b u
  -> State RemoteJoinMap (AnnFieldsG b u)
transformAnnFields path fields = do

  -- TODO: Check for correctness. I think this entire function seems to be
  -- assuming that the column names will appear as is in the response from the
  -- server, which is incorrect as they can be aliased. Similarly, the phantom
  -- columns are being added without checking for overlap with aliases

  let pgColumnFields_ = Map.fromList $
        [ (pgiColumn . _acfInfo $ annColumn, alias)
        | (alias, annColumn) <- getFields _AFColumn fields
        ]

      getJoinColumnAlias columnName =
        maybe (makeUniqueAlias columnName) JCSelected $
          Map.lookup columnName pgColumnFields_

      longestAliasLength = maximum $ map (T.length . coerce . fst) fields
      makeUniqueAlias columnName =
        let columnNameTxt = toTxt columnName
        in JCPhantom $ FieldName $ columnNameTxt <> "_join_column"
           <> T.replicate (longestAliasLength - (T.length columnNameTxt) - 12) "_"

      remoteSelects = getFields (_AFRemote) fields
      remoteJoins = flip map remoteSelects $ \(fieldName, remoteSelect) ->
        let RemoteSelect inputArgs selSet hasuraColumns remoteFields rsi = remoteSelect
            annotatedJoinColumns =
              Map.fromList $ flip map (toList hasuraColumns) $ \columnInfo ->
                let columnName = pgiColumn columnInfo
                in (fromCol @b $ columnName, (columnInfo, getJoinColumnAlias columnName))
            phantomColumns =
              flip Map.mapMaybe annotatedJoinColumns $ \(columnInfo, alias) ->
                case alias of
                  JCSelected _ -> Nothing
                  JCPhantom a  -> Just (columnInfo, a)
            joinColumnAliases = fmap snd annotatedJoinColumns
            inputArgsToMap = Map.fromList . map (_rfaArgument &&& _rfaValue)
        in (phantomColumns, (fieldName, RemoteSchemaJoin (inputArgsToMap inputArgs) selSet joinColumnAliases remoteFields rsi))

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
      AFRemote rs -> pure $ AFRemote rs
      AFExpression t     -> pure $ AFExpression t

  case NE.nonEmpty remoteJoins of
    Nothing -> pure transformedFields
    Just nonEmptyRemoteJoins -> do
      let phantomColumns = map (\(ci, alias) -> (alias, AFColumn $ AnnColumnField ci False Nothing Nothing)) $ Map.elems $ Map.unions $ map fst $ remoteJoins
      modify (Map.insert path $ fmap snd nonEmptyRemoteJoins)
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

getRemoteJoinsMutationDB
  :: Backend b
  => MutationDB b u
  -> (MutationDB b u, Maybe RemoteJoins)
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
  => AnnActionExecution b v
  -> (AnnActionExecution b v, Maybe RemoteJoins)
getRemoteJoinsSyncAction actionExecution =
  let (fields', remoteJoins) = getRemoteJoinsAnnFields $ _aaeFields actionExecution
  in (actionExecution { _aaeFields = fields' }, remoteJoins)

getRemoteJoinsActionQuery
  :: (Backend b)
  => ActionQuery b v
  -> (ActionQuery b v, Maybe RemoteJoins)
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
  => ActionMutation b v
  -> (ActionMutation b v, Maybe RemoteJoins)
getRemoteJoinsActionMutation = \case
  AMSync sync ->
    first AMSync $ getRemoteJoinsSyncAction sync
  AMAsync async -> (AMAsync async, Nothing)
