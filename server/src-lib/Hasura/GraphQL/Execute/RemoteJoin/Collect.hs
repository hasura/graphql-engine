module Hasura.GraphQL.Execute.RemoteJoin.Collect
  ( getRemoteJoins,
    getRemoteJoinsSelect,
    getRemoteJoinsMutationDB,
    getRemoteJoinsActionQuery,
    getRemoteJoinsActionMutation,
  )
where

import Control.Lens (Traversal', preview, _2)
import Control.Monad.Writer
import Data.HashMap.Strict qualified as Map
import Data.HashMap.Strict.NonEmpty (NEHashMap)
import Data.HashMap.Strict.NonEmpty qualified as NEMap
import Data.Text qualified as T
import Hasura.GraphQL.Execute.RemoteJoin.Types
import Hasura.GraphQL.Parser.Column (UnpreparedValue (..))
import Hasura.Prelude
import Hasura.RQL.IR
import Hasura.RQL.Types
import Hasura.SQL.AnyBackend qualified as AB
import Language.GraphQL.Draft.Syntax qualified as G

{- Note [Remote Joins Architecture]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Unparsed Incoming GraphQL   +------------------------------+
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

-- | A writer monad used to collect together all remote joins
-- appearing in some data structure.
--
-- In the functions below, the 'withField' function is used to track the
-- context of the path from the root of the current selection set.
--
-- It is important that we work bottom-up, and do not 'collect' duplicate
-- field names at any level, because the 'Semigroup' instance for 'RemoteJoins'
-- does not allow for these duplicates.
newtype Collector a = Collector {runCollector :: (a, Maybe RemoteJoins)}
  deriving
    (Functor, Applicative, Monad, MonadWriter (Maybe RemoteJoins))
    via Writer (Maybe RemoteJoins)

-- | Collect some remote joins appearing at the given field names in the current
-- context.
collect :: NEHashMap FieldName RemoteJoin -> Collector ()
collect = tell . Just . JoinTree . fmap Leaf

-- | Keep track of the given field name in the current path from the root of the
-- selection set.
withField :: FieldName -> Collector a -> Collector a
withField name = censor (fmap wrap)
  where
    wrap rjs = JoinTree $ NEMap.singleton name (Tree rjs)

-- | Collects remote joins from the AST and also adds the necessary join fields
getRemoteJoins ::
  Backend b =>
  QueryDB b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b) ->
  (QueryDB b Void (UnpreparedValue b), Maybe RemoteJoins)
getRemoteJoins = \case
  QDBMultipleRows s -> first QDBMultipleRows $ getRemoteJoinsSelect s
  QDBSingleRow s -> first QDBSingleRow $ getRemoteJoinsSelect s
  QDBAggregation s -> first QDBAggregation $ getRemoteJoinsAggregateSelect s
  QDBConnection s -> first QDBConnection $ getRemoteJoinsConnectionSelect s

-- | Traverse through 'AnnSimpleSel' and collect remote join fields (if any).
getRemoteJoinsSelect ::
  Backend b =>
  AnnSimpleSelectG b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b) ->
  (AnnSimpleSelectG b Void (UnpreparedValue b), Maybe RemoteJoins)
getRemoteJoinsSelect =
  runCollector . transformSelect

-- | Traverse through @'AnnAggregateSelect' and collect remote join fields (if any).
getRemoteJoinsAggregateSelect ::
  Backend b =>
  AnnAggregateSelectG b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b) ->
  (AnnAggregateSelectG b Void (UnpreparedValue b), Maybe RemoteJoins)
getRemoteJoinsAggregateSelect =
  runCollector . transformAggregateSelect

-- | Traverse through @'ConnectionSelect' and collect remote join fields (if any).
getRemoteJoinsConnectionSelect ::
  Backend b =>
  ConnectionSelect b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b) ->
  (ConnectionSelect b Void (UnpreparedValue b), Maybe RemoteJoins)
getRemoteJoinsConnectionSelect =
  runCollector . transformConnectionSelect

-- | Traverse through 'MutationOutput' and collect remote join fields (if any)
getRemoteJoinsMutationOutput ::
  Backend b =>
  MutationOutputG b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b) ->
  (MutationOutputG b Void (UnpreparedValue b), Maybe RemoteJoins)
getRemoteJoinsMutationOutput =
  runCollector . transformMutationOutput
  where
    transformMutationOutput = \case
      MOutMultirowFields mutationFields ->
        MOutMultirowFields <$> transfromMutationFields mutationFields
      MOutSinglerowObject annFields ->
        MOutSinglerowObject <$> transformAnnFields annFields
      where
        transfromMutationFields fields =
          for fields $ \(fieldName, field') -> withField fieldName do
            (fieldName,) <$> case field' of
              MCount -> pure MCount
              MExp t -> pure $ MExp t
              MRet annFields -> MRet <$> transformAnnFields annFields

-- local helpers

getRemoteJoinsActionFields ::
  ActionFieldsG (RemoteRelationshipField UnpreparedValue) ->
  (ActionFieldsG Void, Maybe RemoteJoins)
getRemoteJoinsActionFields =
  runCollector . transformActionFields

getRemoteJoinsMutationDB ::
  Backend b =>
  MutationDB b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b) ->
  (MutationDB b Void (UnpreparedValue b), Maybe RemoteJoins)
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
       in (insert {_aiOutput = output'}, remoteJoins)

    getRemoteJoinsUpdate update =
      let (output', remoteJoins) = getRemoteJoinsMutationOutput $ _auOutput update
       in (update {_auOutput = output'}, remoteJoins)

    getRemoteJoinsDelete delete =
      let (output', remoteJoins) = getRemoteJoinsMutationOutput $ dqp1Output delete
       in (delete {dqp1Output = output'}, remoteJoins)

getRemoteJoinsSyncAction ::
  AnnActionExecution (RemoteRelationshipField UnpreparedValue) ->
  (AnnActionExecution Void, Maybe RemoteJoins)
getRemoteJoinsSyncAction actionExecution =
  let (fields', remoteJoins) = getRemoteJoinsActionFields $ _aaeFields actionExecution
   in (actionExecution {_aaeFields = fields'}, remoteJoins)

getRemoteJoinsActionQuery ::
  ActionQuery (RemoteRelationshipField UnpreparedValue) ->
  (ActionQuery Void, Maybe RemoteJoins)
getRemoteJoinsActionQuery = \case
  AQQuery sync ->
    first AQQuery $ getRemoteJoinsSyncAction sync
  AQAsync async ->
    first AQAsync $ getRemoteJoinsAsyncQuery async
  where
    getRemoteJoinsAsyncQuery async =
      let (fields', remoteJoins) =
            runCollector . transformAsyncFields $
              _aaaqFields async
       in (async {_aaaqFields = fields'}, remoteJoins)

    transformAsyncFields fields =
      for fields $ \(fieldName, field) -> withField fieldName do
        (fieldName,) <$> case field of
          AsyncTypename t -> pure $ AsyncTypename t
          AsyncOutput outputFields ->
            AsyncOutput <$> transformActionFields outputFields
          AsyncId -> pure AsyncId
          AsyncCreatedAt -> pure AsyncCreatedAt
          AsyncErrors -> pure AsyncErrors

getRemoteJoinsActionMutation ::
  ActionMutation (RemoteRelationshipField UnpreparedValue) ->
  (ActionMutation Void, Maybe RemoteJoins)
getRemoteJoinsActionMutation = \case
  AMAsync async -> (AMAsync async, Nothing)
  AMSync sync -> first AMSync $ getRemoteJoinsSyncAction sync

transformSelect ::
  Backend b =>
  AnnSimpleSelectG b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b) ->
  Collector (AnnSimpleSelectG b Void (UnpreparedValue b))
transformSelect select@AnnSelectG {_asnFields = fields} = do
  -- Transform selects in array, object and computed fields
  transformedFields <- transformAnnFields fields
  pure select {_asnFields = transformedFields}

transformAggregateSelect ::
  Backend b =>
  AnnAggregateSelectG b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b) ->
  Collector (AnnAggregateSelectG b Void (UnpreparedValue b))
transformAggregateSelect select@AnnSelectG {_asnFields = aggFields} = do
  transformedFields <- for aggFields \(fieldName, aggField) ->
    withField fieldName $ case aggField of
      TAFAgg agg -> pure (fieldName, TAFAgg agg)
      TAFExp t -> pure (fieldName, TAFExp t)
      TAFNodes nodesAgg annFields -> do
        transformed <- transformAnnFields annFields
        pure (fieldName, TAFNodes nodesAgg transformed)
  pure select {_asnFields = transformedFields}

transformConnectionSelect ::
  forall b.
  Backend b =>
  ConnectionSelect b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b) ->
  Collector (ConnectionSelect b Void (UnpreparedValue b))
transformConnectionSelect connSelect@ConnectionSelect {..} = do
  transformedFields <- for (_asnFields _csSelect) \(fieldName, connField) ->
    withField fieldName $ case connField of
      ConnectionTypename t -> pure (fieldName, ConnectionTypename t)
      ConnectionPageInfo p -> pure (fieldName, ConnectionPageInfo p)
      ConnectionEdges edges -> do
        transformed <- transformEdges edges
        pure (fieldName, ConnectionEdges transformed)

  let select = _csSelect {_asnFields = transformedFields}
  pure connSelect {_csSelect = select}
  where
    transformEdges ::
      [(FieldName, EdgeField b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))] ->
      Collector [(FieldName, EdgeField b Void (UnpreparedValue b))]
    transformEdges edgeFields = for edgeFields \(fieldName, edgeField) ->
      withField fieldName $ case edgeField of
        EdgeTypename t -> pure (fieldName, EdgeTypename t)
        EdgeCursor -> pure (fieldName, EdgeCursor)
        EdgeNode annFields -> do
          transformed <- transformAnnFields annFields
          pure (fieldName, EdgeNode transformed)

transformObjectSelect ::
  Backend b =>
  AnnObjectSelectG b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b) ->
  Collector (AnnObjectSelectG b Void (UnpreparedValue b))
transformObjectSelect select@AnnObjectSelectG {_aosFields = fields} = do
  transformedFields <- transformAnnFields fields
  pure select {_aosFields = transformedFields}

-- | Converts a remote relationship field into a 'RemoteJoin' that
-- the execution engine understands
createRemoteJoin ::
  -- We need information about 'how' the lhs join fields appear in the lhs
  -- response to construct a 'RemoteJoin' node
  Map.HashMap FieldName JoinColumnAlias ->
  -- The remote relationship field as captured in the IR
  RemoteRelationshipField UnpreparedValue ->
  RemoteJoin
createRemoteJoin joinColumnAliases remoteRelationship =
  case remoteRelationship of
    RemoteSchemaField RemoteSchemaSelect {..} ->
      let inputArgsToMap = Map.fromList . map (_rfaArgument &&& _rfaValue)
          remoteJoin =
            RemoteJoinRemoteSchema $
              RemoteSchemaJoin
                (inputArgsToMap _rselArgs)
                _rselResultCustomizer
                (convertSelectionSet _rselSelection)
                joinColumnAliases
                _rselFieldCall
                _rselRemoteSchema
       in remoteJoin
    RemoteSourceField anySourceSelect ->
      AB.dispatchAnyBackend @Backend anySourceSelect \RemoteSourceSelect {..} ->
        let (transformedSourceRelationship, sourceRelationshipJoins) =
              getRemoteJoinsSourceRelation _rssSelection

            -- the invariant here is that the the keys in joinColumnAliases and
            -- _rssJoinMapping are the same. We could've opted for a more type
            -- safe representation Map k (a, b) instead of (Map k a, Map k b)
            -- but that would make the type of lhs join columns creep into
            -- RemoteRelationshipField which would make the type a little
            -- unweildy
            joinColumns =
              _rssJoinMapping & Map.mapMaybeWithKey
                \joinFieldName (rhsColumn, rhsColumnType) ->
                  (,rhsColumn,rhsColumnType)
                    <$> Map.lookup joinFieldName joinColumnAliases
            anySourceJoin =
              AB.mkAnyBackend $
                RemoteSourceJoin
                  _rssName
                  _rssConfig
                  transformedSourceRelationship
                  joinColumns
         in RemoteJoinSource anySourceJoin sourceRelationshipJoins

transformAnnFields ::
  forall src.
  Backend src =>
  AnnFieldsG src (RemoteRelationshipField UnpreparedValue) (UnpreparedValue src) ->
  Collector (AnnFieldsG src Void (UnpreparedValue src))
transformAnnFields fields = do
  -- Produces a list of transformed fields that may or may not have an
  -- associated remote join.
  annotatedFields <- for fields \(fieldName, field') -> withField fieldName do
    (fieldName,) <$> case field' of
      -- AnnFields which do not need to be transformed.
      AFNodeId x qt pkeys -> pure (AFNodeId x qt pkeys, Nothing)
      AFColumn c -> pure (AFColumn c, Nothing)
      AFExpression t -> pure (AFExpression t, Nothing)
      -- AnnFields with no associated remote joins and whose transformations are
      -- relatively straightforward.
      AFObjectRelation annRel -> do
        transformed <- transformAnnRelation transformObjectSelect annRel
        pure (AFObjectRelation transformed, Nothing)
      AFArrayRelation (ASSimple annRel) -> do
        transformed <- transformAnnRelation transformSelect annRel
        pure (AFArrayRelation . ASSimple $ transformed, Nothing)
      AFArrayRelation (ASAggregate aggRel) -> do
        transformed <- transformAnnRelation transformAggregateSelect aggRel
        pure (AFArrayRelation . ASAggregate $ transformed, Nothing)
      AFArrayRelation (ASConnection annRel) -> do
        transformed <- transformAnnRelation transformConnectionSelect annRel
        pure (AFArrayRelation . ASConnection $ transformed, Nothing)
      AFComputedField computedField computedFieldName computedFieldSelect -> do
        transformed <- case computedFieldSelect of
          CFSScalar cfss cbe -> pure $ CFSScalar cfss cbe
          CFSTable jsonAggSel annSel -> do
            transformed <- transformSelect annSel
            pure $ CFSTable jsonAggSel transformed
        pure (AFComputedField computedField computedFieldName transformed, Nothing)
      -- Remote AnnFields, whose elements require annotation so that they can be
      -- used to construct a remote join.
      AFRemote RemoteRelationshipSelect {..} ->
        pure
          ( -- We generate this so that the response has a key with the relationship,
            -- without which preserving the order of fields in the final response
            -- would require a lot of bookkeeping.
            remoteAnnPlaceholder,
            Just $ createRemoteJoin joinColumnAliases _rrsRelationship
          )

  let transformedFields = (fmap . fmap) fst annotatedFields
      remoteJoins =
        annotatedFields & mapMaybe \(fieldName, (_, mRemoteJoin)) ->
          (fieldName,) <$> mRemoteJoin

  case NEMap.fromList remoteJoins of
    Nothing -> pure transformedFields
    Just neRemoteJoins -> do
      collect neRemoteJoins
      pure $ transformedFields <> phantomFields
  where
    -- Placeholder text to annotate a remote relationship field.
    remoteAnnPlaceholder :: AnnFieldG src Void (UnpreparedValue src)
    remoteAnnPlaceholder = AFExpression "remote relationship placeholder"

    -- This is a map of column name to its alias of all columns in the
    -- selection set.
    columnFields :: HashMap (Column src) FieldName
    columnFields =
      Map.fromList $
        [ (_acfColumn annColumn, alias)
          | (alias, annColumn) <- getFields _AFColumn fields
        ]

    -- This is a map of computed field name to its alias of all computed fields
    -- in the selection set.
    computedFields :: Map.HashMap ComputedFieldName FieldName
    computedFields =
      Map.fromList $
        [ (fieldName, alias)
          | -- Note that we do not currently care about input arguments to a computed
            -- field because only computed fields which do not accept input arguments
            -- are currently allowed.
            (alias, fieldName) <- getFields (_AFComputedField . _2) fields
        ]

    -- Annotate a 'DBJoinField' with its field name and an alias so that it may
    -- be used to construct a remote join.
    annotateDBJoinField ::
      FieldName -> DBJoinField src -> (DBJoinField src, JoinColumnAlias)
    annotateDBJoinField fieldName = \case
      jc@(JoinColumn column _) ->
        let alias = getJoinColumnAlias fieldName column columnFields allAliases
         in (jc, alias)
      jcf@(JoinComputedField ScalarComputedField {..}) ->
        let alias = getJoinColumnAlias fieldName _scfName computedFields allAliases
         in (jcf, alias)
      where
        allAliases = map fst fields

    -- goes through all the remote relationships in the selection set and emits
    -- 1. a map of join field names to their aliases in the lhs response
    -- 2. a list of extra fields that need to be included in the lhs query
    --    that are required for the join
    (joinColumnAliases, phantomFields) =
      let lhsJoinFields =
            Map.unions $ map (_rrsLHSJoinFields . snd) $ getFields _AFRemote fields
          annotatedJoinColumns = Map.mapWithKey annotateDBJoinField $ lhsJoinFields
          phantomFields_ =
            toList annotatedJoinColumns & mapMaybe \(joinField, alias) ->
              case alias of
                JCSelected _ -> Nothing
                JCPhantom a -> case joinField of
                  JoinColumn column columnType ->
                    let annotatedColumn =
                          AFColumn $ AnnColumnField column columnType False Nothing Nothing
                     in Just (a, annotatedColumn)
                  JoinComputedField computedFieldInfo ->
                    Just (a, mkScalarComputedFieldSelect computedFieldInfo)
       in (fmap snd annotatedJoinColumns, phantomFields_)

    mkScalarComputedFieldSelect ::
      ScalarComputedField b ->
      AnnFieldG b Void (UnpreparedValue b)
    mkScalarComputedFieldSelect ScalarComputedField {..} =
      let functionArgs =
            flip FunctionArgsExp mempty $
              functionArgsWithTableRowAndSession UVSession _scfTableArgument _scfSessionArgument
          fieldSelect =
            flip CFSScalar Nothing $
              ComputedFieldScalarSelect _scfFunction functionArgs _scfType Nothing
       in AFComputedField _scfXField _scfName fieldSelect

-- | Get the fields targeted by some 'Traversal' for an arbitrary list of
-- tuples, discarding any elements whose fields cannot be focused upon.
getFields :: Traversal' super sub -> [(any, super)] -> [(any, sub)]
getFields focus = mapMaybe (traverse $ preview focus)

-- | Annotate an element a remote source join from '_rssJoinMapping' so that
-- a remote join can be constructed.
transformAnnRelation ::
  (a -> Collector b) ->
  AnnRelationSelectG src a ->
  Collector (AnnRelationSelectG src b)
transformAnnRelation transform relation@(AnnRelationSelectG _ _ select) = do
  transformedSelect <- transform select
  pure $ relation {aarAnnSelect = transformedSelect}

transformActionFields ::
  ActionFieldsG (RemoteRelationshipField UnpreparedValue) ->
  Collector (ActionFieldsG Void)
transformActionFields fields = do
  -- Produces a list of transformed fields that may or may not have an
  -- associated remote join.
  annotatedFields <- for fields \(fieldName, field') -> withField fieldName do
    (fieldName,) <$> case field' of
      -- ActionFields which do not need to be transformed.
      ACFScalar c -> pure (ACFScalar c, Nothing)
      ACFExpression t -> pure (ACFExpression t, Nothing)
      -- Remote ActionFields, whose elements require annotation so that they can be
      -- used to construct a remote join.
      ACFRemote ActionRemoteRelationshipSelect {..} ->
        pure
          ( -- We generate this so that the response has a key with the relationship,
            -- without which preserving the order of fields in the final response
            -- would require a lot of bookkeeping.
            remoteActionPlaceholder,
            Just $ createRemoteJoin joinColumnAliases _arrsRelationship
          )
      ACFNestedObject fn fs ->
        (,Nothing) . ACFNestedObject fn <$> transformActionFields fs

  let transformedFields = (fmap . fmap) fst annotatedFields
      remoteJoins =
        annotatedFields & mapMaybe \(fieldName, (_, mRemoteJoin)) ->
          (fieldName,) <$> mRemoteJoin

  case NEMap.fromList remoteJoins of
    Nothing -> pure transformedFields
    Just neRemoteJoins -> do
      collect neRemoteJoins
      pure $ transformedFields <> phantomFields
  where
    -- Placeholder text to annotate a remote relationship field.
    remoteActionPlaceholder :: ActionFieldG Void
    remoteActionPlaceholder = ACFExpression "remote relationship placeholder"

    -- This is a map of column name to its alias of all columns in the
    -- selection set.
    scalarFields :: HashMap G.Name FieldName
    scalarFields =
      Map.fromList $
        [ (name, alias)
          | (alias, name) <- getFields _ACFScalar fields
        ]

    -- Annotate a join field with its field name and an alias so that it may
    -- be used to construct a remote join.
    annotateJoinField ::
      FieldName -> G.Name -> (G.Name, JoinColumnAlias)
    annotateJoinField fieldName field =
      let alias = getJoinColumnAlias fieldName field scalarFields allAliases
       in (field, alias)
      where
        allAliases = map fst fields

    -- goes through all the remote relationships in the selection set and emits
    -- 1. a map of join field names to their aliases in the lhs response
    -- 2. a list of extra fields that need to be included in the lhs query
    --    that are required for the join
    (joinColumnAliases, phantomFields :: ([(FieldName, ActionFieldG Void)])) =
      let lhsJoinFields =
            Map.unions $ map (_arrsLHSJoinFields . snd) $ getFields _ACFRemote fields
          annotatedJoinColumns = Map.mapWithKey annotateJoinField $ lhsJoinFields
          phantomFields_ :: ([(FieldName, ActionFieldG Void)]) =
            toList annotatedJoinColumns & mapMaybe \(joinField, alias) ->
              case alias of
                JCSelected _ -> Nothing
                JCPhantom a ->
                  let annotatedColumn =
                        ACFScalar joinField
                   in Just (a, annotatedColumn)
       in (fmap snd annotatedJoinColumns, phantomFields_)

getJoinColumnAlias ::
  (Eq field, Hashable field) =>
  FieldName ->
  field ->
  HashMap field FieldName ->
  [FieldName] ->
  JoinColumnAlias
getJoinColumnAlias fieldName field selectedFields allAliases =
  case Map.lookup field selectedFields of
    Nothing -> JCPhantom uniqueAlias
    Just fieldAlias -> JCSelected fieldAlias
  where
    -- This generates an alias for a phantom field that does not conflict with
    -- any of the existing aliases in the selection set
    --
    -- If we generate a unique name for each field name which is longer than
    -- the longest alias in the selection set, the generated name would be
    -- unique
    uniqueAlias :: FieldName
    uniqueAlias =
      let suffix =
            "_join_column"
              <>
              -- 12 is the length of "_join_column"
              T.replicate ((longestAliasLength - (T.length (coerce fieldName) + 12)) + 1) "_"
       in fieldName <> FieldName suffix
      where
        longestAliasLength = maximum $ map (T.length . coerce) allAliases

getRemoteJoinsSourceRelation ::
  Backend b =>
  SourceRelationshipSelection b (RemoteRelationshipField UnpreparedValue) UnpreparedValue ->
  (SourceRelationshipSelection b Void UnpreparedValue, Maybe RemoteJoins)
getRemoteJoinsSourceRelation = runCollector . transformSourceRelation
  where
    transformSourceRelation = \case
      SourceRelationshipObject objectSelect ->
        SourceRelationshipObject <$> transformObjectSelect objectSelect
      SourceRelationshipArray simpleSelect ->
        SourceRelationshipArray <$> transformSelect simpleSelect
      SourceRelationshipArrayAggregate aggregateSelect ->
        SourceRelationshipArrayAggregate <$> transformAggregateSelect aggregateSelect
