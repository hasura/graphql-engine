module Hasura.GraphQL.Execute.RemoteJoin.Collect
  ( getRemoteJoinsQueryDB,
    getRemoteJoinsMutationDB,
    getRemoteJoinsActionQuery,
    getRemoteJoinsActionMutation,
    getRemoteJoinsGraphQLField,
  )
where

import Control.Lens (Traversal', preview, traverseOf, _2)
import Control.Monad.Writer
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.HashMap.Strict.NonEmpty (NEHashMap)
import Data.HashMap.Strict.NonEmpty qualified as NEMap
import Data.Text qualified as T
import Hasura.Function.Cache
import Hasura.GraphQL.Execute.RemoteJoin.Types
import Hasura.GraphQL.Parser.Name qualified as GName
import Hasura.Name qualified as Name
import Hasura.Prelude
import Hasura.RQL.IR
import Hasura.RQL.IR.Select.Lenses
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.Relationships.Remote
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

-------------------------------------------------------------------------------
-- AST entry points

-- | Collects remote joins from the a 'QueryDB' if any, and transforms the
-- selection to add new join fields where those occured.
--
-- Returns the transformed selection set, in which remote fields have been
-- inserted, and for which the @r@ type is now 'Void'.
getRemoteJoinsQueryDB ::
  (Backend b) =>
  QueryDB b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b) ->
  (QueryDB b Void (UnpreparedValue b), Maybe RemoteJoins)
getRemoteJoinsQueryDB =
  runCollector . \case
    QDBMultipleRows s ->
      QDBMultipleRows <$> transformSelect s
    QDBSingleRow s ->
      QDBSingleRow <$> transformSelect s
    QDBAggregation s ->
      QDBAggregation <$> transformAggregateSelect s
    QDBConnection s ->
      QDBConnection <$> transformConnectionSelect s
    QDBStreamMultipleRows s ->
      QDBStreamMultipleRows <$> transformStreamSelect s

-- | Collects remote joins from the a 'MutationDB' if any, and transforms the
-- selection to add new join fields where those occured.
--
-- Returns the transformed selection set, in which remote fields have been
-- inserted, and for which the @r@ type is now 'Void'.
getRemoteJoinsMutationDB ::
  (Backend b) =>
  MutationDB b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b) ->
  (MutationDB b Void (UnpreparedValue b), Maybe RemoteJoins)
getRemoteJoinsMutationDB =
  runCollector . \case
    MDBInsert insert ->
      MDBInsert <$> traverseOf aiOutput transformMutationOutput insert
    MDBUpdate update ->
      MDBUpdate <$> traverseOf auOutput transformMutationOutput update
    MDBDelete delete ->
      MDBDelete <$> traverseOf adOutput transformMutationOutput delete
    MDBFunction aggSelect select ->
      MDBFunction aggSelect <$> transformSelect select

getRemoteJoinsActionQuery ::
  ActionQuery (RemoteRelationshipField UnpreparedValue) ->
  (ActionQuery Void, Maybe RemoteJoins)
getRemoteJoinsActionQuery =
  runCollector . \case
    AQQuery sync ->
      AQQuery <$> transformSyncAction sync
    AQAsync async ->
      AQAsync <$> traverseOf aaaqFields (traverseFields transformAsyncFields) async

getRemoteJoinsActionMutation ::
  ActionMutation (RemoteRelationshipField UnpreparedValue) ->
  (ActionMutation Void, Maybe RemoteJoins)
getRemoteJoinsActionMutation =
  runCollector . \case
    AMAsync async -> pure $ AMAsync async
    AMSync sync -> AMSync <$> transformSyncAction sync

getRemoteJoinsSourceRelation ::
  (Backend b) =>
  SourceRelationshipSelection b (RemoteRelationshipField UnpreparedValue) UnpreparedValue ->
  (SourceRelationshipSelection b Void UnpreparedValue, Maybe RemoteJoins)
getRemoteJoinsSourceRelation =
  runCollector . \case
    SourceRelationshipObject objectSelect ->
      SourceRelationshipObject <$> transformObjectSelect objectSelect
    SourceRelationshipArray simpleSelect ->
      SourceRelationshipArray <$> transformSelect simpleSelect
    SourceRelationshipArrayAggregate aggregateSelect ->
      SourceRelationshipArrayAggregate <$> transformAggregateSelect aggregateSelect

getRemoteJoinsGraphQLField ::
  GraphQLField (RemoteRelationshipField UnpreparedValue) var ->
  (GraphQLField Void var, Maybe RemoteJoins)
getRemoteJoinsGraphQLField =
  runCollector . transformGraphQLField

getRemoteJoinsGraphQLSelectionSet ::
  SelectionSet (RemoteRelationshipField UnpreparedValue) var ->
  (SelectionSet Void var, Maybe RemoteJoins)
getRemoteJoinsGraphQLSelectionSet =
  runCollector . transformGraphQLSelectionSet

-------------------------------------------------------------------------------

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
collect :: NEHashMap QualifiedFieldName RemoteJoin -> Collector ()
collect = tell . Just . JoinTree . fmap Leaf

-- | Keep track of the given field name in the current path from the root of the
-- selection set.
withField :: Maybe Text -> Text -> Collector a -> Collector a
withField typeName fieldName = censor (fmap wrap)
  where
    wrap rjs = JoinTree $ NEMap.singleton (QualifiedFieldName typeName fieldName) (Tree rjs)

-- | Traverse a list of fields, while applying 'withField' to keep track of the
-- path within the AST. This function assumes that no type name is required for
-- the 'QualifiedFieldName' and uses 'Nothing'.
traverseFields ::
  (a -> Collector b) ->
  Fields a ->
  Collector (Fields b)
traverseFields fun =
  traverse \field@(fieldName, _) ->
    withField Nothing (getFieldNameTxt fieldName) $ traverse fun field

-------------------------------------------------------------------------------
-- Internal AST traversals

transformAsyncFields ::
  AsyncActionQueryFieldG (RemoteRelationshipField UnpreparedValue) ->
  Collector (AsyncActionQueryFieldG Void)
transformAsyncFields = traverseOf _AsyncOutput transformActionFields

transformMutationOutput ::
  (Backend b) =>
  MutationOutputG b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b) ->
  Collector (MutationOutputG b Void (UnpreparedValue b))
transformMutationOutput = \case
  MOutMultirowFields mutationFields ->
    MOutMultirowFields <$> transformMutationFields mutationFields
  MOutSinglerowObject annFields ->
    MOutSinglerowObject <$> transformAnnFields annFields
  where
    transformMutationFields = traverseFields $ traverseOf _MRet transformAnnFields

transformSyncAction ::
  AnnActionExecution (RemoteRelationshipField UnpreparedValue) ->
  Collector (AnnActionExecution Void)
transformSyncAction = traverseOf aaeFields transformActionFields

transformSelect ::
  (Backend b) =>
  AnnSimpleSelectG b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b) ->
  Collector (AnnSimpleSelectG b Void (UnpreparedValue b))
transformSelect = traverseOf asnFields transformAnnFields

transformStreamSelect ::
  (Backend b) =>
  AnnSimpleStreamSelectG b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b) ->
  Collector (AnnSimpleStreamSelectG b Void (UnpreparedValue b))
transformStreamSelect select@AnnSelectStreamG {_assnFields = fields} = do
  -- Transform selects in array, object and computed fields
  transformedFields <- transformAnnFields fields
  pure select {_assnFields = transformedFields}

transformAggregateSelect ::
  (Backend b) =>
  AnnAggregateSelectG b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b) ->
  Collector (AnnAggregateSelectG b Void (UnpreparedValue b))
transformAggregateSelect =
  traverseOf asnFields
    $ traverseFields transformTableAggregateField

transformTableAggregateField ::
  (Backend b) =>
  TableAggregateFieldG b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b) ->
  Collector (TableAggregateFieldG b Void (UnpreparedValue b))
transformTableAggregateField = \case
  TAFAgg aggFields -> pure $ TAFAgg aggFields
  TAFNodes xNodesAgg annFields -> TAFNodes xNodesAgg <$> transformAnnFields annFields
  TAFGroupBy xGroupBy groupBy -> TAFGroupBy xGroupBy <$> transformGroupBy groupBy
  TAFExp txt -> pure $ TAFExp txt

transformGroupBy ::
  (Backend b) =>
  GroupByG b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b) ->
  Collector (GroupByG b Void (UnpreparedValue b))
transformGroupBy =
  traverseOf gbgFields
    $ traverseFields
    $ traverseOf _GBFNodes transformAnnFields

-- Relay doesn't support remote relationships: we can drill down directly to the
-- inner non-relay selection sets.
transformConnectionSelect ::
  forall b.
  (Backend b) =>
  ConnectionSelect b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b) ->
  Collector (ConnectionSelect b Void (UnpreparedValue b))
transformConnectionSelect =
  traverseOf (csSelect . asnFields)
    $ traverseFields
    $ traverseOf _ConnectionEdges
    $ traverseFields
    $ traverseOf _EdgeNode transformAnnFields

transformObjectSelect ::
  (Backend b) =>
  AnnObjectSelectG b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b) ->
  Collector (AnnObjectSelectG b Void (UnpreparedValue b))
transformObjectSelect = traverseOf aosFields transformAnnFields

transformNestedObjectSelect ::
  (Backend b) =>
  AnnNestedObjectSelectG b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b) ->
  Collector (AnnNestedObjectSelectG b Void (UnpreparedValue b))
transformNestedObjectSelect = traverseOf anosFields transformAnnFields

transformGraphQLField ::
  GraphQLField (RemoteRelationshipField UnpreparedValue) var ->
  Collector (GraphQLField Void var)
transformGraphQLField = traverseOf fSelectionSet transformGraphQLSelectionSet

transformGraphQLSelectionSet ::
  SelectionSet (RemoteRelationshipField UnpreparedValue) var ->
  Collector (SelectionSet Void var)
transformGraphQLSelectionSet = \case
  SelectionSetNone -> pure SelectionSetNone
  SelectionSetObject s -> SelectionSetObject <$> transformObjectSelectionSet Nothing s
  SelectionSetUnion s -> SelectionSetUnion <$> transformDeduplicatedTypeSelectionSet s
  SelectionSetInterface s -> SelectionSetInterface <$> transformDeduplicatedTypeSelectionSet s
  where
    transformDeduplicatedTypeSelectionSet =
      traverseOf dssMemberSelectionSets $ HashMap.traverseWithKey \typeName objectSelectionSet ->
        transformObjectSelectionSet (Just typeName) objectSelectionSet

-------------------------------------------------------------------------------
-- Actual transformations

-- | Transforms a source selection set.
--
-- This function takes an 'AnnFieldsG', which corresponds to a selection of
-- fields on a source, and extracts remote joins: for every field we encounter
-- that maps to a remote destination (either another source or a remote schema),
-- we replace it with a phantom field and 'collect' the corresponding
-- 'RemoteJoin'.
transformAnnFields ::
  forall src.
  (Backend src) =>
  AnnFieldsG src (RemoteRelationshipField UnpreparedValue) (UnpreparedValue src) ->
  Collector (AnnFieldsG src Void (UnpreparedValue src))
transformAnnFields fields = do
  let transformAnnField :: AnnFieldG src (RemoteRelationshipField UnpreparedValue) (UnpreparedValue src) -> Collector (AnnFieldG src Void (UnpreparedValue src), Maybe RemoteJoin)
      transformAnnField = \case
        -- AnnFields which do not need to be transformed.
        AFNodeId x sn qt pkeys ->
          pure (AFNodeId x sn qt pkeys, Nothing)
        AFColumn c ->
          pure (AFColumn c, Nothing)
        AFExpression t ->
          pure (AFExpression t, Nothing)
        -- AnnFields with no associated remote joins and whose transformations are
        -- relatively straightforward.
        AFObjectRelation annRel -> do
          transformed <- traverseOf aarAnnSelect transformObjectSelect annRel
          pure (AFObjectRelation transformed, Nothing)
        AFArrayRelation (ASSimple annRel) -> do
          transformed <- traverseOf aarAnnSelect transformSelect annRel
          pure (AFArrayRelation . ASSimple $ transformed, Nothing)
        AFArrayRelation (ASAggregate aggRel) -> do
          transformed <- traverseOf aarAnnSelect transformAggregateSelect aggRel
          pure (AFArrayRelation . ASAggregate $ transformed, Nothing)
        AFArrayRelation (ASConnection annRel) -> do
          transformed <- traverseOf aarAnnSelect transformConnectionSelect annRel
          pure (AFArrayRelation . ASConnection $ transformed, Nothing)
        AFComputedField computedField computedFieldName computedFieldSelect -> do
          transformed <- case computedFieldSelect of
            CFSScalar cfss -> pure $ CFSScalar cfss
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
              Just $ createRemoteJoin (HashMap.intersection joinColumnAliases _rrsLHSJoinFields) _rrsRelationship
            )
        AFNestedObject nestedObj ->
          (,Nothing) . AFNestedObject <$> transformNestedObjectSelect nestedObj
        AFNestedArray supportsNestedArray (ANASSimple nestedArrayField) -> do
          (,Nothing) . AFNestedArray supportsNestedArray . ANASSimple . fst <$> transformAnnField nestedArrayField
        AFNestedArray supportsNestedArray (ANASAggregate agg) -> do
          transformed <- transformAggregateSelect agg
          pure (AFNestedArray supportsNestedArray (ANASAggregate transformed), Nothing)

  -- Produces a list of transformed fields that may or may not have an
  -- associated remote join.
  annotatedFields <-
    fields & traverseFields transformAnnField

  let transformedFields = (fmap . fmap) fst annotatedFields
      remoteJoins =
        annotatedFields & mapMaybe \(fieldName, (_, mRemoteJoin)) ->
          (QualifiedFieldName Nothing (getFieldNameTxt fieldName),) <$> mRemoteJoin

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
      HashMap.fromList
        $ [ (_acfColumn annColumn, alias)
            | (alias, annColumn) <- getFields _AFColumn fields
          ]

    -- This is a map of computed field name to its alias of all computed fields
    -- in the selection set.
    computedFields :: HashMap.HashMap ComputedFieldName FieldName
    computedFields =
      HashMap.fromList
        $ [ (fieldName, alias)
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
            HashMap.unions $ map (_rrsLHSJoinFields . snd) $ getFields _AFRemote fields
          annotatedJoinColumns = HashMap.mapWithKey annotateDBJoinField $ lhsJoinFields
          phantomFields_ =
            toList annotatedJoinColumns & mapMaybe \(joinField, alias) ->
              case alias of
                JCSelected _ -> Nothing
                JCPhantom a -> case joinField of
                  JoinColumn column columnType ->
                    let annotatedColumn =
                          AFColumn $ AnnColumnField column columnType False Nothing NoRedaction
                     in Just (a, annotatedColumn)
                  JoinComputedField computedFieldInfo ->
                    Just (a, mkScalarComputedFieldSelect computedFieldInfo)
       in (fmap snd annotatedJoinColumns, phantomFields_)

    mkScalarComputedFieldSelect ::
      forall b.
      (Backend b) =>
      ScalarComputedField b ->
      AnnFieldG b Void (UnpreparedValue b)
    mkScalarComputedFieldSelect ScalarComputedField {..} =
      let functionArgs =
            flip FunctionArgsExp mempty $ fromComputedFieldImplicitArguments @b UVSession _scfComputedFieldImplicitArgs
          fieldSelect =
            CFSScalar $ ComputedFieldScalarSelect _scfFunction functionArgs _scfType Nothing NoRedaction
       in AFComputedField _scfXField _scfName fieldSelect

-- | Transforms an action's selection set.
--
-- This function takes an 'ActionFieldsG', which corresponds to a selection of
-- fields on the result of an action, and extracts remote joins: for every field
-- we encounter that maps to a remote destination (either a source or a remote
-- schema), we replace it with a phantom field and 'collect' the corresponding
-- 'RemoteJoin'.
transformActionFields ::
  ActionFieldsG (RemoteRelationshipField UnpreparedValue) ->
  Collector ActionFields
transformActionFields fields = do
  -- Produces a list of transformed fields that may or may not have an
  -- associated remote join.
  annotatedFields <-
    fields & traverseFields \case
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
            Just $ createRemoteJoin (HashMap.intersection joinColumnAliases _arrsLHSJoinFields) _arrsRelationship
          )
      ACFNestedObject fn fs ->
        (,Nothing) . ACFNestedObject fn <$> transformActionFields fs

  let transformedFields = (fmap . fmap) fst annotatedFields
      remoteJoins =
        annotatedFields & mapMaybe \(fieldName, (_, mRemoteJoin)) ->
          (QualifiedFieldName Nothing (getFieldNameTxt fieldName),) <$> mRemoteJoin

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
      HashMap.fromList
        $ [ (name, alias)
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
            HashMap.unions $ map (_arrsLHSJoinFields . snd) $ getFields _ACFRemote fields
          annotatedJoinColumns = HashMap.mapWithKey annotateJoinField $ lhsJoinFields
          phantomFields_ :: ([(FieldName, ActionFieldG Void)]) =
            toList annotatedJoinColumns & mapMaybe \(joinField, alias) ->
              case alias of
                JCSelected _ -> Nothing
                JCPhantom a ->
                  let annotatedColumn =
                        ACFScalar joinField
                   in Just (a, annotatedColumn)
       in (fmap snd annotatedJoinColumns, phantomFields_)

-- | Transforms a GraphQL selection set.
--
-- This function takes an 'SelectionSet', which corresponds to a selection of
-- fields on a remote GraphQL schema, and extracts remote joins: for every field
-- we encounter that maps to a remote destination (either a source or another
-- remote schema), we replace it with a phantom field and 'collect' the
-- corresponding 'RemoteJoin'.
transformObjectSelectionSet ::
  -- | The type name on which this selection set is defined; this is only
  -- expected to be provided for unions and interfaces, not for regular objects,
  -- as this is used to determine whether a selection set is potentially
  -- "ambiguous" or not, and regular objects cannot. This will be used as the
  -- type name in the 'QualifiedFieldName' key of the join tree if this
  -- selection set or its subselections contain remote joins.
  Maybe G.Name ->
  ObjectSelectionSet (RemoteRelationshipField UnpreparedValue) var ->
  Collector (ObjectSelectionSet Void var)
transformObjectSelectionSet typename selectionSet = do
  -- we need to keep track of whether any subfield contained a remote join
  (annotatedFields, subfieldsContainRemoteJoins) <-
    listens isJust
      $ flip InsOrdHashMap.traverseWithKey selectionSet \alias field ->
        withField (G.unName <$> typename) (G.unName alias) do
          case field of
            FieldGraphQL f -> (,Nothing) <$> transformGraphQLField f
            FieldRemote SchemaRemoteRelationshipSelect {..} -> do
              pure
                ( mkPlaceholderField alias,
                  Just $ createRemoteJoin (HashMap.intersection joinColumnAliases _srrsLHSJoinFields) _srrsRelationship
                )
  let internalTypeAlias = Name.___hasura_internal_typename
      remoteJoins = InsOrdHashMap.mapMaybe snd annotatedFields
      additionalFields =
        if
          | isJust typename && (not (null remoteJoins) || subfieldsContainRemoteJoins) ->
              -- We are in a situation in which the type name matters, and we know
              -- that there is at least one remote join in this part of tree, meaning
              -- we might need to branch on the typename when traversing the join
              -- tree: we insert a custom field that will return the type name.
              InsOrdHashMap.singleton internalTypeAlias
                $ mkGraphQLField
                  (Just internalTypeAlias)
                  GName.___typename
                  mempty
                  mempty
                  SelectionSetNone
          | otherwise ->
              -- Either the typename doesn't matter, or this tree doesn't have remote
              -- joins; this selection set isn't "ambiguous".
              mempty
      transformedFields = fmap fst annotatedFields <> additionalFields
  case NEMap.fromList $ InsOrdHashMap.toList remoteJoins of
    Nothing -> pure $ fmap FieldGraphQL transformedFields
    Just neRemoteJoins -> do
      collect $ NEMap.mapKeys (\fieldGName -> QualifiedFieldName (G.unName <$> typename) (G.unName fieldGName)) neRemoteJoins
      pure
        $ fmap
          FieldGraphQL
          (transformedFields <> InsOrdHashMap.fromList [(_fAlias fld, fld) | fld <- toList phantomFields])
  where
    nameToField = FieldName . G.unName
    allAliases = map (nameToField . fst) $ InsOrdHashMap.toList selectionSet

    mkPlaceholderField alias =
      mkGraphQLField (Just alias) GName.___typename mempty mempty SelectionSetNone

    -- A map of graphql scalar fields (without any arguments) to their aliases
    -- in the selection set. We do not yet support lhs join fields which take
    -- arguments. To be consistent with that, we ignore fields with arguments
    noArgsGraphQLFields =
      HashMap.fromList
        $ flip mapMaybe (InsOrdHashMap.toList selectionSet) \(alias, field) -> case field of
          FieldGraphQL f ->
            if null (_fArguments f)
              then Just (_fName f, FieldName $ G.unName alias)
              else Nothing
          FieldRemote _ -> Nothing

    annotateLHSJoinField fieldName lhsJoinField =
      let columnAlias =
            getJoinColumnAlias fieldName lhsJoinField noArgsGraphQLFields allAliases
          -- This alias is generated in 'getJoinColumnAlias', and is guaranteed
          -- to be a valid GraphQLName.
          columnGraphQLName =
            G.mkName $ getFieldNameTxt $ getAliasFieldName columnAlias
       in ( mkGraphQLField
              columnGraphQLName
              lhsJoinField
              mempty
              mempty
              SelectionSetNone,
            columnAlias
          )

    (joinColumnAliases, phantomFields) =
      let lhsJoinFields =
            HashMap.unions $ map _srrsLHSJoinFields $ mapMaybe (preview _FieldRemote) $ toList selectionSet
          annotatedJoinColumns = HashMap.mapWithKey annotateLHSJoinField lhsJoinFields
       in (fmap snd annotatedJoinColumns, fmap fst annotatedJoinColumns)

-------------------------------------------------------------------------------
-- Internal helpers

-- | Converts a remote relationship field into a 'RemoteJoin' that
-- the execution engine understands.
createRemoteJoin ::
  -- We need information about 'how' the lhs join fields appear in the lhs
  -- response to construct a 'RemoteJoin' node
  HashMap.HashMap FieldName JoinColumnAlias ->
  -- The remote relationship field as captured in the IR
  RemoteRelationshipField UnpreparedValue ->
  RemoteJoin
createRemoteJoin joinColumnAliases = \case
  RemoteSchemaField RemoteSchemaSelect {..} ->
    let inputArgsToMap = HashMap.fromList . map (_rfaArgument &&& _rfaValue)
        (transformedSchemaRelationship, schemaRelationshipJoins) =
          getRemoteJoinsGraphQLSelectionSet _rselSelection
        remoteJoin =
          RemoteSchemaJoin
            (inputArgsToMap _rselArgs)
            _rselResultCustomizer
            transformedSchemaRelationship
            joinColumnAliases
            _rselFieldCall
            _rselRemoteSchema
     in RemoteJoinRemoteSchema remoteJoin schemaRelationshipJoins
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
            _rssJoinMapping & HashMap.mapMaybeWithKey
              \joinFieldName (rhsColumnType, rhsColumn) ->
                (,(rhsColumn, rhsColumnType))
                  <$> HashMap.lookup joinFieldName joinColumnAliases
          anySourceJoin =
            AB.mkAnyBackend
              $ RemoteSourceJoin
                _rssName
                _rssConfig
                transformedSourceRelationship
                joinColumns
                _rssStringifyNums
       in RemoteJoinSource anySourceJoin sourceRelationshipJoins

-- | Constructs a 'JoinColumnAlias' for a given field in a selection set.
--
-- If the field was already requested, we leave it unchanged, to avoid
-- double-fetching the same information. However, if this field is a "phantom"
-- field, that we only add for the purpose of fetching a join key, we rename it
-- in a way that is guaranteed to avoid conflicts.
--
-- NOTE: if the @fieldName@ argument is a valid GraphQL name, then the
-- constructed alias MUST also be a valid GraphQL name.
getJoinColumnAlias ::
  (Hashable field) =>
  FieldName ->
  field ->
  HashMap field FieldName ->
  [FieldName] ->
  JoinColumnAlias
getJoinColumnAlias fieldName field selectedFields allAliases =
  case HashMap.lookup field selectedFields of
    Nothing -> JCPhantom uniqueAlias
    Just fieldAlias -> JCSelected fieldAlias
  where
    -- This generates an alias for a phantom field that does not conflict with
    -- any of the existing aliases in the selection set
    --
    -- If we generate a unique name for each field name which is longer than
    -- the longest alias in the selection set, the generated name would be
    -- unique.
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

-- | Get the fields targeted by some 'Traversal' for an arbitrary list of
-- tuples, discarding any elements whose fields cannot be focused upon.
getFields :: Traversal' super sub -> [(any, super)] -> [(any, sub)]
getFields focus = mapMaybe (traverse $ preview focus)
