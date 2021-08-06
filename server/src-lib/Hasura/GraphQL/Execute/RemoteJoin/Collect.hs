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

import           Control.Lens                            (Traversal', _2, preview)
import           Control.Monad.Writer


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

-- | A writer monad used to collect together all remote joins
-- appearing in some data structure.
--
-- In the functions below, the 'withField' function is used to track the
-- context of the path from the root of the current selection set.
--
-- It is important that we work bottom-up, and do not 'collect' duplicate
-- field names at any level, because the 'Semigroup' instance for 'RemoteJoins'
-- does not allow for these duplicates.
newtype Collector a = Collector { runCollector :: (a, Maybe RemoteJoins) }
  deriving (Functor, Applicative, Monad, MonadWriter (Maybe RemoteJoins))
    via Writer (Maybe RemoteJoins)

-- | Collect some remote joins appearing at the given field names in the current
-- context.
collect :: NonEmpty (FieldName, RemoteJoin) -> Collector ()
collect = tell . Just . JoinTree . fmap (second Leaf)

-- | Keep track of the given field name in the current path from the root of the
-- selection set.
withField :: FieldName -> Collector a -> Collector a
withField name = censor (fmap wrap) where
  wrap rjs = JoinTree ((name, Tree rjs) :| [])

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
  => AnnSimpleSelectG b (RemoteSelect UnpreparedValue) (UnpreparedValue b)
  -> (AnnSimpleSelectG b (Const Void) (UnpreparedValue b), Maybe RemoteJoins)
getRemoteJoinsSelect =
  runCollector . transformSelect

-- | Traverse through @'AnnAggregateSelect' and collect remote join fields (if any).
getRemoteJoinsAggregateSelect
  :: Backend b
  => AnnAggregateSelectG b (RemoteSelect UnpreparedValue) (UnpreparedValue b)
  -> (AnnAggregateSelectG b (Const Void) (UnpreparedValue b), Maybe RemoteJoins)
getRemoteJoinsAggregateSelect =
  runCollector . transformAggregateSelect

-- | Traverse through @'ConnectionSelect' and collect remote join fields (if any).
getRemoteJoinsConnectionSelect
  :: Backend b
  => ConnectionSelect b (RemoteSelect UnpreparedValue) (UnpreparedValue b)
  -> (ConnectionSelect b (Const Void) (UnpreparedValue b), Maybe RemoteJoins)
getRemoteJoinsConnectionSelect =
  runCollector . transformConnectionSelect

-- | Traverse through 'MutationOutput' and collect remote join fields (if any)
getRemoteJoinsMutationOutput
  :: Backend b
  => MutationOutputG b (RemoteSelect UnpreparedValue) (UnpreparedValue b)
  -> (MutationOutputG b (Const Void) (UnpreparedValue b), Maybe RemoteJoins)
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
              MCount         -> pure MCount
              MExp t         -> pure $ MExp t
              MRet annFields -> MRet <$> transformAnnFields annFields


-- local helpers

getRemoteJoinsAnnFields
  :: Backend b
  => AnnFieldsG b (RemoteSelect UnpreparedValue) (UnpreparedValue b)
  -> (AnnFieldsG b (Const Void) (UnpreparedValue b), Maybe RemoteJoins)
getRemoteJoinsAnnFields =
  runCollector . transformAnnFields

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
            runCollector . transformAsyncFields $
            _aaaqFields async
      in (async { _aaaqFields = fields' }, remoteJoins)

    transformAsyncFields fields =
      for fields $ \(fieldName, field) -> withField fieldName do
        (fieldName,) <$> case field of
          AsyncTypename t -> pure $ AsyncTypename t
          AsyncOutput outputFields ->
            AsyncOutput <$> transformAnnFields outputFields
          AsyncId -> pure AsyncId
          AsyncCreatedAt -> pure AsyncCreatedAt
          AsyncErrors -> pure AsyncErrors

getRemoteJoinsActionMutation
  :: (Backend b)
  => ActionMutation b (RemoteSelect UnpreparedValue) (UnpreparedValue b)
  -> (ActionMutation b (Const Void) (UnpreparedValue b), Maybe RemoteJoins)
getRemoteJoinsActionMutation = \case
  AMAsync async -> (AMAsync async, Nothing)
  AMSync  sync  -> first AMSync $ getRemoteJoinsSyncAction sync


transformSelect
  :: Backend b
  => AnnSimpleSelectG b (RemoteSelect UnpreparedValue) (UnpreparedValue b)
  -> Collector (AnnSimpleSelectG b (Const Void) (UnpreparedValue b))
transformSelect select@AnnSelectG{_asnFields = fields} = do
  -- Transform selects in array, object and computed fields
  transformedFields <- transformAnnFields fields
  pure select{_asnFields = transformedFields}

transformAggregateSelect
  :: Backend b
  => AnnAggregateSelectG b (RemoteSelect UnpreparedValue) (UnpreparedValue b)
  -> Collector (AnnAggregateSelectG b (Const Void) (UnpreparedValue b))
transformAggregateSelect select@AnnSelectG{_asnFields = aggFields} = do
  transformedFields <- for aggFields \(fieldName, aggField) ->
    withField fieldName $ case aggField of
    TAFAgg agg                  -> pure (fieldName, TAFAgg agg)
    TAFExp t                    -> pure (fieldName, TAFExp t)
    TAFNodes nodesAgg annFields -> do
      transformed <- transformAnnFields annFields
      pure (fieldName, TAFNodes nodesAgg transformed)
  pure select{_asnFields = transformedFields}

transformConnectionSelect
  :: forall b. Backend b
  => ConnectionSelect b (RemoteSelect UnpreparedValue) (UnpreparedValue b)
  -> Collector (ConnectionSelect b (Const Void) (UnpreparedValue b))
transformConnectionSelect connSelect@ConnectionSelect{..} = do
  transformedFields <- for (_asnFields _csSelect) \(fieldName, connField) ->
    withField fieldName $ case connField of
      ConnectionTypename t  -> pure (fieldName, ConnectionTypename t)
      ConnectionPageInfo p  -> pure (fieldName, ConnectionPageInfo p)
      ConnectionEdges edges -> do
        transformed <- transformEdges edges
        pure (fieldName, ConnectionEdges transformed)

  let select = _csSelect{_asnFields = transformedFields}
  pure connSelect{ _csSelect = select }
  where
    transformEdges
      :: [(FieldName, EdgeField b (RemoteSelect UnpreparedValue) (UnpreparedValue b))]
      -> Collector [(FieldName, EdgeField b (Const Void) (UnpreparedValue b))]
    transformEdges edgeFields = for edgeFields \(fieldName, edgeField) ->
      withField fieldName $ case edgeField of
        EdgeTypename t     -> pure (fieldName, EdgeTypename t)
        EdgeCursor         -> pure (fieldName, EdgeCursor)
        EdgeNode annFields -> do
          transformed <- transformAnnFields annFields
          pure (fieldName, EdgeNode transformed)

transformObjectSelect
  :: Backend b
  => AnnObjectSelectG b (RemoteSelect UnpreparedValue) (UnpreparedValue b)
  -> Collector (AnnObjectSelectG b (Const Void) (UnpreparedValue b))
transformObjectSelect select@AnnObjectSelectG{_aosFields = fields} = do
  transformedFields <- transformAnnFields fields
  pure select{_aosFields = transformedFields}

transformAnnFields
  :: forall b . Backend b
  => AnnFieldsG b (RemoteSelect UnpreparedValue) (UnpreparedValue b)
  -> Collector (AnnFieldsG b (Const Void) (UnpreparedValue b))
transformAnnFields fields = do
  -- Produces a list of transformed fields that may or may not have an
  -- associated remote join.
  annotatedFields <- for fields \(fieldName, field') -> withField fieldName do
    -- FIXME: There's way too much going on in this 'case .. of' block...
    (fieldName,) <$> case field' of
      -- AnnFields which do not need to be transformed.
      AFNodeId x qt pkeys -> pure (AFNodeId x qt pkeys, Nothing)
      AFColumn c          -> pure (AFColumn c, Nothing)
      AFExpression t      -> pure (AFExpression t, Nothing)
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
          CFSScalar cfss cbe  -> pure $ CFSScalar cfss cbe
          CFSTable jsonAggSel annSel -> do
            transformed <- transformSelect annSel
            pure $ CFSTable jsonAggSel transformed
        pure (AFComputedField computedField computedFieldName transformed, Nothing)
      -- Remote AnnFields, whose elements require annotation so that they can be
      -- used to construct a remote join.
      --
      -- We generate this so that the response has a key with the relationship,
      -- without which preserving the order of fields in the final response
      -- would require a lot of bookkeeping.
      AFRemote (RemoteSelectRemoteSchema RemoteSchemaSelect{..}) ->
        let
          annotatedJoinColumns =
            Map.fromList $ map annotateDBJoinField (toList _rselHasuraFields)
          phantomColumns = annotatedJoinColumns & Map.mapMaybe \(columnInfo, alias) ->
            case alias of
              JCSelected _ -> Nothing
              JCPhantom a  -> Just (columnInfo, a)
          joinColumnAliases = fmap snd annotatedJoinColumns
          inputArgsToMap = Map.fromList . map (_rfaArgument &&& _rfaValue)
          remoteJoin = RemoteJoinRemoteSchema $ RemoteSchemaJoin
            (inputArgsToMap _rselArgs)
            _rselResultCustomizer
            _rselSelection
            joinColumnAliases
            _rselFieldCall
            _rselRemoteSchema
          annotatedJoin = Just (phantomColumns, remoteJoin)
        in
          pure (remoteAnnPlaceholder, annotatedJoin)

  let
    transformedFields = (fmap . fmap) fst annotatedFields
    remoteJoins = annotatedFields & mapMaybe \(fieldName, (_, mRemoteJoin)) ->
      mRemoteJoin <&> \remoteJoin -> (fieldName, remoteJoin)
  case NE.nonEmpty remoteJoins of
    Nothing -> pure transformedFields
    Just neRemoteJoins -> do
      let
        phantomFields =
          (Map.elems . Map.unions . map (fst . snd) $ remoteJoins) <&>
            \(joinField, alias) -> case joinField of
              JoinColumn columnInfo ->
                let column = AFColumn $ AnnColumnField columnInfo False Nothing Nothing
                in (alias, column)
              JoinComputedField computedFieldInfo ->
                (alias, mkScalarComputedFieldSelect computedFieldInfo)
      collect $ (fmap . fmap) snd neRemoteJoins
      pure $ transformedFields <> phantomFields
  where
    -- Placeholder text to annotate a remote relationship field.
    remoteAnnPlaceholder :: AnnFieldG b (Const Void) (UnpreparedValue b)
    remoteAnnPlaceholder = AFExpression "remote relationship placeholder"

    -- Annotate a 'DBJoinField' with its field name and an alias so that it may
    -- be used to construct a remote join.
    annotateDBJoinField :: DBJoinField b -> (FieldName, (DBJoinField b, JoinColumnAlias))
    annotateDBJoinField = \case
      jc@(JoinColumn columnInfo) ->
        let
          column = pgiColumn columnInfo
          columnFieldName = fromCol @b $ column
          alias = getJoinColumnAlias columnFieldName column columnFields
        in
          (columnFieldName, (jc, alias))
      jcf@(JoinComputedField ScalarComputedField{..}) ->
        let
          computedFieldName = fromComputedField _scfName
          alias = getJoinColumnAlias computedFieldName _scfName computedFields
        in
          (computedFieldName, (jcf, alias))

    -- Get the fields targeted by some 'Traversal' for an arbitrary list of
    -- tuples, discarding any elements whose fields cannot be focused upon.
    getFields :: Traversal' super sub -> [(any, super)] -> [(any, sub)]
    getFields focus = mapMaybe (traverse $ preview focus)

    -- This is a map of column name to its alias of all columns in the
    -- selection set.
    columnFields :: HashMap (Column b) FieldName
    columnFields = Map.fromList $
      [ (pgiColumn . _acfInfo $ annColumn, alias)
      | (alias, annColumn) <- getFields _AFColumn fields
      ]

    -- This is a map of computed field name to its alias of all computed fields
    -- in the selection set.
    computedFields :: Map.HashMap ComputedFieldName FieldName
    computedFields = Map.fromList $
      [ (fieldName, alias)
      -- Note that we do not currently care about input arguments to a computed
      -- field because only computed fields which do not accept input arguments
      -- are currently allowed.
      | (alias, fieldName) <- getFields (_AFComputedField._2) fields
      ]

    getJoinColumnAlias
      :: (Eq field, Hashable field)
      => FieldName
      -> field
      -> HashMap field FieldName
      -> JoinColumnAlias
    getJoinColumnAlias fieldName field selectedFields =
      case Map.lookup field selectedFields of
        Nothing         -> makeUniqueAlias fieldName
        Just fieldAlias -> JCSelected fieldAlias

    longestAliasLength = maximum $ map (T.length . coerce . fst) fields

    -- This generates an alias for a phantom field that does not conflict with
    -- any of the existing aliases in the seleciton set
    --
    -- If we generate a unique name for each field name which is longer than
    -- the longest alias in the selection set, the generated name would be
    -- unique
    makeUniqueAlias :: FieldName -> JoinColumnAlias
    makeUniqueAlias fieldName =
      let suffix = "_join_column" <>
            -- 12 is the length of "_join_column"
            T.replicate ((longestAliasLength - (T.length (coerce fieldName) + 12)) + 1) "_"
      in JCPhantom $ fieldName <> FieldName suffix

    transformAnnRelation
      :: (t -> Collector a)
      -> AnnRelationSelectG b t
      -> Collector (AnnRelationSelectG b a)
    transformAnnRelation transform relation@(AnnRelationSelectG _ _ select) = do
      transformedSelect <- transform select
      pure $ relation{ aarAnnSelect = transformedSelect }

    mkScalarComputedFieldSelect
      :: ScalarComputedField b
      -> (AnnFieldG b (Const Void) (UnpreparedValue b))
    mkScalarComputedFieldSelect ScalarComputedField{..} =
      let functionArgs = flip FunctionArgsExp mempty
            $ functionArgsWithTableRowAndSession UVSession _scfTableArgument _scfSessionArgument
          fieldSelect = flip CFSScalar Nothing
            $ ComputedFieldScalarSelect _scfFunction functionArgs _scfType Nothing
      in AFComputedField _scfXField _scfName fieldSelect
