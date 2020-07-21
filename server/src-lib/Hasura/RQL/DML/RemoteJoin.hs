-- | Types and Functions for resolving remote join fields
module Hasura.RQL.DML.RemoteJoin
  ( executeQueryWithRemoteJoins
  , getRemoteJoins
  , getRemoteJoinsAggregateSelect
  , getRemoteJoinsMutationOutput
  , getRemoteJoinsConnectionSelect
  , RemoteJoins
  ) where

import           Hasura.Prelude

import           Control.Lens
import           Data.List                              (nub)
import           Data.Validation

import           Hasura.EncJSON
import           Hasura.GraphQL.RemoteServer            (execRemoteGQ')
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.GraphQL.Utils
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.DML.Returning
import           Hasura.RQL.DML.Select.Types
import           Hasura.RQL.Types
import           Hasura.Server.Version                  (HasVersion)
import           Hasura.Session
import           Hasura.SQL.Types                       ((<<>))

import qualified Hasura.SQL.DML                         as S

import qualified Data.Aeson                             as A
import qualified Data.Aeson.Ordered                     as AO
import qualified Data.Environment                       as Env
import qualified Data.HashMap.Strict                    as Map
import qualified Data.HashMap.Strict.Extended           as Map
import qualified Data.HashMap.Strict.InsOrd             as OMap
import qualified Data.HashSet                           as HS
import qualified Data.List.NonEmpty                     as NE
import qualified Data.Text                              as T
import qualified Database.PG.Query                      as Q
import qualified Hasura.Tracing                         as Tracing
import qualified Language.GraphQL.Draft.Printer.Text    as G
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Types                     as N

-- | Executes given query and fetch response JSON from Postgres. Substitutes remote relationship fields.
executeQueryWithRemoteJoins
  :: ( HasVersion
     , MonadTx m
     , MonadIO m
     , Tracing.MonadTrace m
     )
  => Env.Environment
  -> HTTP.Manager
  -> [N.Header]
  -> UserInfo
  -> Q.Query
  -> [Q.PrepArg]
  -> RemoteJoins
  -> m EncJSON
executeQueryWithRemoteJoins env manager reqHdrs userInfo q prepArgs rjs = do
  -- Step 1: Perform the query on database and fetch the response
  pgRes <- runIdentity . Q.getRow <$> liftTx (Q.rawQE dmlTxErrorHandler q prepArgs True)
  jsonRes <- either (throw500 . T.pack) pure $ AO.eitherDecode pgRes
  -- Step 2: Traverse through the JSON obtained in above step and generate composite JSON value with remote joins
  compositeJson <- traverseQueryResponseJSON rjMap jsonRes
  let remoteJoins = collectRemoteFields compositeJson
  -- Step 3: Make queries to remote server and fetch graphql response
  remoteServerResp <- fetchRemoteJoinFields env manager reqHdrs userInfo remoteJoins
  -- Step 4: Replace remote fields in composite json with remote join values
  AO.toEncJSON <$> replaceRemoteFields compositeJson remoteServerResp
  where
    rjMap = Map.fromList $ toList rjs

-- | Path to the remote join field in query response JSON from Postgres.
newtype FieldPath = FieldPath {unFieldPath :: [FieldName]}
  deriving (Show, Eq, Semigroup, Monoid, Hashable)

appendPath :: FieldName -> FieldPath -> FieldPath
appendPath fieldName = FieldPath . (<> [fieldName]) . unFieldPath

-- | The counter which is used to append the alias generated for remote field. See 'pathToAlias'.
-- This guarentees the uniqueness of the alias.
newtype Counter = Counter {unCounter :: Int}
  deriving (Show, Eq)

incCounter :: Counter -> Counter
incCounter = Counter . (+1) . unCounter

getCounter :: MonadState Counter m => m Counter
getCounter = do
  c <- get
  modify incCounter
  pure c

-- | Generate the alias for remote field.
pathToAlias :: FieldPath -> Counter -> G.Alias
pathToAlias path counter =
  G.Alias $ G.Name $ T.intercalate "_" (map getFieldNameTxt $ unFieldPath path)
  <> "__" <> (T.pack . show . unCounter) counter

-- | A 'RemoteJoin' represents the context of remote relationship to be extracted from 'AnnFldG's.
data RemoteJoin
  = RemoteJoin
  { _rjName          :: !FieldName -- ^ The remote join field name.
  , _rjArgs          :: ![RemoteFieldArgument] -- ^ User-provided arguments with variables.
  , _rjSelSet        :: !G.SelectionSet -- ^ User-provided selection set of remote field.
  , _rjHasuraFields  :: !(HashSet FieldName) -- ^ Table fields.
  , _rjFieldCall     :: !(NonEmpty FieldCall) -- ^ Remote server fields.
  , _rjRemoteSchema  :: !RemoteSchemaInfo -- ^ The remote schema server info.
  , _rjPhantomFields :: ![PGColumnInfo]
    -- ^ Hasura fields which are not in the selection set, but are required as
    -- parameters to satisfy the remote join.
  } deriving (Show, Eq)

type RemoteJoins = NE.NonEmpty (FieldPath, NE.NonEmpty RemoteJoin)
type RemoteJoinMap = Map.HashMap FieldPath (NE.NonEmpty RemoteJoin)

mapToNonEmpty :: RemoteJoinMap -> Maybe RemoteJoins
mapToNonEmpty = NE.nonEmpty . Map.toList

-- | Traverse through 'AnnSimpleSel' and collect remote join fields (if any).
getRemoteJoins :: AnnSimpleSel -> (AnnSimpleSel, Maybe RemoteJoins)
getRemoteJoins =
  second mapToNonEmpty . flip runState mempty . transformSelect mempty

transformSelect :: FieldPath -> AnnSimpleSel -> State RemoteJoinMap AnnSimpleSel
transformSelect path sel = do
  let fields = _asnFields sel
  -- Transform selects in array, object and computed fields
  transformedFields <- transformAnnFields path fields
  pure sel{_asnFields = transformedFields}

transformObjectSelect :: FieldPath -> AnnObjectSelect -> State RemoteJoinMap AnnObjectSelect
transformObjectSelect path sel = do
  let fields = _aosFields sel
  transformedFields <- transformAnnFields path fields
  pure sel{_aosFields = transformedFields}

-- | Traverse through @'AnnAggregateSelect' and collect remote join fields (if any).
getRemoteJoinsAggregateSelect :: AnnAggregateSelect -> (AnnAggregateSelect, Maybe RemoteJoins)
getRemoteJoinsAggregateSelect =
  second mapToNonEmpty . flip runState mempty . transformAggregateSelect mempty

transformAggregateSelect
  :: FieldPath
  -> AnnAggregateSelect
  -> State RemoteJoinMap AnnAggregateSelect
transformAggregateSelect path sel = do
  let aggFields = _asnFields sel
  transformedFields <- forM aggFields $ \(fieldName, aggField) ->
    (fieldName,) <$> case aggField of
      TAFAgg agg         -> pure $ TAFAgg agg
      TAFNodes annFields -> TAFNodes <$> transformAnnFields (appendPath fieldName path) annFields
      TAFExp t           -> pure $ TAFExp t
  pure sel{_asnFields = transformedFields}

-- | Traverse through @'ConnectionSelect' and collect remote join fields (if any).
getRemoteJoinsConnectionSelect :: ConnectionSelect S.SQLExp -> (ConnectionSelect S.SQLExp, Maybe RemoteJoins)
getRemoteJoinsConnectionSelect =
  second mapToNonEmpty . flip runState mempty . transformConnectionSelect mempty

transformConnectionSelect
  :: FieldPath
  -> ConnectionSelect S.SQLExp
  -> State RemoteJoinMap (ConnectionSelect S.SQLExp)
transformConnectionSelect path ConnectionSelect{..} = do
  let connectionFields = _asnFields _csSelect
  transformedFields <- forM connectionFields $ \(fieldName, field) ->
    (fieldName,) <$> case field of
      ConnectionTypename t -> pure $ ConnectionTypename t
      ConnectionPageInfo p -> pure $ ConnectionPageInfo p
      ConnectionEdges edges -> ConnectionEdges <$> transformEdges (appendPath fieldName path) edges
  let select = _csSelect{_asnFields = transformedFields}
  pure $ ConnectionSelect _csPrimaryKeyColumns _csSplit _csSlice select
  where
    transformEdges edgePath edgeFields =
      forM edgeFields $ \(fieldName, edgeField) ->
      (fieldName,) <$> case edgeField of
        EdgeTypename t -> pure $ EdgeTypename t
        EdgeCursor -> pure EdgeCursor
        EdgeNode annFields ->
          EdgeNode <$> transformAnnFields (appendPath fieldName edgePath) annFields


-- | Traverse through 'MutationOutput' and collect remote join fields (if any)
getRemoteJoinsMutationOutput :: MutationOutput -> (MutationOutput, Maybe RemoteJoins)
getRemoteJoinsMutationOutput =
  second mapToNonEmpty . flip runState mempty . transformMutationOutput mempty
  where
    transformMutationOutput :: FieldPath -> MutationOutput -> State RemoteJoinMap MutationOutput
    transformMutationOutput path = \case
      MOutMultirowFields mutationFields ->
        MOutMultirowFields <$> transfromMutationFields mutationFields
      MOutSinglerowObject annFields ->
        MOutSinglerowObject <$> transformAnnFields path annFields
      where
        transfromMutationFields fields =
          forM fields $ \(fieldName, field) -> do
          let fieldPath = appendPath fieldName path
          (fieldName,) <$> case field of
            MCount         -> pure MCount
            MExp t         -> pure $ MExp t
            MRet annFields -> MRet <$> transformAnnFields fieldPath annFields

transformAnnFields :: FieldPath -> AnnFields -> State RemoteJoinMap AnnFields
transformAnnFields path fields = do
  let pgColumnFields = map fst $ getFields _AFColumn fields
      remoteSelects = getFields _AFRemote fields
      remoteJoins = flip map remoteSelects $ \(fieldName, remoteSelect) ->
        let RemoteSelect argsMap selSet hasuraColumns remoteFields rsi = remoteSelect
            hasuraColumnL = toList hasuraColumns
            hasuraColumnFields = HS.fromList $ map (fromPGCol . pgiColumn) hasuraColumnL
            phantomColumns = filter ((`notElem` pgColumnFields) . fromPGCol . pgiColumn) hasuraColumnL
        in RemoteJoin fieldName argsMap selSet hasuraColumnFields remoteFields rsi phantomColumns

  transformedFields <- forM fields $ \(fieldName, field) -> do
    let fieldPath = appendPath fieldName path
    (fieldName,) <$> case field of
      AFNodeId qt pkeys -> pure $ AFNodeId qt pkeys
      AFColumn c -> pure $ AFColumn c
      AFObjectRelation annRel ->
        AFObjectRelation <$> transformAnnRelation annRel (transformObjectSelect fieldPath)
      AFArrayRelation (ASSimple annRel) ->
        AFArrayRelation . ASSimple <$> transformAnnRelation annRel (transformSelect fieldPath)
      AFArrayRelation (ASAggregate aggRel) ->
        AFArrayRelation . ASAggregate <$> transformAnnAggregateRelation fieldPath aggRel
      AFArrayRelation (ASConnection annRel) ->
        AFArrayRelation . ASConnection <$> transformArrayConnection fieldPath annRel
      AFComputedField computedField ->
        AFComputedField <$> case computedField of
          CFSScalar _         -> pure computedField
          CFSTable jas annSel -> CFSTable jas <$> transformSelect fieldPath annSel
      AFRemote rs -> pure $ AFRemote rs
      AFExpression t     -> pure $ AFExpression t

  case NE.nonEmpty remoteJoins of
    Nothing -> pure transformedFields
    Just nonEmptyRemoteJoins -> do
      let phantomColumns = map (\ci -> (fromPGCol $ pgiColumn ci, AFColumn $ AnnColumnField ci False Nothing)) $
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

type CompositeObject a = OMap.InsOrdHashMap Text (CompositeValue a)

-- | A hybrid JSON value representation which captures the context of remote join field in type parameter.
data CompositeValue a
  = CVOrdValue !AO.Value
  | CVObject !(CompositeObject a)
  | CVObjectArray ![CompositeValue a]
  | CVFromRemote !a
  deriving (Show, Eq, Functor, Foldable, Traversable)

collectRemoteFields :: CompositeValue a -> [a]
collectRemoteFields = toList

compositeValueToJSON :: CompositeValue AO.Value -> AO.Value
compositeValueToJSON = \case
  CVOrdValue v -> v
  CVObject obj -> AO.object $ OMap.toList $ OMap.map compositeValueToJSON obj
  CVObjectArray vals -> AO.array $ map compositeValueToJSON vals
  CVFromRemote v -> v

-- | A 'RemoteJoinField' carries the minimal GraphQL AST of a remote relationship field.
-- All such 'RemoteJoinField's of a particular remote schema are batched together
-- and made GraphQL request to remote server to fetch remote join values.
data RemoteJoinField
  = RemoteJoinField
  { _rjfRemoteSchema :: !RemoteSchemaInfo -- ^ The remote schema server info.
  , _rjfAlias        :: !G.Alias -- ^ Top level alias of the field
  , _rjfField        :: !G.Field -- ^ The field AST
  , _rjfFieldCall    :: ![G.Name] -- ^ Path to remote join value
  , _rjfVariables    :: ![(G.VariableDefinition,A.Value)] -- ^ Variables used in the AST
  } deriving (Show, Eq)

-- | Generate composite JSON ('CompositeValue') parameterised over 'RemoteJoinField'
-- from remote join map and query response JSON from Postgres.
traverseQueryResponseJSON
  :: (MonadError QErr m)
  => RemoteJoinMap -> AO.Value -> m (CompositeValue RemoteJoinField)
traverseQueryResponseJSON rjm =
  flip runReaderT rjm . flip evalStateT (Counter 0) . traverseValue mempty
  where
    askRemoteJoins :: MonadReader RemoteJoinMap m
                   => FieldPath -> m (Maybe (NE.NonEmpty RemoteJoin))
    askRemoteJoins path = asks (Map.lookup path)

    traverseValue :: (MonadError QErr m, MonadReader RemoteJoinMap m, MonadState Counter m)
                  => FieldPath -> AO.Value -> m (CompositeValue RemoteJoinField)
    traverseValue path = \case
      AO.Object obj -> traverseObject obj
      AO.Array arr -> CVObjectArray <$> mapM (traverseValue path) (toList arr)
      v            -> pure $ CVOrdValue v

      where
        mkRemoteSchemaField siblingFields remoteJoin = do
          counter <- getCounter
          let RemoteJoin fieldName inputArgs selSet hasuraFields fieldCall rsi _ = remoteJoin
              hasuraFieldVariables = map (G.Variable . G.Name . getFieldNameTxt) $ toList hasuraFields
              siblingFieldArgs = Map.fromList $
                                 map ((G.Variable . G.Name) *** ordJsonvalueToGValue) siblingFields
              hasuraFieldArgs = flip Map.filterWithKey siblingFieldArgs $ \k _ -> k `elem` hasuraFieldVariables
              fieldAlias = pathToAlias (appendPath fieldName path) counter
          queryField <- fieldCallsToField (map _rfaArgument inputArgs) hasuraFieldArgs selSet fieldAlias fieldCall
          pure $ RemoteJoinField rsi
                                 fieldAlias
                                 queryField
                                 (map fcName $ toList $ NE.tail fieldCall)
                                 (concat $ mapMaybe _rfaVariable inputArgs)
          where
            ordJsonvalueToGValue = jsonValueToGValue . AO.fromOrdered

        traverseObject obj = do
          let fields = AO.toList obj
          maybeRemoteJoins <- askRemoteJoins path
          processedFields <- fmap catMaybes $ forM fields $ \(fieldText, value) -> do
            let fieldName = FieldName fieldText
                fieldPath = appendPath fieldName path
            fmap (fieldText,) <$> case maybeRemoteJoins of
                Nothing -> Just <$> traverseValue fieldPath value
                Just nonEmptyRemoteJoins -> do
                  let remoteJoins = toList nonEmptyRemoteJoins
                      phantomColumnFields = map (fromPGCol . pgiColumn) $
                                            concatMap _rjPhantomFields remoteJoins
                  if | fieldName `elem` phantomColumnFields -> pure Nothing
                     | otherwise ->
                         case find ((== fieldName) . _rjName) remoteJoins of
                           Just rj -> Just . CVFromRemote <$> mkRemoteSchemaField fields rj
                           Nothing -> Just <$> traverseValue fieldPath value
          pure $ CVObject $ OMap.fromList processedFields

-- | Fetch remote join field value from remote servers by batching respective 'RemoteJoinField's
fetchRemoteJoinFields
  :: ( HasVersion
     , MonadError QErr m
     , MonadIO m
     , Tracing.MonadTrace m
     )
  => Env.Environment
  -> HTTP.Manager
  -> [N.Header]
  -> UserInfo
  -> [RemoteJoinField]
  -> m AO.Object
fetchRemoteJoinFields env manager reqHdrs userInfo remoteJoins = do
  results <- forM (Map.toList remoteSchemaBatch) $ \(rsi, batch) -> do
    let batchList = toList batch
        gqlReq = fieldsToRequest G.OperationTypeQuery
                                 (map _rjfField batchList)
                                 (concatMap _rjfVariables batchList)
        gqlReqUnparsed = (GQLQueryText . G.renderExecutableDoc . G.ExecutableDocument . unGQLExecDoc) <$> gqlReq
    -- NOTE: discard remote headers (for now):
    (_, _, respBody) <- execRemoteGQ' env manager userInfo reqHdrs gqlReqUnparsed rsi G.OperationTypeQuery
    case AO.eitherDecode respBody of
      Left e -> throw500 $ "Remote server response is not valid JSON: " <> T.pack e
      Right r -> do
        respObj <- either throw500 pure $ AO.asObject r
        let errors = AO.lookup "errors" respObj

        if | isNothing errors || errors == Just AO.Null ->
               case AO.lookup "data" respObj of
                 Nothing -> throw400 Unexpected "\"data\" field not found in remote response"
                 Just v  -> either throw500 pure $ AO.asObject v

           | otherwise ->
             throwError (err400 Unexpected "Errors from remote server")
             {qeInternal = Just $ A.object ["errors" A..= (AO.fromOrdered <$> errors)]}

  either (throw500 . T.pack) pure $ foldM AO.safeUnion AO.empty results
  where
    remoteSchemaBatch = Map.groupOnNE _rjfRemoteSchema remoteJoins

    fieldsToRequest :: G.OperationType -> [G.Field] -> [(G.VariableDefinition,A.Value)] -> GQLReqParsed
    fieldsToRequest opType gfields vars =
      case vars of
        [] ->
          GQLReq
            { _grOperationName = Nothing
            , _grQuery =
               GQLExecDoc
                 [ G.ExecutableDefinitionOperation
                     (G.OperationDefinitionTyped
                       ( emptyOperationDefinition
                           { G._todSelectionSet = map G.SelectionField gfields
                           }
                       )
                     )
                  ]
             , _grVariables = Nothing
             }
        vars' ->
          GQLReq
            { _grOperationName = Nothing
            , _grQuery =
                GQLExecDoc
                  [ G.ExecutableDefinitionOperation
                      (G.OperationDefinitionTyped
                       ( emptyOperationDefinition
                            { G._todSelectionSet = map G.SelectionField gfields
                            , G._todVariableDefinitions = nub (map fst vars')
                            }
                        )
                      )
                  ]
            , _grVariables = Just $ Map.fromList
                                      (map (\(varDef, val) -> (G._vdVariable varDef, val)) vars')
            }

      where
        emptyOperationDefinition =
          G.TypedOperationDefinition {
             G._todType = opType
           , G._todName = Nothing
           , G._todVariableDefinitions = []
           , G._todDirectives = []
           , G._todSelectionSet = [] }



-- | Replace 'RemoteJoinField' in composite JSON with it's json value from remote server response.
replaceRemoteFields
  :: MonadError QErr m
  => CompositeValue RemoteJoinField
  -> AO.Object
  -> m AO.Value
replaceRemoteFields compositeJson remoteServerResponse =
  compositeValueToJSON <$> traverse replaceValue compositeJson
  where
    replaceValue rj = do
      let alias = G.unAlias $ _rjfAlias rj
          fieldCall = _rjfFieldCall rj
      extractAtPath (alias:fieldCall) $ AO.Object remoteServerResponse

    -- | 'FieldCall' is path to remote relationship value in remote server response.
    -- 'extractAtPath' traverse through the path and extracts the json value
    extractAtPath path v =
      case NE.nonEmpty path of
        Nothing          -> pure v
        Just (h :| rest) -> case v of
          AO.Object o   -> maybe
                           (throw500 $ "cannnot find value in remote response at path " <> T.pack (show path))
                           (extractAtPath rest)
                           (AO.lookup (G.unName h) o)
          AO.Array arr -> AO.array <$> mapM (extractAtPath path) (toList arr)
          _            -> throw500 $ "expecting array or object in remote response at path " <> T.pack (show path)

-- | Fold nested 'FieldCall's into a bare 'Field', inserting the passed
-- selection set at the leaf of the tree we construct.
fieldCallsToField
  :: forall m. MonadError QErr m
  => [G.Argument]
  -> Map.HashMap G.Variable G.Value
  -> G.SelectionSet
  -- ^ Inserted at leaf of nested FieldCalls
  -> G.Alias
  -- ^ Top-level name to set for this Field
  -> NonEmpty FieldCall
  -> m G.Field
fieldCallsToField rrArguments variables finalSelSet topAlias =
  fmap (\f -> f{G._fAlias = Just topAlias}) . nest
  where
    -- almost: `foldr nest finalSelSet`
    nest :: NonEmpty FieldCall -> m G.Field
    nest ((FieldCall name remoteArgs) :| rest) = do
      templatedArguments <- createArguments variables remoteArgs
      (args, selSet) <- case NE.nonEmpty rest of
            Just f -> do
              s <- nest f
              pure (templatedArguments, [G.SelectionField s])
            Nothing ->
              let argsToMap = Map.fromList . map (G._aName &&& G._aValue)
                  arguments = map (uncurry G.Argument) $ Map.toList $
                              Map.unionWith mergeValue
                                (argsToMap rrArguments)
                                (argsToMap templatedArguments)
              in pure (arguments, finalSelSet)
      pure $ G.Field Nothing name args [] selSet

-- This is a kind of "deep merge".
-- For e.g. suppose the input argument of the remote field is something like:
-- `where: { id : 1}`
-- And during execution, client also gives the input arg: `where: {name: "tiru"}`
-- We need to merge the input argument to where: {id : 1, name: "tiru"}
mergeValue :: G.Value -> G.Value -> G.Value
mergeValue lVal rVal = case (lVal, rVal) of
  (G.VList (G.ListValueG l), G.VList (G.ListValueG r)) ->
    G.VList $ G.ListValueG $ l <> r
  (G.VObject (G.ObjectValueG l), G.VObject (G.ObjectValueG r)) ->
    let fieldsToMap = Map.fromList . map (G._ofName &&& G._ofValue)
    in G.VObject $ G.ObjectValueG $ map (uncurry G.ObjectFieldG) $ Map.toList $
       Map.unionWith mergeValue (fieldsToMap l) (fieldsToMap r)
  (_, _) -> error $ "can only merge a list with another list or an " <>
                    "object with another object"

-- | Create an argument map using the inputs taken from the hasura database.
createArguments
  :: (MonadError QErr m)
  => Map.HashMap G.Variable G.Value
  -> RemoteArguments
  -> m [G.Argument]
createArguments variables (RemoteArguments arguments) =
  either
    (throw400 Unexpected . \errors -> "Found errors: " <> T.intercalate ", " errors)
    (pure . map (\(G.ObjectFieldG key val) -> G.Argument key val))
    (toEither (substituteVariables variables arguments))

-- | Substitute values in the argument list.
substituteVariables
  :: HashMap G.Variable G.Value -- ^ Values to use.
  -> [G.ObjectFieldG G.Value] -- ^ A template.
  -> Validation [Text] [G.ObjectFieldG G.Value]
substituteVariables values = traverse (traverse go)
  where
    go v = case v of
      G.VVariable variable ->
        case Map.lookup variable values of
          Nothing    -> Failure ["Value for variable " <> G.unVariable variable <<> " not provided"]
          Just value -> pure value
      G.VList (G.ListValueG listValue) ->
        fmap (G.VList . G.ListValueG) (traverse go listValue)
      G.VObject (G.ObjectValueG objectValue) ->
        fmap (G.VObject . G.ObjectValueG) (traverse (traverse go) objectValue)
      _ -> pure v
