-- | Types and Functions for resolving remote join fields
module Hasura.RQL.DML.RemoteJoin
  ( executeQueryWithRemoteJoins
  , getRemoteJoins
  , getRemoteJoinsAggSel
  , getRemoteJoinsMutationOutput
  , RemoteJoins
  ) where

import           Hasura.Prelude

import           Control.Lens
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

import qualified Data.Aeson                             as A
import qualified Data.Aeson.Ordered                     as AO
import qualified Data.HashMap.Strict                    as Map
import qualified Data.HashMap.Strict.Extended           as Map
import qualified Data.HashMap.Strict.InsOrd             as OMap
import qualified Data.HashSet                           as HS
import qualified Data.List.NonEmpty                     as NE
import qualified Data.Text                              as T
import qualified Database.PG.Query                      as Q
import qualified Language.GraphQL.Draft.Printer.Text    as G
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Types                     as N

-- | Executes given query and fetch response JSON from Postgres. Substitutes remote relationship fields.
executeQueryWithRemoteJoins
  :: (HasVersion, MonadTx m, MonadIO m)
  => HTTP.Manager
  -> [N.Header]
  -> UserInfo
  -> Q.Query
  -> [Q.PrepArg]
  -> RemoteJoins
  -> m EncJSON
executeQueryWithRemoteJoins manager reqHdrs userInfo q prepArgs rjs = do
  -- Step 1: Perform the query on database and fetch the response
  pgRes <- runIdentity . Q.getRow <$> liftTx (Q.rawQE dmlTxErrorHandler q prepArgs True)
  jsonRes <- either (throw500 . T.pack) pure $ AO.eitherDecode pgRes
  -- Step 2: Traverse through the JSON obtained in above step and generate composite JSON value with remote joins
  compositeJson <- traverseQueryResponseJSON rjMap jsonRes
  let remoteJoins = collectRemoteFields compositeJson
  -- Step 3: Make queries to remote server and fetch graphql response
  remoteServerResp <- fetchRemoteJoinFields manager reqHdrs userInfo remoteJoins
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
  , _rjArgs          :: ![G.Argument] -- ^ User-provided arguments.
  , _rjSelSet        :: ![G.Field] -- ^ User-provided selection set of remote field.
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

-- | Traverse through 'AnnAggSel' and collect remote join fields (if any).
getRemoteJoinsAggSel :: AnnAggSel -> (AnnAggSel, Maybe RemoteJoins)
getRemoteJoinsAggSel =
  second mapToNonEmpty . flip runState mempty . transformAggSelect mempty

transformAggSelect :: FieldPath -> AnnAggSel -> State RemoteJoinMap AnnAggSel
transformAggSelect path sel = do
  let aggFields = _asnFields sel
  transformedFields <- forM aggFields $ \(fieldName, aggField) ->
    (fieldName,) <$> case aggField of
      TAFAgg agg         -> pure $ TAFAgg agg
      TAFNodes annFields -> TAFNodes <$> transformAnnFields (appendPath fieldName path) annFields
      TAFExp t           -> pure $ TAFExp t
  pure sel{_asnFields = transformedFields}

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

transformAnnFields :: FieldPath -> AnnFlds -> State RemoteJoinMap AnnFlds
transformAnnFields path fields = do
  let pgColumnFields = map fst $ getFields _FCol fields
      remoteSelects = getFields _FRemote fields
      remoteJoins = flip map remoteSelects $ \(fieldName, remoteSelect) ->
        let RemoteSelect argsMap selSet hasuraColumns remoteFields rsi = remoteSelect
            hasuraColumnL = toList hasuraColumns
            hasuraColumnFields = HS.fromList $ map (fromPGCol . pgiColumn) hasuraColumnL
            phantomColumns = filter ((`notElem` pgColumnFields) . fromPGCol . pgiColumn) hasuraColumnL
        in RemoteJoin fieldName argsMap selSet hasuraColumnFields remoteFields rsi phantomColumns

  transformedFields <- forM fields $ \(fieldName, field) -> do
    let fieldPath = appendPath fieldName path
    (fieldName,) <$> case field of
      FCol c -> pure $ FCol c
      FObj annRel -> FObj <$> transformAnnRel fieldPath annRel
      FArr (ASSimple annRel) -> FArr . ASSimple <$> transformAnnRel fieldPath annRel
      FArr (ASAgg aggRel) -> FArr . ASAgg <$> transformAnnAggRel fieldPath aggRel
      FComputedField computedField ->
        FComputedField <$> case computedField of
          CFSScalar _         -> pure computedField
          CFSTable jas annSel -> CFSTable jas <$> transformSelect fieldPath annSel
      FRemote rs -> pure $ FRemote rs
      FExp t     -> pure $ FExp t

  case NE.nonEmpty remoteJoins of
    Nothing -> pure transformedFields
    Just nonEmptyRemoteJoins -> do
      let phantomColumns = map (\ci -> (fromPGCol $ pgiColumn ci, FCol $ AnnColField ci False Nothing)) $
                           concatMap _rjPhantomFields remoteJoins
      modify (Map.insert path nonEmptyRemoteJoins)
      pure $ transformedFields <> phantomColumns
    where
      getFields f = mapMaybe (sequence . second (^? f))
      transformAnnRel fieldPath annRel = do
        let annSel = aarAnnSel annRel
        transformedSel <- transformSelect fieldPath annSel
        pure annRel{aarAnnSel = transformedSel}

      transformAnnAggRel fieldPath annRel = do
        let annSel = aarAnnSel annRel
        transformedSel <- transformAggSelect fieldPath annSel
        pure annRel{aarAnnSel = transformedSel}

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
-- All such 'RemoteJoinField's of a pariticular remote schema are batched together
-- and made GraphQL request to remote server to fetch remote join values.
data RemoteJoinField
  = RemoteJoinField
  { _rjfRemoteSchema :: !RemoteSchemaInfo -- ^ The remote schema server info.
  , _rjfAlias        :: !G.Alias -- ^ Top level alias of the field
  , _rjfField        :: !G.Field -- ^ The field AST
  , _rjfFieldCall    :: ![G.Name] -- ^ Path to remote join value
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
          queryField <- fieldCallsToField inputArgs hasuraFieldArgs selSet fieldAlias fieldCall
          pure $ RemoteJoinField rsi fieldAlias queryField $ map fcName $ toList $ NE.tail fieldCall
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
     )
  => HTTP.Manager
  -> [N.Header]
  -> UserInfo
  -> [RemoteJoinField]
  -> m AO.Object
fetchRemoteJoinFields manager reqHdrs userInfo remoteJoins = do
  results <- forM (Map.toList remoteSchemaBatch) $ \(rsi, batch) -> do
    let gqlReq = fieldsToRequest G.OperationTypeQuery $ map _rjfField $ toList batch
        gqlReqUnparsed = (GQLQueryText . G.renderExecutableDoc . G.ExecutableDocument . unGQLExecDoc) <$> gqlReq
    -- NOTE: discard remote headers (for now):
    (_, _, respBody) <- execRemoteGQ' manager userInfo reqHdrs gqlReqUnparsed rsi G.OperationTypeQuery
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
  :: MonadError QErr m
  => [G.Argument]
  -> Map.HashMap G.Variable G.Value
  -> [G.Field]
  -- ^ Inserted at leaf of nested FieldCalls
  -> G.Alias
  -- ^ Top-level name to set for this Field
  -> NonEmpty FieldCall
  -> m G.Field
fieldCallsToField rrArguments variables finalSelSet topAlias =
  fmap (\f -> f{G._fAlias = Just topAlias}) . nest
  where
    -- almost: `foldr nest finalSelSet`
    nest ((FieldCall name remoteArgs) :| rest) = do
      templatedArguments <- createArguments variables remoteArgs
      (args, selSet) <- case NE.nonEmpty rest of
            Just f -> do
              s <- nest f
              pure (templatedArguments, pure s)
            Nothing ->
              let argsToMap = Map.fromList . map (G._aName &&& G._aValue)
                  arguments = map (uncurry G.Argument) $ Map.toList $
                              Map.unionWith mergeValue
                                (argsToMap rrArguments)
                                (argsToMap templatedArguments)
              in pure (arguments, finalSelSet)
      pure $ G.Field Nothing name args [] $ map G.SelectionField selSet

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
  (l, _) -> l -- FIXME:- throw error for merging non-lists and non-objects

fieldsToRequest :: G.OperationType -> [G.Field] -> GQLReqParsed
fieldsToRequest opType gfields =
  GQLReq
     { _grOperationName = Nothing
     , _grQuery =
         GQLExecDoc
           [ G.ExecutableDefinitionOperation
               (G.OperationDefinitionTyped
                 ( emptyOperationDefinition
                     {G._todSelectionSet = map G.SelectionField gfields}
                 )
               )
           ]
     , _grVariables = Nothing -- TODO: Put variables in here?
     }
    where
      emptyOperationDefinition =
        G.TypedOperationDefinition {
          G._todType = opType
        , G._todName = Nothing
        , G._todVariableDefinitions = []
        , G._todDirectives = []
        , G._todSelectionSet = [] }

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
