module Hasura.Backends.Postgres.Execute.RemoteJoin
  ( getRemoteJoins
  , getRemoteJoinsAggregateSelect
  , getRemoteJoinsMutationOutput
  , getRemoteJoinsConnectionSelect
  , RemoteJoins
  -- * These are required in pro:
  , FieldPath(..)
  , RemoteJoin(..)
  , executeQueryWithRemoteJoins
  , processRemoteJoins
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                             as A
import qualified Data.Aeson.Ordered                     as AO
import qualified Data.ByteString.Lazy                   as BL
import qualified Data.Environment                       as Env
import qualified Data.HashMap.Strict                    as Map
import qualified Data.HashMap.Strict.Extended           as Map
import qualified Data.HashMap.Strict.InsOrd             as OMap
import qualified Data.HashSet                           as HS
import qualified Data.List.NonEmpty                     as NE
import qualified Data.Text                              as T
import qualified Database.PG.Query                      as Q
import qualified Language.GraphQL.Draft.Printer         as G
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Types                     as N

import           Control.Lens
import           Data.Text.Extended                     (commaSeparated, (<<>))
import           Data.Validation

import qualified Hasura.Backends.Postgres.SQL.DML       as S
import qualified Hasura.Tracing                         as Tracing

import           Hasura.Backends.Postgres.Connection
import           Hasura.EncJSON
import           Hasura.GraphQL.Parser                  hiding (field)
import           Hasura.GraphQL.RemoteServer            (execRemoteGQ')
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.IR.RemoteJoin
import           Hasura.RQL.IR.Returning
import           Hasura.RQL.IR.Select
import           Hasura.RQL.Types
import           Hasura.Server.Version                  (HasVersion)
import           Hasura.Session


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
  -> RemoteJoins 'Postgres
  -> m EncJSON
executeQueryWithRemoteJoins env manager reqHdrs userInfo q prepArgs rjs = do
  -- Perform the query on database and fetch the response
  pgRes <- runIdentity . Q.getRow <$> Tracing.trace "Postgres" (liftTx (Q.rawQE dmlTxErrorHandler q prepArgs True))
  -- Process remote joins in the response
  processRemoteJoins env manager reqHdrs userInfo pgRes rjs

processRemoteJoins
  :: ( HasVersion
     , MonadTx m
     , MonadIO m
     , Tracing.MonadTrace m
     )
  => Env.Environment
  -> HTTP.Manager
  -> [N.Header]
  -> UserInfo
  -> BL.ByteString
  -> RemoteJoins 'Postgres
  -> m EncJSON
processRemoteJoins env manager reqHdrs userInfo pgRes rjs = do
  -- Step 1: Decode the given bytestring as a JSON value
  jsonRes <- onLeft (AO.eitherDecode pgRes) (throw500 . T.pack)
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

type Alias = G.Name

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

parseGraphQLName :: (MonadError QErr m) => Text -> m G.Name
parseGraphQLName txt = onNothing (G.mkName txt) (throw400 RemoteSchemaError $ errMsg)
  where
    errMsg = txt <> " is not a valid GraphQL name"

-- | Generate the alias for remote field.
pathToAlias :: (MonadError QErr m) => FieldPath -> Counter -> m Alias
pathToAlias path counter = do
  parseGraphQLName $ T.intercalate "_" (map getFieldNameTxt $ unFieldPath path)
                 <> "__" <> (T.pack . show . unCounter) counter

type RemoteJoins b = NE.NonEmpty (FieldPath, NE.NonEmpty (RemoteJoin b))
type RemoteJoinMap b = Map.HashMap FieldPath (NE.NonEmpty (RemoteJoin b))

mapToNonEmpty :: RemoteJoinMap backend -> Maybe (RemoteJoins backend)
mapToNonEmpty = NE.nonEmpty . Map.toList

-- | Traverse through 'AnnSimpleSel' and collect remote join fields (if any).
getRemoteJoins :: AnnSimpleSel 'Postgres -> (AnnSimpleSel 'Postgres, Maybe (RemoteJoins 'Postgres))
getRemoteJoins =
  second mapToNonEmpty . flip runState mempty . transformSelect mempty

transformSelect :: FieldPath -> AnnSimpleSel 'Postgres -> State (RemoteJoinMap 'Postgres) (AnnSimpleSel 'Postgres)
transformSelect path sel = do
  let fields = _asnFields sel
  -- Transform selects in array, object and computed fields
  transformedFields <- transformAnnFields path fields
  pure sel{_asnFields = transformedFields}

transformObjectSelect :: FieldPath -> AnnObjectSelect 'Postgres -> State (RemoteJoinMap 'Postgres) (AnnObjectSelect 'Postgres)
transformObjectSelect path sel = do
  let fields = _aosFields sel
  transformedFields <- transformAnnFields path fields
  pure sel{_aosFields = transformedFields}

-- | Traverse through @'AnnAggregateSelect' and collect remote join fields (if any).
getRemoteJoinsAggregateSelect :: AnnAggregateSelect 'Postgres -> (AnnAggregateSelect 'Postgres, Maybe (RemoteJoins 'Postgres))
getRemoteJoinsAggregateSelect =
  second mapToNonEmpty . flip runState mempty . transformAggregateSelect mempty

transformAggregateSelect
  :: FieldPath
  -> AnnAggregateSelect 'Postgres
  -> State (RemoteJoinMap 'Postgres) (AnnAggregateSelect 'Postgres)
transformAggregateSelect path sel = do
  let aggFields = _asnFields sel
  transformedFields <- forM aggFields $ \(fieldName, aggField) ->
    (fieldName,) <$> case aggField of
      TAFAgg agg         -> pure $ TAFAgg agg
      TAFNodes annFields -> TAFNodes <$> transformAnnFields (appendPath fieldName path) annFields
      TAFExp t           -> pure $ TAFExp t
  pure sel{_asnFields = transformedFields}

-- | Traverse through @'ConnectionSelect' and collect remote join fields (if any).
getRemoteJoinsConnectionSelect :: ConnectionSelect 'Postgres S.SQLExp -> (ConnectionSelect 'Postgres S.SQLExp, Maybe (RemoteJoins 'Postgres))
getRemoteJoinsConnectionSelect =
  second mapToNonEmpty . flip runState mempty . transformConnectionSelect mempty

transformConnectionSelect
  :: FieldPath
  -> ConnectionSelect 'Postgres S.SQLExp
  -> State (RemoteJoinMap 'Postgres) (ConnectionSelect 'Postgres S.SQLExp)
transformConnectionSelect path ConnectionSelect{..} = do
  let connectionFields = _asnFields _csSelect
  transformedFields <- forM connectionFields $ \(fieldName, field) ->
    (fieldName,) <$> case field of
      ConnectionTypename t  -> pure $ ConnectionTypename t
      ConnectionPageInfo p  -> pure $ ConnectionPageInfo p
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
getRemoteJoinsMutationOutput
  :: MutationOutput 'Postgres
  -> (MutationOutput 'Postgres, Maybe (RemoteJoins 'Postgres))
getRemoteJoinsMutationOutput =
  second mapToNonEmpty . flip runState mempty . transformMutationOutput mempty
  where
    transformMutationOutput :: FieldPath -> MutationOutput 'Postgres -> State (RemoteJoinMap 'Postgres) (MutationOutput 'Postgres)
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

transformAnnFields :: FieldPath -> AnnFields 'Postgres -> State (RemoteJoinMap 'Postgres) (AnnFields 'Postgres)
transformAnnFields path fields = do
  let pgColumnFields = map fst $ getFields _AFColumn fields
      remoteSelects = getFields _AFRemote fields
      remoteJoins = flip map remoteSelects $ \(fieldName, remoteSelect) ->
        let RemoteSelect argsMap selSet hasuraColumns remoteFields rsi = remoteSelect
            hasuraColumnL = toList hasuraColumns
            hasuraColumnFields = HS.fromList $ map (fromPGCol . pgiColumn) hasuraColumnL
            phantomColumns = filter ((`notElem` pgColumnFields) . fromPGCol . pgiColumn) hasuraColumnL
        in RemoteJoin fieldName argsMap selSet hasuraColumnFields remoteFields rsi phantomColumns

  transformedFields <- forM fields $ \(fieldName, field') -> do
    let fieldPath = appendPath fieldName path
    (fieldName,) <$> case field' of
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
  CVOrdValue v       -> v
  CVObject obj       -> AO.object $ OMap.toList $ OMap.map compositeValueToJSON obj
  CVObjectArray vals -> AO.array $ map compositeValueToJSON vals
  CVFromRemote v     -> v

-- | A 'RemoteJoinField' carries the minimal GraphQL AST of a remote relationship field.
-- All such 'RemoteJoinField's of a particular remote schema are batched together
-- and made GraphQL request to remote server to fetch remote join values.
data RemoteJoinField
  = RemoteJoinField
  { _rjfRemoteSchema :: !RemoteSchemaInfo -- ^ The remote schema server info.
  , _rjfAlias        :: !Alias -- ^ Top level alias of the field
  , _rjfField        :: !(G.Field G.NoFragments Variable) -- ^ The field AST
  , _rjfFieldCall    :: ![G.Name] -- ^ Path to remote join value
  } deriving (Show, Eq)

-- | Generate composite JSON ('CompositeValue') parameterised over 'RemoteJoinField'
--   from remote join map and query response JSON from Postgres.
traverseQueryResponseJSON
  :: (MonadError QErr m)
  => RemoteJoinMap 'Postgres -> AO.Value -> m (CompositeValue RemoteJoinField)
traverseQueryResponseJSON rjm =
  flip runReaderT rjm . flip evalStateT (Counter 0) . traverseValue mempty
  where
    askRemoteJoins :: MonadReader (RemoteJoinMap 'Postgres) m
                   => FieldPath -> m (Maybe (NE.NonEmpty (RemoteJoin 'Postgres)))
    askRemoteJoins path = asks (Map.lookup path)

    traverseValue :: (MonadError QErr m, MonadReader (RemoteJoinMap 'Postgres) m, MonadState Counter m)
                  => FieldPath -> AO.Value -> m (CompositeValue RemoteJoinField)
    traverseValue path = \case
      AO.Object obj -> traverseObject obj
      AO.Array arr  -> CVObjectArray <$> mapM (traverseValue path) (toList arr)
      v             -> pure $ CVOrdValue v

      where
        mkRemoteSchemaField siblingFields remoteJoin = do
          counter <- getCounter
          let RemoteJoin fieldName inputArgs selSet hasuraFields fieldCall rsi _ = remoteJoin
          hasuraFieldVariables <- mapM (parseGraphQLName . getFieldNameTxt) $ toList hasuraFields
          siblingFieldArgsVars <- mapM (\(k,val) -> do
                                          (,) <$> parseGraphQLName k <*> ordJSONValueToGValue val)
                                  $ siblingFields
          let siblingFieldArgs = Map.fromList $ siblingFieldArgsVars
              hasuraFieldArgs = flip Map.filterWithKey siblingFieldArgs $ \k _ -> k `elem` hasuraFieldVariables
          fieldAlias <- pathToAlias (appendPath fieldName path) counter
          queryField <- fieldCallsToField (inputArgsToMap inputArgs) hasuraFieldArgs selSet fieldAlias fieldCall
          pure $ RemoteJoinField rsi
                                 fieldAlias
                                 queryField
                                 (map fcName $ toList $ NE.tail fieldCall)
          where
            ordJSONValueToGValue :: (MonadError QErr m) => AO.Value -> m (G.Value Void)
            ordJSONValueToGValue =
              either (throw400 ValidationFailed) pure . jsonToGraphQL . AO.fromOrdered

            inputArgsToMap = Map.fromList . map (_rfaArgument &&& _rfaValue)

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

convertFieldWithVariablesToName :: G.Field G.NoFragments Variable -> G.Field G.NoFragments G.Name
convertFieldWithVariablesToName = fmap getName

inputValueToJSON :: InputValue Void -> A.Value
inputValueToJSON = \case
  JSONValue    j -> j
  GraphQLValue g -> graphQLValueToJSON g
  where
    graphQLValueToJSON :: G.Value Void -> A.Value
    graphQLValueToJSON = \case
      G.VNull                 -> A.Null
      G.VInt i                -> A.toJSON i
      G.VFloat f              -> A.toJSON f
      G.VString t             -> A.toJSON t
      G.VBoolean b            -> A.toJSON b
      G.VEnum (G.EnumValue n) -> A.toJSON n
      G.VList values          -> A.toJSON $ graphQLValueToJSON <$> values
      G.VObject objects       -> A.toJSON $ graphQLValueToJSON <$> objects

defaultValue :: InputValue Void -> Maybe (G.Value Void)
defaultValue = \case
  JSONValue    _ -> Nothing
  GraphQLValue g -> Just g

collectVariables :: G.Value Variable -> HashMap G.VariableDefinition A.Value
collectVariables = \case
  G.VNull          -> mempty
  G.VInt _         -> mempty
  G.VFloat _       -> mempty
  G.VString _      -> mempty
  G.VBoolean _     -> mempty
  G.VEnum _        -> mempty
  G.VList values   -> foldl Map.union mempty $ map collectVariables values
  G.VObject values -> foldl Map.union mempty $ map collectVariables $ Map.elems values
  G.VVariable var@(Variable _ gType val) ->
    let name       = getName var
        jsonVal    = inputValueToJSON val
        defaultVal = defaultValue val
    in Map.singleton (G.VariableDefinition name gType defaultVal) jsonVal

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
    let gqlReq = fieldsToRequest G.OperationTypeQuery $ _rjfField <$> batch
        gqlReqUnparsed = GQLQueryText . G.renderExecutableDoc . G.ExecutableDocument . unGQLExecDoc <$> gqlReq
    -- NOTE: discard remote headers (for now):
    (_, _, respBody) <- execRemoteGQ' env manager userInfo reqHdrs gqlReqUnparsed rsi G.OperationTypeQuery
    case AO.eitherDecode respBody of
      Left e -> throw500 $ "Remote server response is not valid JSON: " <> T.pack e
      Right r -> do
        respObj <- onLeft (AO.asObject r) throw500
        let errors = AO.lookup "errors" respObj
        if | isNothing errors || errors == Just AO.Null ->
               case AO.lookup "data" respObj of
                 Nothing -> throw400 Unexpected "\"data\" field not found in remote response"
                 Just v  -> onLeft (AO.asObject v) throw500

           | otherwise ->
             throwError (err400 Unexpected "Errors from remote server")
             {qeInternal = Just $ A.object ["errors" A..= (AO.fromOrdered <$> errors)]}
  onLeft (foldM AO.safeUnion AO.empty results) (throw500 . T.pack)
  where
    remoteSchemaBatch = Map.groupOnNE _rjfRemoteSchema remoteJoins

    fieldsToRequest :: G.OperationType -> NonEmpty (G.Field G.NoFragments Variable) -> GQLReqParsed
    fieldsToRequest opType gFields@(headField :| _) =
      let variableInfos =
            -- only the `headField` is used for collecting the variables here because
            -- the variable information of all the fields will be the same.
            -- For example:
            -- {
            --   author {
            --     name
            --     remote_relationship (extra_arg: $extra_arg)
            --   }
            -- }
            --
            -- If there are 10 authors, then there are 10 fields that will be requested
            -- each containing exactly the same variable info.
            foldl Map.union mempty $ Map.elems $ fmap collectVariables $ G._fArguments $ headField
          gFields' = NE.toList $ NE.map (G.fmapFieldFragment G.inline . convertFieldWithVariablesToName) gFields
      in mkGQLRequest gFields' variableInfos
      where
        emptyOperationDefinition =
          G.TypedOperationDefinition {
             G._todType = opType
           , G._todName = Nothing
           , G._todVariableDefinitions = []
           , G._todDirectives = []
           , G._todSelectionSet = []
           }

        mkGQLRequest fields variableInfos =
          let variableValues =
                if (Map.null variableInfos)
                then Nothing
                else Just $ Map.fromList (map (\(varDef, val) -> (G._vdName varDef, val)) $ Map.toList variableInfos)
          in
          GQLReq
            { _grOperationName = Nothing
            , _grQuery =
               GQLExecDoc
                 [ G.ExecutableDefinitionOperation
                     (G.OperationDefinitionTyped
                       ( emptyOperationDefinition
                           { G._todSelectionSet        = map G.SelectionField fields
                           , G._todVariableDefinitions = map fst $ Map.toList variableInfos
                           }
                       )
                     )
                  ]
             , _grVariables = variableValues
            }

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
      let alias = _rjfAlias rj
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
  => Map.HashMap G.Name (InputValue Variable)
  -- ^ user input arguments to the remote join field
  -> Map.HashMap G.Name (G.Value Void)
  -- ^ Contains the values of the variables that have been defined in the remote join definition
  -> G.SelectionSet G.NoFragments Variable
  -- ^ Inserted at leaf of nested FieldCalls
  -> Alias
  -- ^ Top-level name to set for this Field
  -> NonEmpty FieldCall
  -> m (G.Field G.NoFragments Variable)
fieldCallsToField rrArguments variables finalSelSet topAlias =
  fmap (\f -> f{G._fAlias = Just topAlias}) . nest
  where
    -- almost: `foldr nest finalSelSet`
    nest :: NonEmpty FieldCall -> m (G.Field G.NoFragments Variable)
    nest ((FieldCall name remoteArgs) :| rest) = do
      templatedArguments <- convert <$> createArguments variables remoteArgs
      graphQLarguments <- traverse peel rrArguments
      (args, selSet) <- case NE.nonEmpty rest of
            Just f -> do
              s <- nest f
              pure (templatedArguments, [G.SelectionField s])
            Nothing ->
              let arguments = Map.unionWith mergeValue
                                graphQLarguments
                                -- converting (G.Value Void) -> (G.Value Variable) to merge the
                                -- 'rrArguments' with the 'variables'
                                templatedArguments
              in pure (arguments, finalSelSet)
      pure $ G.Field Nothing name args [] selSet

    convert :: Map.HashMap G.Name (G.Value Void) -> Map.HashMap G.Name (G.Value Variable)
    convert = fmap G.literal

    peel :: InputValue Variable -> m (G.Value Variable)
    peel = \case
      GraphQLValue v -> pure v
      JSONValue _ ->
        -- At this point, it is theoretically impossible that we have
        -- unpacked a variable into a JSONValue, as there's no "outer
        -- scope" at which this value could have been peeled.
        -- FIXME: check that this is correct!
        throw500 "internal error: encountered an already expanded variable when folding remote field arguments"
        -- FIXME: better error message

-- This is a kind of "deep merge".
-- For e.g. suppose the input argument of the remote field is something like:
-- `where: { id : 1}`
-- And during execution, client also gives the input arg: `where: {name: "tiru"}`
-- We need to merge the input argument to where: {id : 1, name: "tiru"}
mergeValue :: G.Value Variable -> G.Value Variable -> G.Value Variable
mergeValue lVal rVal = case (lVal, rVal) of
  (G.VList l, G.VList r) ->
    G.VList $ l <> r
  (G.VObject l, G.VObject r) ->
    G.VObject $ Map.unionWith mergeValue l r
  (_, _) -> error $ "can only merge a list with another list or an " <>
                    "object with another object"

-- | Create an argument map using the inputs taken from the hasura database.
createArguments
  :: (MonadError QErr m)
  => Map.HashMap G.Name (G.Value Void)
  -> RemoteArguments
  -> m (HashMap G.Name (G.Value Void))
createArguments variables (RemoteArguments arguments) =
  onLeft
    (toEither (substituteVariables variables arguments))
    (throw400 Unexpected . \errors -> "Found errors: " <> commaSeparated errors)

-- | Substitute values in the argument list.
substituteVariables
  :: HashMap G.Name (G.Value Void) -- ^ Values of the variables to substitute.
  -> HashMap G.Name (G.Value G.Name) -- ^ Template which contains the variables.
  -> Validation [Text] (HashMap G.Name (G.Value Void))
substituteVariables values = traverse go
  where
    go = \case
      G.VVariable variableName ->
        case Map.lookup variableName values of
          Nothing    -> Failure ["Value for variable " <> variableName <<> " not provided"]
          Just value -> pure value
      G.VList listValue ->
        fmap G.VList (traverse go listValue)
      G.VObject objectValue ->
        fmap G.VObject (traverse go objectValue)
      G.VInt i -> pure $ G.VInt i
      G.VFloat d -> pure $ G.VFloat d
      G.VString txt -> pure $ G.VString txt
      G.VEnum e -> pure $ G.VEnum e
      G.VBoolean b -> pure $ G.VBoolean b
      G.VNull -> pure $ G.VNull
