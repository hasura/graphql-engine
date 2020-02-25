-- | Types and Functions for resolving remote join fields
module Hasura.GraphQL.Resolve.RemoteJoin
  ( selectWithRemoteJoins
  , getRemoteJoins
  , getRemoteJoinsAggSel
  , RemoteJoinMap
  ) where

import           Hasura.Prelude

import           Control.Lens
import           Data.Scientific
import           Data.Time
import           Data.Validation

import           Hasura.EncJSON
import           Hasura.GraphQL.RemoteServer            (execRemoteGQ')
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.GraphQL.Validate.Field
import           Hasura.GraphQL.Validate.Types
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.DML.Select.Types
import           Hasura.RQL.Types
import           Hasura.Server.Version                  (HasVersion)
import           Hasura.SQL.Time
import           Hasura.SQL.Types
import           Hasura.SQL.Value

import qualified Data.Aeson                             as A
import qualified Data.Aeson.Ordered                     as AO
import qualified Data.HashMap.Strict                    as Map
import qualified Data.HashMap.Strict.Extended           as Map
import qualified Data.HashMap.Strict.InsOrd             as OMap
import qualified Data.List.NonEmpty                     as NE
import qualified Data.Text                              as T
import qualified Data.UUID                              as UUID
import qualified Database.PG.Query                      as Q
import qualified Language.GraphQL.Draft.Printer.Text    as G
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Types                     as N

selectWithRemoteJoins
  :: (HasVersion, MonadTx m)
  => HTTP.Manager
  -> [N.Header]
  -> UserInfo
  -> Q.Query
  -> [Q.PrepArg]
  -> RemoteJoinMap
  -> m EncJSON
selectWithRemoteJoins manager reqHdrs userInfo q prepArgs rjMap = liftTx $ do
  -- Step 1: Perform the query on Database and fetch the response
  pgRes <- runIdentity . Q.getRow <$> Q.rawQE dmlTxErrorHandler q prepArgs True
  jsonRes <- either (throw500 . T.pack) pure $ AO.eitherDecode pgRes
  -- Step 2: Traverse through the JSON obtained in above step and generate composite JSON value with remote fields
  compositeJson <- traverseQueryResponseJSON rjMap jsonRes
  let remoteJoins = collectRemoteFields compositeJson
  -- Step 3: Make queries to remote server and fetch remote fields response
  remoteServerResp <- fetchRemoteJoinFields manager reqHdrs userInfo remoteJoins
  -- Step 4: Replace remote fields responses in composite json.
  finalJson <- replaceRemoteFields compositeJson remoteServerResp
  pure $ AO.toEncJSON finalJson

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

data RemoteJoin
  = RemoteJoin
  { _rjName           :: !FieldName -- ^ The remote join field name.
  , _rjArgs           :: !ArgsMap -- ^ User-provided arguments.
  , _rjSelSet         :: !SelSet -- ^ Selection set of remote field.
  , _rjRelationship   :: !RemoteRelationship -- ^ Details of remote join.
  , _rjRemoteSchema   :: !RemoteSchemaInfo -- ^ The remote schema server info.
  , _rjPhantomColumns :: ![FieldName]
    -- ^ Hasura fields which are not in the selection set, but are required as
    -- parameters to satisfy the remote join.
  } deriving (Show, Eq)

type RemoteJoinMap = Map.HashMap FieldPath (NE.NonEmpty RemoteJoin)

-- | Traverse through 'AnnSimpleSel' and collect remote join fields (if any).
getRemoteJoins :: AnnSimpleSel -> (AnnSimpleSel, RemoteJoinMap)
getRemoteJoins =
  flip runState mempty . transformSelect mempty

transformSelect :: FieldPath -> AnnSimpleSel -> State (RemoteJoinMap) AnnSimpleSel
transformSelect path sel = do
  let fields = _asnFields sel
  -- Transform selects in array, object and computed fields
  transformedFields <- transformAnnFields path fields
  pure sel{_asnFields = transformedFields}

-- | Traverse through 'AnnAggSel' and collect remote join fields (if any).
getRemoteJoinsAggSel :: AnnAggSel -> (AnnAggSel, RemoteJoinMap)
getRemoteJoinsAggSel =
  flip runState mempty . transformAggSelect mempty

transformAggSelect :: FieldPath -> AnnAggSel -> State (RemoteJoinMap) AnnAggSel
transformAggSelect path sel = do
  let aggFields = _asnFields sel
  transformedFields <- forM aggFields $ \(fieldName, aggField) ->
    (fieldName,) <$> case aggField of
      TAFAgg agg         -> pure $ TAFAgg agg
      TAFNodes annFields -> TAFNodes <$> transformAnnFields (appendPath fieldName path) annFields
      TAFExp t           -> pure $ TAFExp t
  pure sel{_asnFields = transformedFields}

transformAnnFields :: FieldPath -> AnnFlds -> State (RemoteJoinMap) AnnFlds
transformAnnFields path fields = do
  let pgColumnFields = map fst $ getFields _FCol fields
      remoteSelects = getFields _FRemote fields
      remoteJoins = flip map remoteSelects $ \(fieldName, remoteSelect) ->
        let RemoteSelect argsMap selSet relationship rsi = remoteSelect
            -- TODO:- Have PGColumnInfo for Hasura fields
            hasuraFields = toList $ rtrHasuraFields relationship
            phantomColumns = filter (`notElem` pgColumnFields) hasuraFields
        in RemoteJoin fieldName argsMap selSet relationship rsi phantomColumns

  transformedFields <- forM fields $ \(fieldName, field) -> do
    let fieldPath = appendPath fieldName path
    (fieldName,) <$> case field of
      FCol c -> pure $ FCol c
      FObj annRel -> FObj <$> transformAnnRel fieldPath annRel
      FArr (ASSimple annRel) -> (FArr . ASSimple) <$> transformAnnRel fieldPath annRel
      FArr (ASAgg aggRel) -> (FArr . ASAgg) <$> transformAnnAggRel fieldPath aggRel
      FComputedField computedField ->
        FComputedField <$> case computedField of
          CFSScalar _         -> pure computedField
          CFSTable jas annSel -> CFSTable jas <$> transformSelect fieldPath annSel
      FRemote rs -> pure $ FRemote rs
      FExp t     -> pure $ FExp t

  case NE.nonEmpty remoteJoins of
    Nothing -> pure transformedFields
    Just nonEmptyRemoteJoins -> do
      let phantomColumnFields = map fakePGColumnField $
                               concatMap _rjPhantomColumns remoteJoins
      modify (Map.insert path nonEmptyRemoteJoins)
      pure $ transformedFields <> phantomColumnFields
    where
      getFields f = mapMaybe (sequence . (second (^? f)))
      transformAnnRel fieldPath annRel = do
        let annSel = aarAnnSel annRel
        transformedSel <- transformSelect fieldPath annSel
        pure $ annRel{aarAnnSel = transformedSel}

      transformAnnAggRel fieldPath annRel = do
        let annSel = aarAnnSel annRel
        transformedSel <- transformAggSelect fieldPath annSel
        pure $ annRel{aarAnnSel = transformedSel}

      -- FIXME:- this is terrible
      fakePGColumnField :: FieldName -> (FieldName, AnnFld)
      fakePGColumnField f =
        let info = PGColumnInfo (unsafePGCol $ getFieldNameTxt f)
                   (G.Name $ getFieldNameTxt f)
                   0 (PGColumnScalar $ PGUnknown "unknown") False Nothing
        in (f,  FCol $ AnnColField info False Nothing)


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

data RemoteJoinField
  = RemoteJoinField
  { _rjfRemoteSchema :: !RemoteSchemaInfo
  , _rjfField        :: !Field
  , _rjfFieldCall    :: ![G.Name]
  } deriving (Show, Eq)

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
          let RemoteJoin fieldName inputArgs selSet relationship rsi _ = remoteJoin
              hasuraFields = map (G.Variable . G.Name . getFieldNameTxt) $
                             toList $ rtrHasuraFields relationship
              fieldCall = rtrRemoteFields relationship
              siblingFieldArgs = Map.fromList $
                                 map ((G.Variable . G.Name) *** valueToValueConst) siblingFields
              hasuraFieldArgs = flip Map.filterWithKey siblingFieldArgs $ \k _ -> k `elem` hasuraFields
              fieldAlias = pathToAlias (appendPath fieldName path) counter
          queryField <- fieldCallsToField inputArgs hasuraFieldArgs selSet fieldAlias fieldCall
          pure $ RemoteJoinField rsi queryField $ map fcName $ toList $ NE.tail fieldCall

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
                      phantomColumns = concatMap _rjPhantomColumns remoteJoins
                  if | fieldName `elem` phantomColumns -> pure Nothing
                     | otherwise ->
                         case find ((== fieldName) . _rjName) remoteJoins of
                           Just rj -> (Just . CVFromRemote) <$> mkRemoteSchemaField fields rj
                           Nothing -> Just <$> traverseValue fieldPath value
          pure $ CVObject $ OMap.fromList processedFields

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
    gqlReq <- fieldsToRequest G.OperationTypeQuery $ map _rjfField $ toList batch
    let gqlReqUnparsed = (GQLQueryText . G.renderExecutableDoc . G.ExecutableDocument . unGQLExecDoc) <$> gqlReq
    -- NOTE: discard remote headers (for now):
    (_, _, respBody) <- execRemoteGQ' manager userInfo reqHdrs gqlReqUnparsed rsi G.OperationTypeQuery
    case AO.eitherDecode respBody of
      Left e -> throw500 $ "Remote server response is not valid JSON: " <> T.pack e
      Right r -> do
        respObj <- either throw500 pure $ AO.asObject r
        let errors = AO.lookup "errors" respObj

        if | errors == Nothing || errors == Just AO.Null ->
               case AO.lookup "data" respObj of
                 Nothing -> throw400 Unexpected $ "\"data\" field not found in remote response"
                 Just v  -> either throw500 pure $ AO.asObject v

           | otherwise ->
             throwError (err400 Unexpected "Errors from remote server")
             {qeInternal = Just $ A.object ["errors" A..= (AO.fromOrdered <$> errors)]}


  either (throw500 . T.pack) pure $ foldM AO.safeUnion AO.empty results
  where
    remoteSchemaBatch = Map.groupOnNE _rjfRemoteSchema remoteJoins

replaceRemoteFields
  :: MonadError QErr m
  => CompositeValue RemoteJoinField -> AO.Object -> m AO.Value
replaceRemoteFields compositeJson remoteServerResponse =
  compositeValueToJSON <$> traverse replaceValue compositeJson
  where
    replaceValue rj = do
      let alias = G.unAlias $ _fAlias $ _rjfField rj
          fieldCall = _rjfFieldCall rj
      extractAtPath (alias:fieldCall) $ AO.Object remoteServerResponse

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

-- | Convert a JSON value to a GraphQL value.
valueToValueConst :: AO.Value -> G.ValueConst
valueToValueConst =
  \case
    AO.Array xs -> G.VCList (G.ListValueG (fmap valueToValueConst (toList xs)))
    AO.String str -> G.VCString (G.StringValue str)
    -- TODO: Note the danger zone of scientific:
    AO.Number sci -> either G.VCFloat G.VCInt (floatingOrInteger sci)
    AO.Null -> G.VCNull
    AO.Bool b -> G.VCBoolean b
    AO.Object hashmap ->
      G.VCObject
        (G.ObjectValueG
           (map
              (\(key, value) ->
                 G.ObjectFieldG (G.Name key) (valueToValueConst value))
              (AO.toList hashmap)))

-- | Fold nested 'FieldCall's into a bare 'Field', inserting the passed
-- selection set at the leaf of the tree we construct.
fieldCallsToField
  :: MonadError QErr m
  => ArgsMap
  -> Map.HashMap G.Variable G.ValueConst
  -> SelSet
  -- ^ Inserted at leaf of nested FieldCalls
  -> G.Alias
  -- ^ Top-level name to set for this Field
  -> NonEmpty FieldCall
  -> m Field
fieldCallsToField rrArguments variables finalSelSet topAlias =
  fmap (set fAlias topAlias) . nest
  where
    -- almost: `foldr nest finalSelSet`
    nest (FieldCall{..} :| rest) = do
      templatedArguments <- createArguments variables fcArguments
      (args, selSet) <- case NE.nonEmpty rest of
            Just f -> do
              s <- nest f
              pure $ (templatedArguments, pure s)
            Nothing ->
              let arguments = Map.unionWith mergeAnnInpVal rrArguments templatedArguments
              in pure (arguments, finalSelSet)
      pure $ (bareNamedField fcName)
             { _fArguments = args
             , _fSelSet = selSet
             }

-- This is a kind of "deep merge".
-- For e.g. suppose the input argument of the remote field is something like:
-- `where: { id : 1}`
-- And during execution, client also gives the input arg: `where: {name: "tiru"}`
-- We need to merge the input argument to where: {id : 1, name: "tiru"}
mergeAnnInpVal :: AnnInpVal -> AnnInpVal -> AnnInpVal
mergeAnnInpVal an1 an2 =
  an1 {_aivValue = mergeAnnGValue (_aivValue an1) (_aivValue an2)}

mergeAnnGValue :: AnnGValue -> AnnGValue -> AnnGValue
mergeAnnGValue (AGObject n1 (Just o1)) (AGObject _ (Just o2)) =
  (AGObject n1 (Just (mergeAnnGObject o1 o2)))
mergeAnnGValue (AGObject n1 (Just o1)) (AGObject _ Nothing) =
  (AGObject n1 (Just o1))
mergeAnnGValue (AGObject n1 Nothing) (AGObject _ (Just o1)) =
  (AGObject n1 (Just o1))
mergeAnnGValue (AGArray t (Just xs)) (AGArray _ (Just xs2)) =
  AGArray t (Just (xs <> xs2))
mergeAnnGValue (AGArray t (Just xs)) (AGArray _ Nothing) =
  AGArray t (Just xs)
mergeAnnGValue (AGArray t Nothing) (AGArray _ (Just xs)) =
    AGArray t (Just xs)
mergeAnnGValue x _ = x -- FIXME: Make error condition.

mergeAnnGObject :: AnnGObject -> AnnGObject -> AnnGObject
mergeAnnGObject = OMap.unionWith mergeAnnInpVal

-- | Boilerplate constructor for a 'Field' suitable for use in remote joins.
bareNamedField :: G.Name -> Field
bareNamedField _fName =
  let _fAlias = G.Alias _fName
      (_fArguments, _fSelSet, _fRemoteRel) = (mempty, mempty, Nothing)
      -- we are constructing a nested query and we don't have the "named type"
      -- of the parent fields. A stub is okay since Field is more annotated
      -- than required for constructing a remote query:
      _fType = G.NamedType "unknown_type"
      _fSource = TLHasuraType
   in Field{..}

-- | Create an argument map using the inputs taken from the hasura database.
createArguments
  :: (MonadError QErr m)
  => Map.HashMap G.Variable G.ValueConst
  -> RemoteArguments
  -> m ArgsMap
createArguments variables (RemoteArguments arguments) =
  either
    (throw500 . T.pack . show)
    (pure . Map.fromList . map (\(G.ObjectFieldG key val) -> (key, valueConstToAnnInpVal val)))
    (toEither (substituteVariables variables arguments))

valueConstToAnnInpVal :: G.ValueConst -> AnnInpVal
valueConstToAnnInpVal vc =
  AnnInpVal
    { _aivType =
        G.TypeNamed (G.Nullability False) (G.NamedType (G.Name "unknown1"))
    , _aivVariable = Nothing
    , _aivValue = toAnnGValue vc
    }

toAnnGValue :: G.ValueConst -> AnnGValue
toAnnGValue =
  \case
    G.VCInt i -> AGScalar PGBigInt (Just (PGValInteger i))
    G.VCFloat v -> AGScalar PGFloat (Just (PGValDouble v))
    G.VCString (G.StringValue v) -> AGScalar PGText (Just (PGValText v))
    G.VCBoolean v -> AGScalar PGBoolean (Just (PGValBoolean v))
    G.VCNull -> AGScalar (PGUnknown "null") Nothing
    G.VCEnum {} -> AGScalar (PGUnknown "null") Nothing -- TODO: implement.
    G.VCList (G.ListValueG list) ->
      AGArray
        (G.ListType
           (G.TypeList
              (G.Nullability False)
              (G.ListType
                 (G.TypeNamed
                    (G.Nullability False)
                    (G.NamedType (G.Name "unknown2"))))))
        (pure (map valueConstToAnnInpVal list))
    G.VCObject (G.ObjectValueG keys) ->
      AGObject
        (G.NamedType (G.Name "unknown2"))
        (Just
           (OMap.fromList
              (map
                 (\(G.ObjectFieldG key val) -> (key, valueConstToAnnInpVal val))
                 keys)))

fieldsToRequest
  :: (MonadError QErr m)
  => G.OperationType
  -> [Field]
  -> m GQLReqParsed
fieldsToRequest opType fields = do
  case traverse fieldToField fields of
    Right gfields ->
      pure
        (GQLReq
           { _grOperationName = Nothing
           , _grQuery =
               GQLExecDoc
                 [ G.ExecutableDefinitionOperation
                     (G.OperationDefinitionTyped
                       (emptyOperationDefinition {
                         G._todSelectionSet = (map G.SelectionField gfields)
                         } )
                       )
                 ]
           , _grVariables = Nothing -- TODO: Put variables in here?
           })
    Left err -> throw500 ("While converting remote field: " <> err)
    where
      emptyOperationDefinition =
        G.TypedOperationDefinition {
          G._todType = opType
        , G._todName = Nothing
        , G._todVariableDefinitions = []
        , G._todDirectives = []
        , G._todSelectionSet = [] }

fieldToField :: Field -> Either Text G.Field
fieldToField Field{..} = do
  _fArguments <- traverse makeArgument (Map.toList _fArguments)
  _fSelectionSet <- fmap G.SelectionField . toList <$>
    traverse fieldToField _fSelSet
  _fDirectives <- pure []
  _fAlias      <- pure (Just _fAlias)
  pure G.Field{..}

makeArgument :: (G.Name, AnnInpVal) -> Either Text G.Argument
makeArgument (_aName, annInpVal) =
  do _aValue <- annInpValToValue annInpVal
     pure $ G.Argument {..}

annInpValToValue :: AnnInpVal -> Either Text G.Value
annInpValToValue = annGValueToValue . _aivValue

annGValueToValue :: AnnGValue -> Either Text G.Value
annGValueToValue = fromMaybe (pure G.VNull) .
  \case
    AGScalar _ty mv ->
      pgcolvalueToGValue <$> mv
    AGEnum _ _enumVal ->
      pure (Left "enum not supported")
    AGObject _ mobj ->
      flip fmap mobj $ \obj -> do
        fields <-
          traverse
            (\(_ofName, av) -> do
               _ofValue <- annInpValToValue av
               pure (G.ObjectFieldG {..}))
            (OMap.toList obj)
        pure (G.VObject (G.ObjectValueG fields))
    AGArray _ mvs ->
      fmap (G.VList . G.ListValueG) . traverse annInpValToValue <$> mvs

pgcolvalueToGValue :: PGScalarValue -> Either Text G.Value
pgcolvalueToGValue colVal = case colVal of
  PGValInteger i  -> pure $ G.VInt $ fromIntegral i
  PGValSmallInt i -> pure $ G.VInt $ fromIntegral i
  PGValBigInt i   -> pure $ G.VInt $ fromIntegral i
  PGValFloat f    -> pure $ G.VFloat $ realToFrac f
  PGValDouble d   -> pure $ G.VFloat $ realToFrac d
  -- TODO: Scientific is a danger zone; use its safe conv function.
  PGValNumeric sc -> pure $ G.VFloat $ realToFrac sc
  PGValBoolean b  -> pure $ G.VBoolean b
  PGValChar t     -> pure $ G.VString (G.StringValue (T.singleton t))
  PGValVarchar t  -> pure $ G.VString (G.StringValue t)
  PGValText t     -> pure $ G.VString (G.StringValue t)
  PGValCitext t   -> pure $ G.VString (G.StringValue t)
  PGValDate d     -> pure $ G.VString $ G.StringValue $ T.pack $ showGregorian d
  PGValTimeStampTZ u -> pure $
    G.VString $ G.StringValue $   T.pack $ formatTime defaultTimeLocale "%FT%T%QZ" u
  PGValTimeTZ (ZonedTimeOfDay tod tz) -> pure $
    G.VString $ G.StringValue $   T.pack (show tod ++ timeZoneOffsetString tz)
  PGNull _ -> pure G.VNull
  PGValJSON {}    -> Left "PGValJSON: cannot convert"
  PGValJSONB {}  -> Left "PGValJSONB: cannot convert"
  PGValGeo {}    -> Left "PGValGeo: cannot convert"
  PGValRaster {} -> Left "PGValRaster: cannot convert"
  PGValUUID u    -> pure $ G.VString (G.StringValue $ UUID.toText u)
  PGValUnknown t -> pure $ G.VString $ G.StringValue t
