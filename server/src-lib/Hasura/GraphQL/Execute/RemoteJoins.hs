{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE ViewPatterns #-}

module Hasura.GraphQL.Execute.RemoteJoins
  ( Batch(..)
  , BatchInputs(..)
  , RemoteRelField(..)
  , GQRespValue(..), gqRespData, gqRespErrors
  , encodeGQRespValue
  , parseGQRespValue
  , extractRemoteRelArguments
  , produceBatch
  , joinResults
  , emptyResp
  , rebuildFieldStrippingRemoteRels
  , fieldsToRequest
  ) where

import           Control.Arrow                          (first)
import           Control.Lens
import           Data.Scientific
import           Data.String
import           Data.Traversable
import           Data.Validation
import           Hasura.SQL.Types
import           Data.List
import           Data.List.NonEmpty                     (NonEmpty (..))
import           Data.Time
import           Hasura.GraphQL.Validate.Field
import           Hasura.SQL.Time

import qualified Data.List.NonEmpty                     as NE
import qualified Data.Vector                            as Vec
import qualified Data.Aeson.Ordered                     as OJ
import qualified Data.HashMap.Strict                    as Map
import qualified Data.HashMap.Strict.InsOrd             as OHM
import qualified Data.Sequence                          as Seq
import qualified Data.String.Conversions                as CS
import qualified Data.Text                              as T
import qualified VectorBuilder.Builder                  as VB 
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Hasura.GraphQL.Validate                as VQ

import           Hasura.EncJSON
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.GraphQL.Validate.Types
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Value

newtype RemoteRelKey =
  RemoteRelKey Int
  deriving (Eq, Ord, Show, Hashable)

data RemoteRelField =
  RemoteRelField
    { rrRemoteField   :: !RemoteField
    , rrArguments     :: !ArgsMap
    , rrSelSet        :: !SelSet
    , rrRelFieldPath  :: !RelFieldPath
    , rrAlias         :: !G.Alias
    , rrAliasIndex    :: !Int
    , rrPhantomFields :: ![Text]
    }
  deriving (Show)

newtype RelFieldPath =
  RelFieldPath {
    unRelFieldPath :: (Seq.Seq (Int, G.Alias))
  } deriving (Show, Monoid, Semigroup, Eq)

data InsertPath =
  InsertPath
    { ipFields :: !(Seq.Seq Text)
    , ipIndex  :: !(Maybe ArrayIndex)
    }
  deriving (Show, Eq)

newtype ArrayIndex =
  ArrayIndex Int
  deriving (Show, Eq, Ord)

-- Rebuild the field with remote relationships removed, and paths that
-- point back to them.
rebuildFieldStrippingRemoteRels ::
     VQ.Field -> Maybe (VQ.Field, NonEmpty RemoteRelField)
rebuildFieldStrippingRemoteRels =
  traverse NE.nonEmpty . flip runState mempty . rebuild 0 mempty
  where
    rebuild idx0 parentPath field0 = do
      selSetEithers <-
        for (zip [0..] (toList (_fSelSet field0))) $
          \(idx, subfield) ->
             case _fRemoteRel subfield of
               Nothing -> fmap Right (rebuild idx thisPath subfield)
               Just remoteField -> do
                 modify (remoteRelField :)
                 pure (Left remoteField)
                 where remoteRelField =
                         RemoteRelField
                           { rrRemoteField = remoteField
                           , rrArguments = _fArguments subfield
                           , rrSelSet = _fSelSet subfield
                           , rrRelFieldPath = thisPath
                           , rrAlias = _fAlias subfield
                           , rrAliasIndex = idx
                           , rrPhantomFields =
                               map
                                 G.unName
                                 (filter
                                    (`notElem` fmap _fName (_fSelSet field0))
                                    (hasuraFieldNames remoteField))
                           }
      let fields = rights selSetEithers
      pure
        field0
          { _fSelSet =
              Seq.fromList $
                selSetEithers >>= \case
                  Right field -> [field]
                  Left remoteField ->
                    mapMaybe
                      (\name ->
                         if elem name (map _fName fields)
                           then Nothing
                           else Just
                                  (Field
                                     { _fAlias = G.Alias name
                                     , _fName = name
                                     , _fType =
                                         G.NamedType (G.Name "unknown3")
                                     , _fArguments = mempty
                                     , _fSelSet = mempty
                                     , _fRemoteRel = Nothing
                                     }))
                      (hasuraFieldNames remoteField)
          }
      where
        thisPath = parentPath <> RelFieldPath (pure (idx0, _fAlias field0))
        hasuraFieldNames = 
          map (G.Name . getFieldNameTxt) . toList . rtrHasuraFields . rmfRemoteRelationship

-- | Get a list of fields needed from a hasura result.
neededHasuraFields
  :: RemoteField -> [FieldName]
neededHasuraFields = toList . rtrHasuraFields . rmfRemoteRelationship

-- | Join the data returned from the original hasura request with the remote values.
joinResults :: GQRespValue
            -> [(Batch, EncJSON)]
            -> GQRespValue
joinResults hasuraValue0 =
  foldl' (uncurry . insertBatchResults) hasuraValue0 . map (fmap jsonToGQRespVal)
  
  where
    jsonToGQRespVal = either mkErr id . parseGQRespValue
    mkErr = appendJoinError emptyResp . ("joinResults: eitherDecode: " <>) . fromString

emptyResp :: GQRespValue
emptyResp = GQRespValue OJ.empty VB.empty

-- TODO it's not clear which error conditions here represent internal errors (i.e. bugs)...
-- | Insert at path, index the value in the larger structure.
insertBatchResults ::
     GQRespValue
  -- ^ Original hasura response with accumulated remote responses
  -> Batch
  -> GQRespValue
  -> GQRespValue
insertBatchResults accumResp Batch{..} remoteResp =
  -- It's not clear what to do about errors here, or when to short-circuit so
  -- try to do as much computation as possible for now:
  case remoteDataE of
    Left err -> appendJoinError accumResp err
    Right remoteData0 -> do
      case inHashmap batchRelFieldPath accumData0 remoteData0 of
        Left err  -> appendJoinError accumRespWithRemoteErrs err
        Right (val, _) -> set gqRespData val accumRespWithRemoteErrs
  where
    accumRespWithRemoteErrs = gqRespErrors <>~ _gqRespErrors remoteResp $ accumResp 
    accumData0 = _gqRespData accumResp
    -- The 'sortOn' below is not strictly necessary by the spec, but
    -- implementations may not guarantee order of results matching
    -- order of query.
    -- Since remote results are aliased by keys of the form 'remote_result_1', 'remote_result_2',
    -- we can sort by the keys for a ordered list
    -- On error, we short-circuit
    remoteDataE = let indexedRemotes = map getIdxRemote $ OJ.toList $ _gqRespData remoteResp
                  in case any isNothing indexedRemotes of
                       True -> Left "couldn't parse all remote results"
                       False -> Right $ map snd $ sortOn fst $ catMaybes indexedRemotes

    getIdxRemote (remoteAlias, value) = let idxStr = stripPrefix "hasura_array_idx_" (T.unpack remoteAlias)
                                  in (, value) <$> (join $ readMaybe <$> idxStr :: Maybe Int)

    cardinality = biCardinality batchInputs

    inHashmap ::
         RelFieldPath
      -> OJ.Object
      -> [OJ.Value]
      -- ^ The remote result data with no keys, only the values sorted by key
      -> Either GQJoinError (OJ.Object, [OJ.Value])
    inHashmap (RelFieldPath p) accumData remoteDataVals = case p of
      Seq.Empty ->
        case remoteDataVals of
          [] -> Left $ err <> showHashO accumData
            where err = case cardinality of
                          One -> "Expected one remote object but got none, while traversing "
                          Many -> "Expected many objects but got none, while traversing "

          (remoteDataVal:rest)
            | cardinality == One && not (null rest) ->
                Left $
                  "Expected one remote object but got many, while traversing " <> showHashO accumData
            | otherwise ->
                (, rest) <$> spliceRemote remoteDataVal accumData

      ((idx, G.Alias (G.Name key)) Seq.:<| rest) ->
        case OJ.lookup key accumData of
          Nothing ->
            Left $
              "Couldn't find expected key " <> fromString (show key) <> " in " <> showHashO accumData <>
              ", while traversing " <> showHashO accumData0
          Just currentValue ->
            first (\newValue -> OJ.insert (idx, key) newValue accumData)
              <$> inValue rest currentValue remoteDataVals

    -- TODO Brandon note:
    -- it might be fruitful to inline this and try simplifying further. In
    -- particular I'm looking at the way that e.g. the third argument being []
    -- is always an error condition, but this is checked in several different
    -- places. See also TODO note below
    inValue ::
         Seq.Seq (Int, G.Alias)
      -> OJ.Value
      -> [OJ.Value]
      -> Either GQJoinError (OJ.Value, [OJ.Value])
    inValue path currentValue remoteDataVals =
      case currentValue of
        OJ.Object accumData ->
          first OJ.Object <$>
            inHashmap (RelFieldPath path) accumData remoteDataVals

        OJ.Array hasuraRowValues -> first (OJ.Array . Vec.fromList . reverse) <$>
          let foldHasuraRowObjs f = foldM f_onObj ([], remoteDataVals) hasuraRowValues
                where
                  f_onObj tup (OJ.Object accumData) = f tup accumData
                  f_onObj _    hasuraRowValue = Left $
                    "expected array of objects in " <> showHash hasuraRowValue
           in case path of
                -- TODO Brandon note:
                -- do we need to assert something about the Right.snd of the result here?
                -- the remainingRemotes tail (xs) is tossed away (I think?) in caller
                Seq.Empty ->
                  case cardinality of
                    Many -> foldHasuraRowObjs $
                      \(hasuraRowsSoFar, remainingRemotes) accumData ->
                         case remainingRemotes of
                           [] ->
                             Left $
                               "no remote objects left for joining at " <> showHashO accumData
                           (remoteDataVal:rest) -> do
                             spliced <- spliceRemote remoteDataVal accumData
                             pure
                               (OJ.Object spliced : hasuraRowsSoFar , rest)
                    One ->
                      Left "Cardinality mismatch: found array in hasura value, but expected object"

                nonEmptyPath -> foldHasuraRowObjs $
                  \(hasuraRowsSoFar, remainingRemotes) accumData ->
                     first (\hasuraRowHash'-> OJ.Object hasuraRowHash' : hasuraRowsSoFar) <$>
                       inHashmap
                         (RelFieldPath nonEmptyPath)
                         accumData
                         remainingRemotes

        OJ.Null -> Right (OJ.Null, remoteDataVals)
        _ ->
          Left $
            "Expected object or array in hasura value but got: " <> showHash currentValue

    -- splice the remote result into the hasura result with the proper key (and
    -- at the right index), removing phantom fields
    spliceRemote remoteDataVal accumData = do
      peeledValue <- peelOffNestedFields batchNestedFields remoteDataVal
      pure $
        -- TODO Brandon note:
        -- document rrPhantomFields/batchPhantoms so that we can understand if or why it's okay that the OJ.delete might silently be a noop here:
        foldl' (flip OJ.delete)
                -- TODO Brandon note:
                -- document why we know OJ.insert might not silently clobber an existing field in accumData; e.g. with an assertion here, or directing reader to someplace validation is performed
               (OJ.insert
                  (batchRelationshipKeyIndex, batchRelationshipKeyToMake)
                  peeledValue
                  accumData)
               batchPhantoms

    -- logging helpers:
    showHash = fromString . show . OJ.toEncJSON
    showHashO = showHash . OJ.Object


-- | The drop 1 in here is dropping the first level of nesting. The
-- top field is already aliased to e.g. foo_idx_1, and that layer is
-- already peeled off. So here we are just peeling nested fields.
peelOffNestedFields :: MonadError GQJoinError m => NonEmpty G.Name -> OJ.Value -> m OJ.Value
peelOffNestedFields topNames topValue = foldM peel topValue (NE.drop 1 topNames)
  where
    peel value (G.Name key) = 
      case value of
        OJ.Object (OJ.lookup key -> Just value') -> -- NOTE: ViewPatterns
          pure value'
        _ -> 
          throwError $ fromString $
            "No " <> show key <> " in " <> show value <> " from " <> 
            show topValue <> " with " <> show topNames


-- TODO document all of this
data Batch =
  Batch
    { batchRemoteTopQuery        :: !VQ.RemoteTopField
    , batchRelFieldPath          :: !RelFieldPath
    , batchIndices               :: ![ArrayIndex]
    , batchRelationshipKeyToMake :: !Text
    , batchRelationshipKeyIndex  :: !Int
      -- ^ Insertion index in target object.
    , batchInputs                :: !BatchInputs
    , batchNestedFields          :: !(NonEmpty G.Name)
    , batchPhantoms              :: ![Text]
    } deriving (Show)

-- | Produce batch queries for a given remote relationship.
produceBatch ::
     G.OperationType
  -> RemoteSchemaInfo
  -> RemoteRelField
  -> BatchInputs
  -> Batch
produceBatch rtqOperationType rtqRemoteSchemaInfo RemoteRelField{..} batchInputs =
  Batch{..}
  where
    batchRelationshipKeyToMake = G.unName (G.unAlias rrAlias)
    batchNestedFields =
      fmap
        fcName
        (rtrRemoteFields
           (rmfRemoteRelationship rrRemoteField))
    batchRemoteTopQuery = VQ.RemoteTopField{..}
    rtqFields =
      flip map indexedRows $ \(i, variables) ->
         fieldCallsToField
           rrArguments
           variables
           rrSelSet
           (Just (arrayIndexAlias i))
           (rtrRemoteFields remoteRelationship)
    indexedRows = zip (map ArrayIndex [0 :: Int ..]) $ toList $ biRows batchInputs
    batchIndices = map fst indexedRows
    remoteRelationship = rmfRemoteRelationship rrRemoteField
    -- TODO These might be good candidates for reusing names with DuplicateRecordFields if they have the same semantics:
    batchRelationshipKeyIndex = rrAliasIndex
    batchPhantoms = rrPhantomFields
    batchRelFieldPath = rrRelFieldPath

-- | Produce the alias name for a result index.
arrayIndexAlias :: ArrayIndex -> G.Alias
arrayIndexAlias i =
  G.Alias (G.Name (arrayIndexText i))

-- | Produce the alias name for a result index.
arrayIndexText :: ArrayIndex -> Text
arrayIndexText (ArrayIndex i) =
  T.pack ("hasura_array_idx_" ++ show i)

-- | Produce a field from the nested field calls.
fieldCallsToField ::
     Map.HashMap G.Name AnnInpVal
  -> Map.HashMap G.Variable G.ValueConst
  -> SelSet
  -> Maybe G.Alias
  -> NonEmpty FieldCall
  -> Field
fieldCallsToField userProvidedArguments variables finalSelSet = nest
  where
    nest mindexedAlias (FieldCall{..} :| rest) =
      Field
        { _fAlias = fromMaybe (G.Alias fcName) mindexedAlias
        , _fName = fcName
        , _fType = G.NamedType (G.Name "unknown_type")
        , _fArguments =
            let templatedArguments =
                  createArguments variables fcArguments
             in case NE.nonEmpty rest of
                  Just {} -> templatedArguments
                  Nothing ->
                    Map.unionWith
                      mergeAnnInpVal
                      userProvidedArguments
                      templatedArguments
        , _fSelSet = maybe finalSelSet (pure . nest Nothing) $ NE.nonEmpty rest
        , _fRemoteRel = Nothing
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
mergeAnnGObject = OHM.unionWith mergeAnnInpVal

-- | Create an argument map using the inputs taken from the hasura database.
createArguments ::
     Map.HashMap G.Variable G.ValueConst
  -> RemoteArguments
  -> Map.HashMap G.Name AnnInpVal
createArguments variables (RemoteArguments arguments) =
  either
    (error . show)
    (Map.fromList . map (\(G.ObjectFieldG key val) -> (key, valueConstToAnnInpVal val)))
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
           (OHM.fromList
              (map
                 (\(G.ObjectFieldG key val) -> (key, valueConstToAnnInpVal val))
                 keys)))

-- | Extract from the Hasura results the remote relationship arguments.
extractRemoteRelArguments ::
     EncJSON
  -> [RemoteRelField]
  -> ( GQRespValue , [ BatchInputs ])
     -- ^ Errors are accumulated in GQRespValue. [BatchInputs] may be empty.
extractRemoteRelArguments hasuraJson rels =
  case parseGQRespValue hasuraJson of
    Left (fromString-> err) ->
      (appendJoinError emptyResp $ "decode error: " <> err, [])
    Right gqResp@GQRespValue{..} -> do
      let hashE =
            execStateT
              (extractFromResult One keyedRemotes (OJ.Object _gqRespData))
              mempty
      case hashE of
        Left err -> (appendJoinError gqResp err, [])
        Right hash -> (gqResp, Map.elems hash)
  where
    keyedRemotes = zip (fmap RemoteRelKey [0 ..]) rels

data BatchInputs =
  BatchInputs
    { biRows        :: !(Seq.Seq (Map.HashMap G.Variable G.ValueConst))
    , biCardinality :: Cardinality
    } deriving (Show)

instance Semigroup BatchInputs where
  (<>) (BatchInputs r1 c1) (BatchInputs r2 c2) =
    BatchInputs (r1 <> r2) (c1 <> c2)

data Cardinality = Many | One
 deriving (Eq, Show)

instance Semigroup Cardinality where
    (<>) _ Many  = Many
    (<>) Many _  = Many
    (<>) One One = One

-- TODO extract what from what exactly? Document please.
-- | Extract from a given result.
extractFromResult ::
     Cardinality
  -> [(RemoteRelKey, RemoteRelField)]
  -> OJ.Value
  -> StateT (Map.HashMap RemoteRelKey BatchInputs) (Either GQJoinError) ()
extractFromResult cardinality keyedRemotes value =
  case value of
    OJ.Array values -> mapM_ (extractFromResult Many keyedRemotes) values
    OJ.Object hashmap -> do
      remotesRows :: Map.HashMap RemoteRelKey (Seq.Seq ( G.Variable
                                                       , G.ValueConst)) <-
        foldM
          (\result (key, remotes) ->
             case OJ.lookup key hashmap of
               Just subvalue -> do
                 let (remoteRelKeys, unfinishedKeyedRemotes) =
                       partitionEithers (toList remotes)
                 extractFromResult cardinality unfinishedKeyedRemotes subvalue
                 pure
                   (foldl'
                      (\result' remoteRelKey ->
                         Map.insertWith
                           (<>)
                           remoteRelKey
                           (pure
                              ( G.Variable (G.Name key)
                                -- TODO: Pay attention to variable naming wrt. aliasing.
                              , valueToValueConst subvalue))
                           result')
                      result
                      remoteRelKeys)
               Nothing ->
                 lift
                   (Left $
                      fromString $
                        "Expected key " <> T.unpack key <> " at this position: " <>
                          show (OJ.toEncJSON value)))
          mempty
          (Map.toList candidates)
      mapM_
        (\(remoteRelKey, row) ->
           modify
             (Map.insertWith
                (flip (<>))
                remoteRelKey
                (BatchInputs {biRows = pure (Map.fromList (toList row))
                             ,biCardinality = cardinality})))
        (Map.toList remotesRows)
    _ -> pure ()
  where
    candidates ::
         Map.HashMap Text (NonEmpty (Either RemoteRelKey ( RemoteRelKey
                                                         , RemoteRelField)))
    candidates =
      foldl'
        (\(!outerHashmap) keys ->
           foldl'
             (\(!innerHashmap) (key, remote) ->
                Map.insertWith (<>) key (pure remote) innerHashmap)
             outerHashmap
             keys)
        mempty
        (fmap peelRemoteKeys keyedRemotes)

-- | Peel one layer of expected keys from the remote to be looked up
-- at the current level of the result object.
peelRemoteKeys ::
     (RemoteRelKey, RemoteRelField) -> [(Text, Either RemoteRelKey (RemoteRelKey, RemoteRelField))]
peelRemoteKeys (remoteRelKey, remoteRelField) =
  map
    (updatingRelPath . unconsPath)
    (neededHasuraFields (rrRemoteField remoteRelField))
  where
    updatingRelPath ::
         Either Text (Text, RelFieldPath)
      -> (Text, Either RemoteRelKey (RemoteRelKey, RemoteRelField))
    updatingRelPath result =
      case result of
        Right (key, remainingPath) ->
          ( key
          , Right (remoteRelKey, remoteRelField {rrRelFieldPath = remainingPath}))
        Left key -> (key, Left remoteRelKey)
    unconsPath :: FieldName -> Either Text (Text, RelFieldPath)
    unconsPath fieldName =
      case rrRelFieldPath remoteRelField of
        RelFieldPath Seq.Empty -> Left (getFieldNameTxt fieldName)
        RelFieldPath ((_, G.Alias (G.Name key)) Seq.:<| xs) -> Right (key, RelFieldPath xs)

-- | Convert a JSON value to a GraphQL value.
valueToValueConst :: OJ.Value -> G.ValueConst
valueToValueConst =
  \case
    OJ.Array xs -> G.VCList (G.ListValueG (fmap valueToValueConst (toList xs)))
    OJ.String str -> G.VCString (G.StringValue str)
    -- TODO: Note the danger zone of scientific:
    OJ.Number sci -> either G.VCFloat G.VCInt (floatingOrInteger sci)
    OJ.Null -> G.VCNull
    OJ.Bool b -> G.VCBoolean b
    OJ.Object hashmap ->
      G.VCObject
        (G.ObjectValueG
           (map
              (\(key, value) ->
                 G.ObjectFieldG (G.Name key) (valueToValueConst value))
              (OJ.toList hashmap)))

fieldsToRequest
  :: (MonadIO m, MonadError QErr m)
  => G.OperationType
  -> [VQ.Field]
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

fieldToField :: VQ.Field -> Either Text G.Field
fieldToField VQ.Field{..} = do
  _fArguments <- traverse makeArgument (Map.toList _fArguments)
  _fSelectionSet <- fmap G.SelectionField . toList <$>
    traverse fieldToField _fSelSet
  _fDirectives <- pure []
  _fAlias      <- pure (Just _fAlias)
  pure $ 
    G.Field{..}

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
    AGEnum _ mval ->
      pure . G.VEnum <$> mval
    AGObject _ mobj ->
      flip fmap mobj $ \obj -> do
        fields <-
          traverse
            (\(_ofName, av) -> do
               _ofValue <- annInpValToValue av
               pure (G.ObjectFieldG {..}))
            (OHM.toList obj)
        pure (G.VObject (G.ObjectValueG fields))
    AGArray _ mvs ->
      fmap (G.VList . G.ListValueG) . traverse annInpValToValue <$> mvs

pgcolvalueToGValue :: PGColValue -> Either Text G.Value
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
  PGValDate d     -> pure $ G.VString $ G.StringValue $ T.pack $ showGregorian d
  PGValTimeStampTZ u -> pure $
    G.VString $ G.StringValue $   T.pack $ formatTime defaultTimeLocale "%FT%T%QZ" u
  PGValTimeTZ (ZonedTimeOfDay tod tz) -> pure $
    G.VString $ G.StringValue $   T.pack (show tod ++ timeZoneOffsetString tz)
  PGNull _ -> pure G.VNull
  PGValJSON {}    -> Left "PGValJSON: cannot convert"
  PGValJSONB {}  -> Left "PGValJSONB: cannot convert"
  PGValGeo {}    -> Left "PGValGeo: cannot convert"
  PGValUnknown t -> pure $ G.VString $ G.StringValue t

appendJoinError :: GQRespValue -> GQJoinError -> GQRespValue
appendJoinError resp err = 
  gqRespErrors <>~ (VB.singleton $ gqJoinErrorToValue err) $ resp
