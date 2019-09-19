{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE ViewPatterns             #-}

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
  , fieldCallsToField
  , enumerateRowAliases
  , unEnumerateRowAliases
  ) where

import           Control.Arrow                          (first)
import           Control.Lens
import           Data.List
import           Data.List.NonEmpty                     (NonEmpty (..))
import           Data.Scientific
import           Data.Set                               (Set)
import qualified Data.Set                               as Set
import           Data.String
import           Data.Time
import           Data.Validation
import           Hasura.GraphQL.Validate.Field
import           Hasura.SQL.Time
import           Hasura.SQL.Types

import qualified Data.Aeson.Ordered                     as OJ
import qualified Data.HashMap.Strict                    as Map
import qualified Data.HashMap.Strict.InsOrd             as OHM
import qualified Data.List.NonEmpty                     as NE
import qualified Data.Sequence                          as Seq
import qualified Data.String.Conversions                as CS
import qualified Data.Text                              as T
import qualified Data.Vector                            as Vec
import qualified Hasura.GraphQL.Validate                as VQ
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified VectorBuilder.Builder                  as VB

import           Hasura.EncJSON
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.GraphQL.Validate.Types
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Value

-- | Like a 'VQ.Field' but with a path indicating where its response should be
-- spliced back into the hasura result.
data RemoteRelField =
  RemoteRelField
    { rrRemoteField   :: !RemoteField
    -- ^ The hasura to remote schema field mapping.
    , rrArguments     :: !ArgsMap
    -- ^ See '_fArguments'.
    , rrSelSet        :: !SelSet
    -- ^ Selection set of 'rrAlias' (may be empty)
    , rrRelFieldPath  :: !RelFieldPath
    -- ^ Splice path.
    , rrAlias         :: !G.Alias
    -- ^ A possible alias (provided by client) for `rtrName .
    -- rmfRemoteRelationship . rrRemoteField`, otherwise this will be defaulted
    -- to that value.
    , rrAliasIndex    :: !Int
    -- ^ index of selection set at 'rrRelFieldPath' at which this remote field should be spliced.
    -- This is morally the last component of 'rrRelFieldPath'.
    , rrPhantomFields :: ![Text]
    -- ^ Hasura fields which are not in the selection set, but are required as
    -- parameters to satisfy the remote join.
    }
  deriving (Show)

-- | A path of ordered keys (suitable for 'OJ.insert') allowing us to splice a
-- remote result at a nested location.
newtype RelFieldPath =
  RelFieldPath {
    unRelFieldPath :: (Seq.Seq (Int, G.Alias))
  } deriving (Show, Monoid, Semigroup, Eq)

-- | Rebuild the query tree, returning a new tree with remote relationships
-- removed, along with a list of paths that point back to them.
--
-- 'Nothing' means there were no remote fields.
--
-- Output here is eventually assembled into a 'QExecPlan' 'Tree' in
-- 'getExecPlan', and finally executed in 'runGQ'.
rebuildFieldStrippingRemoteRels
  :: VQ.Field 
  -> Maybe (VQ.Field, NonEmpty RemoteRelField)
  -- ^ NOTE: the ordering of the _fSelSet fields in the result 'VQ.Field' seems not to matter at 
  -- all for correctness here (based on experimentation)... I haven't puzzled out why yet.
rebuildFieldStrippingRemoteRels =
  traverse NE.nonEmpty . flip runState mempty . rebuild 0 mempty
  where
    rebuild :: Int -> RelFieldPath -> Field -> State [RemoteRelField] Field
    rebuild idx0 parentPath field0 = do
      forMOf fSelSet field0 $ \ss0 ->
        fmap join $ flip Seq.traverseWithIndex ss0 $
          \idx subfield ->
             case _fRemoteRel subfield of
               Nothing -> Seq.singleton <$> rebuild idx thisPath subfield
               Just remoteField -> do

                 modify (remoteRelField :)
                 -- NOTE: this may result in redundant fields in the SELECT, but this seems fine.
                 -- NOTE: this sub-sequence is in arbitrary order, since coming from Set.
                 pure $ Seq.fromList $
                   map (\name ->
                       Field
                         { _fAlias = G.Alias name
                         , _fName = name
                         , _fType = G.NamedType (G.Name "unknown3")
                         , _fArguments = mempty
                         , _fSelSet = mempty
                         , _fRemoteRel = Nothing
                         })
                     $ toList hasuraFieldNames

                 where remoteRelField =
                         RemoteRelField
                           { rrRemoteField = remoteField
                           , rrArguments = _fArguments subfield
                           , rrSelSet = _fSelSet subfield
                           , rrRelFieldPath = thisPath
                           , rrAlias = _fAlias subfield
                           , rrAliasIndex = idx
                           , rrPhantomFields =
                               coerce $ toList $
                                 hasuraFieldNames `Set.difference` siblingSelections
                           }
                       hasuraFieldNames = 
                         coerceSet $ rtrHasuraFields $ rmfRemoteRelationship remoteField
      where
        thisPath = parentPath <> RelFieldPath (pure (idx0, _fAlias field0))
        siblingSelections = Set.fromList $ toList $ fmap _fName $ _fSelSet field0


-- | Join the data returned from the original hasura request with the remote values.
joinResults :: GQRespValue -> [(Batch, EncJSON)] -> GQRespValue
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
  -- ^ Original hasura response
  -> Batch
  -> GQRespValue
  -- ^ Remote response
  -> GQRespValue
  -- ^ Original hasura response with accumulated remote responses and errors
insertBatchResults accumResp Batch{..} remoteResp =
  -- It's not clear what to do about errors here, or when to short-circuit so
  -- try to do as much computation as possible for now:
  case orderedTopLevelRemoteSelections of
    Nothing -> appendJoinError accumResp $ 
      "did not find expected hasura alias prefex in all remote results: " <>
      fromString (show $ _gqRespData remoteResp)
    Just remoteData0 -> do
      case inHashmap batchRelFieldPath accumData0 remoteData0 of
        Left err -> appendJoinError accumRespWithRemoteErrs err
        -- TODO assert something about _remainingRemote?
        Right (val, _remainingRemote) -> set gqRespData val accumRespWithRemoteErrs
  where
    accumRespWithRemoteErrs = gqRespErrors <>~ _gqRespErrors remoteResp $ accumResp
    accumData0 = _gqRespData accumResp
    -- Re-sort expected aliases to account for naughty remotes that don't
    -- respect the ordering parts of spec. See 'enumerateRowAliases':
    orderedTopLevelRemoteSelections = unEnumerateRowAliases $ OJ.toList $ _gqRespData remoteResp

    inHashmap ::
         RelFieldPath
      -> OJ.Object
      -> [OJ.Value]
      -- ^ The top-level selection set of remote result, sorted.
      -> Either GQJoinError (OJ.Object, [OJ.Value])
      -- ^ Accumulated joined response JSON, along with remaining remote top-level field responses
    inHashmap (RelFieldPath p) accumData remoteDataVals = case p of
      Seq.Empty ->
        case remoteDataVals of
          [] -> Left $ err <> showHashO accumData
            where err = case batchCardinality of
                          One -> "Expected one remote object but got none, while traversing "
                          Many -> "Expected many objects but got none, while traversing "

          (remoteDataVal:rest)
            | batchCardinality == One && not (null rest) ->
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
                  case batchCardinality of
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
    spliceRemote :: OJ.Value -> OJ.Object -> Either GQJoinError OJ.Object 
    spliceRemote remoteDataVal accumData = do
      peeledValue <- peelOffNestedFields batchNestedFields remoteDataVal
      pure $
        -- Strip phantom fields
        foldl' (flip OJ.delete)
                -- TODO Brandon note:
                -- document why we know OJ.insert might not silently clobber an existing field in accumData; e.g. with an assertion here, or directing reader to someplace validation is performed
                -- TODO Brandon note:
                -- should we delete first? how do we know batchRelationshipKeyToMake isn't in batchPhantoms? Document.
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


-- TODO In what sense is this a "batch"?
data Batch =
  Batch
    { batchRelFieldPath          :: !RelFieldPath
    -- ^ See 'rrRelFieldPath'.
    , batchRelationshipKeyToMake :: !Text
    -- ^ See 'rrAlias'
    , batchRelationshipKeyIndex  :: !Int
    -- ^ Insertion index in target object. See 'rrAliasIndex'.
    , batchCardinality           :: !Cardinality
    -- ^ See 'biCardinality'
    , batchNestedFields          :: !(NonEmpty G.Name)
    -- ^ See 'rtrRemoteFields'.
    , batchPhantoms              :: ![Text]
    -- ^ See 'rrPhantomFields'.
    } deriving (Show)

-- | Produce batch queries for a given remote relationship.
produceBatch ::
     RemoteRelField
  -> Cardinality
  -> Batch
produceBatch RemoteRelField{..} batchCardinality =
  Batch{..}
  where
    batchRelationshipKeyToMake = G.unName (G.unAlias rrAlias)
    batchNestedFields =
      fmap
        fcName
        (rtrRemoteFields
           (rmfRemoteRelationship rrRemoteField))
    -- TODO These might be good candidates for reusing names with DuplicateRecordFields if they have the same semantics:
    batchRelationshipKeyIndex = rrAliasIndex
    batchPhantoms = rrPhantomFields
    batchRelFieldPath = rrRelFieldPath

-- | Tag 'biRows' list with enumerated aliases we can use to recover the
-- original order of the input list here, from the remote results (even if the
-- remote is not well-behaved wrt ordering.
--
-- This must satisfy (modulo newtypes)
--
-- @
--   id =  unEnumerateRowAliases . enumerateRowAliases
-- @
enumerateRowAliases :: [row] -> [(G.Alias, row)]
enumerateRowAliases = map (over _1 alias) . zip [0::Int ..] where
  alias i = G.Alias $ G.Name $ "hasura_array_idx_" <> T.pack (show i)

-- | re-sort and return aliased results, following 'enumerateRowAliases'
--
-- 'Nothing' here means either a bug or a very naughty remote.
unEnumerateRowAliases :: [(Text, row)] -> Maybe [row]
unEnumerateRowAliases = fmap (map snd . sortOn fst) . mapM (mapMOf _1 toInt)
  where
    -- NOTE: a naive lexicographic sort here will be incorrect when len > 9
    toInt s = do 
      ns <- T.stripPrefix hasuraAliasTag s
      readMaybe (T.unpack ns) :: Maybe Int

hasuraAliasTag :: Text
hasuraAliasTag = "hasura_array_idx_"

-- | Produce a field from the nested field calls.
fieldCallsToField ::
     Map.HashMap G.Name AnnInpVal
  -> Map.HashMap G.Variable G.ValueConst
  -> SelSet
  -> G.Alias
  -> NonEmpty FieldCall
  -> Field
fieldCallsToField userProvidedArguments variables finalSelSet topAlias = 
  set fAlias topAlias . nest
  where
    -- almost: `foldr nest finalSelSet`
    nest (FieldCall{..} :| rest) =
      Field
        { _fAlias = G.Alias fcName
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
        , _fSelSet = maybe finalSelSet (pure . nest) $ NE.nonEmpty rest
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

-- throwaway; used internally in extractRemoteRelArguments.
newtype RemoteRelKey =
  RemoteRelKey Int
  deriving (Eq, Ord, Show, Hashable, Enum)

-- | Extract from the Hasura results the remote relationship arguments.
extractRemoteRelArguments ::
     EncJSON
  -> [RemoteRelField]
  -- ^ NOTE: This function only makes use of rrRelFieldPath, and 
  -- (rtrHasuraFields . rmfRemoteRelationship . rrRemoteField); maybe refactor.
  -> ( GQRespValue , [ BatchInputs ])
  -- ^ Errors are accumulated in GQRespValue. [BatchInputs] may be empty.
extractRemoteRelArguments hasuraJson rels =
  case parseGQRespValue hasuraJson of
    Left (fromString-> err) ->
      (appendJoinError emptyResp $ "decode error: " <> err, [])
    Right gqResp@GQRespValue{..} -> do
      let hashE = Map.elems <$>
            execStateT (go One (keyRemotes rels) (OJ.Object _gqRespData)) mempty
      case hashE of
        Left err  -> (appendJoinError gqResp err, [])
        Right bis -> (gqResp, bis)
  where
    keyRemotes = 
      zip [RemoteRelKey 0 ..] .
        map (\r-> (rrRelFieldPath r, rtrHasuraFields $ rmfRemoteRelationship $ rrRemoteField r))

    -- insert into map, accumulating from the left on duplicates
    insertWithCons m k v = Map.insertWith (<>) k (pure v) m

    go :: Cardinality
       -> [(RemoteRelKey, (RelFieldPath, Set FieldName))]
       -> OJ.Value
       -> StateT (Map.HashMap RemoteRelKey BatchInputs) (Either GQJoinError) ()
    go biCardinality keyedRemotes hasuraResp =
      case hasuraResp of
        OJ.Array values -> mapM_ (go Many keyedRemotes) values
        OJ.Object hashmap -> do
          remotesRows :: Map.HashMap RemoteRelKey [(G.Variable, G.ValueConst)] <-
            foldM
              (\result (key, remotes) ->
                 case OJ.lookup key hashmap of
                   Just subvalue -> do
                     let (remoteRelKeys, unfinishedKeyedRemotes) =
                           partitionEithers (toList remotes)
                     go biCardinality unfinishedKeyedRemotes subvalue
                     pure
                       (foldl'
                          (\result' remoteRelKey ->
                             insertWithCons result' remoteRelKey
                              ( G.Variable (G.Name key)
                                -- TODO: Pay attention to variable naming wrt. aliasing.
                              , valueToValueConst subvalue)
                          )
                          result
                          remoteRelKeys)
                   Nothing -> throwError $ fromString $
                    "Expected key " <> T.unpack key <> " at this position: " <> show (OJ.toEncJSON hasuraResp)
              )
              mempty
              (Map.toList $ foldCandidates keyedRemotes)

          modify $ Map.unionWith (flip (<>)) $
            fmap ((\biRows -> BatchInputs{..}) . pure . Map.fromList) remotesRows

        -- scalar values are the base case; assert no remotes left:
        -- NOTE: keyedRemotes may be non-empty here when permissions have filtered hasura results:
        OJ.Null -> pure ()
        -- ...otherwise this is an internal error (I think):
        _ -> when (not $ null keyedRemotes) $
               throwError $ fromString $
                 "In extractRemoteRelArguments expected empty keyedRemotes, got: " <> show keyedRemotes

    -- NOTE: RemoteRelKey is just passed through in these two functions in a
    -- way that's convenient for our `partitionEithers` above:
    foldCandidates
      :: [(RemoteRelKey, (RelFieldPath, Set FieldName))]
      -> Map.HashMap Text (NonEmpty (Either RemoteRelKey (RemoteRelKey, (RelFieldPath, Set FieldName))))
    foldCandidates =
      foldl' (uncurry . insertWithCons) mempty . concatMap peelRemoteKeys

    -- TODO understand and document this better:
    -- Peel one layer of expected keys from the remote to be looked up at the
    -- current level of the result object.
    --
    peelRemoteKeys 
      :: (RemoteRelKey, (RelFieldPath, Set FieldName)) 
      -> [(Text, Either RemoteRelKey (RemoteRelKey, (RelFieldPath, Set FieldName)))]
    peelRemoteKeys (remoteRelKey, (relFieldPath, hasuraFields)) =
      -- For each hasura field name...
      map unconsUpdatingRelPath (toList hasuraFields)
      where
        unconsUpdatingRelPath =
          case relFieldPath of
            -- ...if we've reached the end of the path, associate the hasura field with the remote
            RelFieldPath Seq.Empty -> 
              \fn -> (getFieldNameTxt fn, Left remoteRelKey)
            RelFieldPath ((_, G.Alias (G.Name key)) Seq.:<| xs) -> 
              \_  -> (key,                Right (remoteRelKey, (RelFieldPath xs, hasuraFields)))

data BatchInputs =
  BatchInputs
    { biRows        :: !(Seq.Seq (Map.HashMap G.Variable G.ValueConst))
      -- ^ The results from hasura that are required to make the requests to
      -- satisfy the remote joins. 
      --
      -- Each element of the 'Seq' corresponds to a top-level selection in the
      -- remote query. The map key corresponds to the @hasura_fields@ from the 
      -- @create_remote_relationship@ configuration.
    , biCardinality :: Cardinality
    -- ^ Used for validation in 'insertBatchResults'.
    --
    -- TODO how exactly does this work...?
    } deriving (Show)

-- See 'extractFromResult':
instance Semigroup BatchInputs where
  (<>) (BatchInputs r1 c1) (BatchInputs r2 c2) =
    BatchInputs (r1 <> r2) (c1 <> c2)

data Cardinality = Many | One
 deriving (Eq, Show)

instance Semigroup Cardinality where
    (<>) _ Many  = Many
    (<>) Many _  = Many
    (<>) One One = One


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
    AGEnum _ _enumVal ->
      pure (Left "enum not supported")
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
  PGValUnknown t -> pure $ G.VString $ G.StringValue t

appendJoinError :: GQRespValue -> GQJoinError -> GQRespValue
appendJoinError resp err =
  gqRespErrors <>~ (VB.singleton $ gqJoinErrorToValue err) $ resp
