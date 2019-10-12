{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE ViewPatterns             #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE StandaloneDeriving       #-}

module Hasura.GraphQL.Execute.RemoteJoins
  ( JoinArguments(..)
  , RemoteRelBranch(..), RRF_P(..), rrFieldToSplice
  , GQRespValue(..), gqRespData, gqRespErrors
  , encodeGQRespValue
  , parseGQRespValue
  , extractRemoteRelArguments
  , joinResults
  , emptyResp
  , rebuildFieldStrippingRemoteRels
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
import           Data.Validation
import           Hasura.GraphQL.Validate.Field
import           Hasura.SQL.Types

import qualified Data.Aeson.Ordered                     as OJ
import qualified Data.HashMap.Strict                    as Map
import qualified Data.HashMap.Strict.InsOrd             as OHM
-- Any Map with ordered 'elems' or 'toAscList' will do here:
import qualified Data.IntMap.Strict                     as IntMap
import qualified Data.List.NonEmpty                     as NE
import qualified Data.Sequence                          as Seq
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

-- | When parameterised by 'RRF_Tree' this is like a 'VQ.Field' but with a path
-- indicating where its response should be spliced back into the hasura result
-- (it's a branch in the sense that it's cut off from its trunk).
--
-- When parameterised by 'RRF_Splice' this represents just the path and
-- metadata needed to splice our remote result.
--
-- TODO give this a better name.
data RemoteRelBranch (p :: RRF_P) =
  RemoteRelBranch
    { rrRemoteRelationship :: !RemoteRelationship
    -- ^ The hasura to remote schema field mapping.
    , rrArguments     :: !(Param p ArgsMap)
    -- ^ User-provided arguments. See '_fArguments'.
    , rrSelSet        :: !(Param p SelSet)
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
    , rrPhantomFields :: ![G.Name]
    -- ^ Hasura fields which are not in the selection set, but are required as
    -- parameters to satisfy the remote join.
    }
deriving instance Show (RemoteRelBranch 'RRF_Tree)
deriving instance Show (RemoteRelBranch 'RRF_Splice)

-- Trying out this pattern in a low-stakes setting: this lets us express
-- variations of a data type but with less friction or boilerplate than if the
-- parameter were a 'Functor' directly, say. Concretely e.g. code does not need
-- to unwrap an Identity when accessing rrSelSet.
--
-- Related or complimentary:
--   https://hackage.haskell.org/package/higgledy 
--   https://reasonablypolymorphic.com/blog/higher-kinded-data/ 
--   https://hackage.haskell.org/package/barbies

-- | This could live in "Hasura.Prelude" if we want to do this elsewhere.
type family Param (p :: k) x
-- DataKinds:
data RRF_P = RRF_Tree | RRF_Splice
-- Use the special parameter to determine whether to keep or drop fields (when
-- used just as a splice):
type instance Param 'RRF_Tree   a = a
type instance Param 'RRF_Splice a = () -- or Void? But then we can't derive Show etc.
-- Maybe barbie takes care of some boilerplate here?
rrFieldToSplice :: RemoteRelBranch 'RRF_Tree -> RemoteRelBranch 'RRF_Splice
rrFieldToSplice RemoteRelBranch{..} = RemoteRelBranch{rrArguments = (), rrSelSet = (), ..}


-- | A path of ordered keys (suitable for 'OJ.insert') allowing us to splice a
-- remote result at a nested location.
newtype RelFieldPath =
  RelFieldPath {
    unRelFieldPath :: (Seq.Seq (Int, G.Alias))
  } deriving (Show, Monoid, Semigroup, Eq)

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


-- | Rebuild the query tree, returning a new tree with remote relationships
-- removed, along with a (possibly empty) list of paths that point back to them.
--
-- Output here is eventually assembled into a 'QExecPlan' 'Tree' in
-- 'getExecPlan', and finally executed in 'runGQ'.
rebuildFieldStrippingRemoteRels
  :: VQ.Field 
  -> (VQ.Field, [RemoteRelBranch 'RRF_Tree])
  -- NOTE: the ordering of the _fSelSet fields in the result 'VQ.Field' seems not to matter at 
  -- all for correctness here (based on experimentation)... I haven't puzzled out why yet.
rebuildFieldStrippingRemoteRels =
  flip runState mempty . rebuild 0 mempty
  where
    rebuild :: Int -> RelFieldPath -> Field -> State [RemoteRelBranch 'RRF_Tree] Field
    rebuild idx0 parentPath field0 = do
      forMOf fSelSet field0 $ \ss0 ->
        fmap join $ flip Seq.traverseWithIndex ss0 $
          \idx subfield ->
             case _fSource subfield of
               -- NOTE: I think _fRemoteRel can become RemoteRelationship too.
               TLRemoteRelType remoteRelationship -> do

                 modify (remoteRelField :)
                 -- NOTE: this may result in redundant fields in the SELECT, but this seems fine.
                 -- NOTE: this sub-sequence is in arbitrary order, since coming from Set.
                 pure $ Seq.fromList $
                   map bareNamedField $ toList requiredHasuraFields

                 where remoteRelField =
                         RemoteRelBranch
                           { rrRemoteRelationship = remoteRelationship
                           , rrArguments = _fArguments subfield
                           , rrSelSet = _fSelSet subfield
                           , rrRelFieldPath = thisPath
                           , rrAlias = _fAlias subfield
                           , rrAliasIndex = idx
                           , rrPhantomFields =
                               toList $
                                 requiredHasuraFields `Set.difference` siblingSelSetFields
                           }
                       requiredHasuraFields = 
                         coerceSet $ rtrHasuraFields remoteRelationship
               _ -> Seq.singleton <$> rebuild idx thisPath subfield
      where
        thisPath = parentPath <> RelFieldPath (pure (idx0, _fAlias field0))
        siblingSelSetFields = Set.fromList $ toList $ fmap _fName $ _fSelSet field0


-- | Join the data returned from the original hasura request with the remote values.
joinResults :: GQRespValue -> [(RemoteRelBranch 'RRF_Splice, EncJSON)] -> GQRespValue
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
  -- ^ Original hasura response, and accumulated/spliced remote responses
  -> RemoteRelBranch 'RRF_Splice
  -> GQRespValue
  -- ^ Remote response
  -> GQRespValue
  -- ^ Original hasura response with accumulated remote responses and errors
insertBatchResults accumResp RemoteRelBranch{..} remoteResp =
  -- It's not clear what to do about errors here, or when to short-circuit so
  -- try to do as much computation as possible for now:
  case orderedTopLevelRemoteSelections of
    Nothing -> appendJoinError accumResp $ 
      "did not find expected hasura alias prefex in all remote results: " <>
      fromString (show $ _gqRespData remoteResp)
    Just remoteData0 -> do
      case inHashmap rrRelFieldPath accumData0 remoteData0 of
        Left err -> appendJoinError accumRespWithRemoteErrs err
        Right (val, remainingRemotes) -> 
          let finalRespVal = set gqRespData val accumRespWithRemoteErrs
           in if null remainingRemotes
                 then finalRespVal
                 else appendJoinError finalRespVal $
                        "Some remote fields unexpectedly remain after joining: " <>
                        fromString (show remainingRemotes)
  where
    accumRespWithRemoteErrs = gqRespErrors <>~ _gqRespErrors remoteResp $ accumResp
    accumData0 = _gqRespData accumResp
    -- Re-sort expected aliases to account for naughty remotes that don't
    -- respect the ordering parts of spec. See 'enumerateRowAliases':
    orderedTopLevelRemoteSelections = 
      unEnumerateRowAliases $ OJ.toList $ _gqRespData remoteResp

    inHashmap ::
         RelFieldPath
      -> OJ.Object
      -> [OJ.Value]
      -- ^ Initially: the top-level selection set of remote result, ordered.
      -> Either GQJoinError (OJ.Object, [OJ.Value])
      -- ^ Accumulated joined response JSON, along with remaining remote top-level field responses
    inHashmap (RelFieldPath p) accumData remoteDataVals = case p of
      -- we've reached the end of the path, so expect to splice
      -- NOTE: this is the ONLY base case for mutually-recursive inHashmap/inValue:
      Seq.Empty ->
        case remoteDataVals of
          [] -> Left $ "Expected one or more remote object but got none, while traversing " 
                    <> showHashO accumData
          (remoteDataVal:rest) -> 
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
                  -- Since we've reached the end of the path in an Array, we
                  -- assume this splice has cardinality > 1 (i.e. we map it)
                  foldHasuraRowObjs $ \(hasuraRowsSoFar, remainingRemotes) accumData ->
                     case remainingRemotes of
                       [] ->
                         Left $
                           "no remote objects left for joining at " <> showHashO accumData
                       (remoteDataVal:rest) -> do
                         spliced <- spliceRemote remoteDataVal accumData
                         pure
                           (OJ.Object spliced : hasuraRowsSoFar , rest)

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
      let nestedFields = fmap fcName $ rtrRemoteFields rrRemoteRelationship
      peeledValue <- peelOffNestedFields nestedFields remoteDataVal
      pure $
        -- Strip phantom fields
        foldl' (flip OJ.delete)
                -- TODO Brandon note:
                -- document why we know OJ.insert might not silently clobber an existing field in accumData; e.g. with an assertion here, or directing reader to someplace validation is performed
                -- TODO Brandon note:
                -- should we delete first? how do we know batchRelationshipKeyToMake isn't in batchPhantoms? Document.
               (OJ.insert
                  (rrAliasIndex, coerce rrAlias)
                  peeledValue
                  accumData)
               (map coerce rrPhantomFields)

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


-- | Tag 'joinArguments' list with enumerated aliases we can use to recover the
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

-- | Fold nested 'FieldCall's into a bare 'Field', inserting the passed
-- selection set at the leaf of the tree we construct.
fieldCallsToField ::
     Map.HashMap G.Name AnnInpVal
  -> Map.HashMap G.Variable G.ValueConst
  -> SelSet
  -- ^ Inserted at leaf of nested FieldCalls
  -> G.Alias
  -- ^ Top-level name to set for this Field
  -> NonEmpty FieldCall
  -> Field
fieldCallsToField rrArguments variables finalSelSet topAlias = 
  set fAlias topAlias . nest
  where
    -- almost: `foldr nest finalSelSet`
    nest (FieldCall{..} :| rest) =
      (bareNamedField fcName)
        { _fArguments =
            let templatedArguments =
                  createArguments variables fcArguments
             in case NE.nonEmpty rest of
                  Just {} -> templatedArguments
                  Nothing ->
                    Map.unionWith
                      mergeAnnInpVal
                      rrArguments
                      templatedArguments
        , _fSelSet = maybe finalSelSet (pure . nest) $ NE.nonEmpty rest
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

-- | We use this in 'extractRemoteRelArguments' for bookkeeping while
-- preserving the order of the @[RemoteRelBranch]@ input.
type RemoteRelKey = Int
type RemoteRelKeyMap = IntMap.IntMap

-- | Extract from the Hasura results the remote relationship arguments,
-- returning them with their associated RemoteRelBranch.
extractRemoteRelArguments ::
     EncJSON
  -> [RemoteRelBranch 'RRF_Tree]
  -- ^ NOTE: This function only internally makes use of rrRelFieldPath, and
  -- (rtrHasuraFields . rrRemoteRelationship).
  -> (GQRespValue , [Maybe JoinArguments])
  -- ^ A 'JoinArguments' for each input rel, if we could extract them. If
  -- permissions filtered hasura results, some of these may be Nothing. 
  -- TODO audit this.
  --
  -- The output list will always be the same length as the input @rels@, so
  -- they can be zipped. 
  -- TODO consider refactoring so we can avoid zipping at call site.
  --
  -- Errors are accumulated in GQRespValue.
extractRemoteRelArguments hasuraJson rels =
  case parseGQRespValue hasuraJson of
    Left (fromString-> err) ->
      (appendJoinError emptyResp $ "decode error: " <> err, [])
    Right gqResp@GQRespValue{..} -> do
      let bisE = 
            IntMap.elems . -- N.B. elems in ascending RemoteRelKey order
            reassociateRels
            <$> execStateT (go keyedRemotes0 (OJ.Object _gqRespData)) mempty
      case bisE of
        Left err  -> (appendJoinError gqResp err, [])
        Right bis -> (gqResp, bis)
  where
    reassociateRels joinArgs = 
      (Just <$> joinArgs) `IntMap.union` -- NOTE: left biased
      (Nothing <$ IntMap.fromList (zip [(0::RemoteRelKey) ..] rels))

    keyedRemotes0 = 
      zip [(0::RemoteRelKey) ..] $
        map (rrRelFieldPath &&& rtrHasuraFields . rrRemoteRelationship) rels

    go :: [(RemoteRelKey, (RelFieldPath, Set FieldName))]
       -> OJ.Value
       -> StateT (RemoteRelKeyMap JoinArguments) (Either GQJoinError) ()
    go keyedRemotes hasuraResp =
      case hasuraResp of
        OJ.Array values -> mapM_ (go keyedRemotes) values
        OJ.Object hasuraRespObj -> do
          remotesRows :: RemoteRelKeyMap (Map.HashMap G.Variable G.ValueConst) <-
            foldM
              (\remoteRows (mbKey, remotes) ->
                 case (mbKey, partitionEithers remotes) of
                   (Just key, (remoteRelKeys, unfinishedKeyedRemotes)) -> 
                     case OJ.lookup key hasuraRespObj of
                       Just subvalue -> do
                         go unfinishedKeyedRemotes subvalue
                         pure
                           (foldl'
                              (\remoteRows' remoteRelKey ->
                                  IntMap.insertWith (<>) remoteRelKey 
                                   -- TODO: Pay attention to variable naming wrt. aliasing.
                                   (Map.singleton (coerce key) (valueToValueConst subvalue))
                                   remoteRows'
                              )
                              remoteRows
                              remoteRelKeys)
                       Nothing -> throwError $ fromString $
                         "Expected key " <> T.unpack key <> " at this position: " <> 
                         show (OJ.toEncJSON hasuraResp)

                   -- for remotes not closed over any hasura values, return empty bindings list:
                   (Nothing,  (remoteRelKeys, [])) ->
                      pure $ IntMap.unionWith (<>) remoteRows $
                        IntMap.fromList $ zip remoteRelKeys (repeat mempty)

                   (Nothing, _) -> error "impossible: fix peelRemoteKeys"
              )
              mempty
              (Map.toList $ foldCandidates keyedRemotes)

          modify $ IntMap.unionWith (flip (<>)) $
            JoinArguments . Seq.singleton <$> remotesRows

        -- scalar values are the base case; assert no remotes left:
        -- NOTE: keyedRemotes may be non-empty here when permissions have filtered hasura results:
        OJ.Null -> pure ()
        -- ...otherwise this is an internal error (I think):
        _ -> when (not $ null keyedRemotes) $
               throwError $ fromString $
                 "In extractRemoteRelArguments expected empty keyedRemotes, got: " <> 
                 show keyedRemotes

    -- NOTE: RemoteRelKey is just passed through in these two functions in a
    -- way that's convenient for our `partitionEithers` above:
    foldCandidates
      :: [(remoteRelKey, (RelFieldPath, Set FieldName))]
      -> Map.HashMap (Maybe Text) [Either remoteRelKey (remoteRelKey, (RelFieldPath, Set FieldName))]
      -- ^ The 'Nothing' key is for remotes closed over no fields (the value will be all Lefts)
    foldCandidates =
      foldl' (uncurry . insertWithCons) mempty . concatMap peelRemoteKeys
      where insertWithCons m k v = Map.insertWith (<>) k (pure v) m

    -- Peel one layer of expected keys from the remote to be looked up at the
    -- current level of the result object.
    peelRemoteKeys 
      :: (remoteRelKey, (RelFieldPath, Set FieldName)) 
      -> [(Maybe Text, Either remoteRelKey (remoteRelKey, (RelFieldPath, Set FieldName)))]
    peelRemoteKeys (remoteRelKey, (relFieldPath, hasuraFields)) =
      case relFieldPath of
        -- ...if we've reached the end of the path...
        RelFieldPath Seq.Empty 
          | Set.null hasuraFields -> 
              -- special case for when we're closed over no hasura fields:
              pure (Nothing, Left remoteRelKey)
          | otherwise -> 
              -- ...associate each hasura field with the remote: 
              map (\fn -> (Just $ getFieldNameTxt fn, Left remoteRelKey)) $ toList hasuraFields
        -- ...otherwise peel off path layer and return unfinished keyed remotes:
        RelFieldPath ((_, G.Alias (G.Name key)) Seq.:<| xs) -> 
          pure (Just key, Right (remoteRelKey, (RelFieldPath xs, hasuraFields)))


-- | The results from hasura as variable bindings that are required to make the
-- remote requests to satisfy the remote joins. 
--
-- Each element of the 'Seq' corresponds to an ordered top-level selection in
-- the remote query. The map key corresponds to the @hasura_fields@ from the
-- @create_remote_relationship@ configuration.
newtype JoinArguments = 
  JoinArguments { joinArguments :: Seq.Seq (Map.HashMap G.Variable G.ValueConst) } 
  deriving (Show, Semigroup)



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

appendJoinError :: GQRespValue -> GQJoinError -> GQRespValue
appendJoinError resp err =
  gqRespErrors <>~ (VB.singleton $ gqJoinErrorToValue err) $ resp
