{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}

module Hasura.GraphQL.Execute
  ( QExecPlanResolved(..)
  , QExecPlanUnresolved(..)
  , QExecPlanPartial(..)
  , QExecPlan(..)
  , Batch(..)
  , GQRespValue(..)
  , getExecPlanPartial
  , gqrespValueToValue
  , parseGQRespValue
  , extractRemoteRelArguments
  , produceBatches
  , joinResults
  , getOpTypeFromExecOp

  , ExecOp(..)
  , getExecPlan
  , execRemoteGQ

  , EP.PlanCache
  , EP.initPlanCache
  , EP.clearPlanCache
  , EP.dumpPlanCache

  , ExecutionCtx(..)

  , emptyResp

  , JoinParams(..)
  , Joinable
  , mkQuery
  ) where

import           Control.Exception                      (try)
import           Control.Lens
import           Data.Scientific
import           Data.Validation
import           Hasura.SQL.Types

import           Data.Has
import           Data.List
import           Data.List.NonEmpty                     (NonEmpty (..))
import qualified Data.List.NonEmpty                     as NE
import           Data.Time
import qualified Data.Vector                            as Vec
import           Hasura.GraphQL.Validate.Field
import           Hasura.SQL.Time

import qualified Data.Aeson                             as J
import qualified Data.Aeson.Ordered                     as OJ
import qualified Data.CaseInsensitive                   as CI
import qualified Data.HashMap.Strict                    as Map
import qualified Data.HashMap.Strict.InsOrd             as OHM
import qualified Data.Sequence                          as Seq
import qualified Data.String.Conversions                as CS
import qualified Data.Text                              as T
import qualified Data.Vector                            as V
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Types                     as N
import qualified Network.Wreq                           as Wreq

import           Hasura.EncJSON
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Resolve.Context
import           Hasura.GraphQL.Schema
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.GraphQL.Validate.Types
import           Hasura.HTTP
import           Hasura.Prelude
import           Hasura.RQL.DDL.Headers
import           Hasura.RQL.Types
import           Hasura.Server.Context
import           Hasura.Server.Utils                    (RequestId,
                                                         filterRequestHeaders)
import           Hasura.SQL.Value

import qualified Hasura.GraphQL.Execute.LiveQuery       as EL
import qualified Hasura.GraphQL.Execute.Plan            as EP
import qualified Hasura.GraphQL.Execute.Query           as EQ
import           Hasura.GraphQL.Logging
import qualified Hasura.GraphQL.Resolve                 as GR
import qualified Hasura.GraphQL.Validate                as VQ
import qualified Hasura.Logging                         as L

data GQRespValue =
  GQRespValue
  { gqRespData   :: Maybe OJ.Object
  , gqRespErrors :: Maybe [OJ.Value]
  } deriving (Show, Eq)

parseGQRespValue :: OJ.Value -> Either String GQRespValue
parseGQRespValue =
  \case
    OJ.Object obj -> do
      gqRespData <-
        case OJ.lookup "data" obj of
          Nothing -> pure Nothing
          Just (OJ.Object dobj) -> pure (Just dobj)
          Just _ -> Left "expected object for GraphQL data response"
      gqRespErrors <-
        case OJ.lookup "errors" obj of
          Nothing -> pure Nothing
          Just (OJ.Array vec) -> pure (Just (toList vec))
          Just _ -> Left "expected array for GraphQL error response"
      pure (GQRespValue {gqRespData, gqRespErrors})
    _ -> Left "expected object for GraphQL response"

gqrespValueToValue :: GQRespValue -> OJ.Value
gqrespValueToValue (GQRespValue mdata' merrors) =
  OJ.Object
    (OJ.fromList
       (concat
          [ [("data", OJ.Object data') | Just data' <- [mdata']]
          , [ ("errors", OJ.Array (V.fromList errors))
            | Just errors <- [merrors]
            ]
          ]))

data GQJoinError =  GQJoinError !T.Text
  deriving (Show, Eq)

gQJoinErrorToValue :: GQJoinError -> OJ.Value
gQJoinErrorToValue (GQJoinError msg) =
  OJ.Object (OJ.fromList [("message", OJ.String msg)])

data QExecPlanPartial
  = ExPHasuraPartial !(GCtx, VQ.HasuraTopField, [G.VariableDefinition])
  | ExPRemotePartial !VQ.RemoteTopField
--
-- The 'a' is parameterised so this AST can represent
-- intermediate passes
data QExecPlanCore a
  = ExPCoreHasura !a
  | ExPCoreRemote !RemoteSchemaInfo !G.TypedOperationDefinition
  deriving (Functor, Foldable, Traversable)

-- The current execution plan of a graphql operation, it is
-- currently, either local pg execution or a remote execution
data QExecPlanResolved
  = ExPHasura !ExecOp
  | ExPRemote !VQ.RemoteTopField
  -- | ExPMixed !ExecOp (NonEmpty RemoteRelField)

class Joinable a where
  mkQuery :: JoinParams -> a -> (Batch, QExecPlanResolved)

instance Joinable QExecPlanUnresolved where
  mkQuery joinParams unresolvedPlan
    = let batch = produceBatch opType remoteSchemaInfo remoteRelField rows
          batchQuery = batchRemoteTopQuery batch
      in (batch, ExPRemote batchQuery)
    where
      JoinParams opType rows = joinParams
      QExecPlanUnresolved remoteRelField remoteSchemaInfo = unresolvedPlan

data QExecPlanUnresolved = QExecPlanUnresolved RemoteRelField RemoteSchemaInfo

-- data JoinConfiguration = JoinConfiguration

data JoinParams = JoinParams G.OperationType BatchInputs

data QExecPlan = Leaf QExecPlanResolved | Tree QExecPlanResolved (NonEmpty QExecPlanUnresolved)

-- | Execution context
data ExecutionCtx
  = ExecutionCtx
  { _ecxLogger          :: !L.Logger
  , _ecxSqlGenCtx       :: !SQLGenCtx
  , _ecxPgExecCtx       :: !PGExecCtx
  , _ecxPlanCache       :: !EP.PlanCache
  , _ecxSchemaCache     :: !SchemaCache
  , _ecxSchemaCacheVer  :: !SchemaCacheVer
  , _ecxHttpManager     :: !HTTP.Manager
  , _ecxEnableAllowList :: !Bool
  }

newtype RemoteRelKey =
  RemoteRelKey Int
  deriving (Eq, Ord, Show, Hashable)

data RemoteRelField =
  RemoteRelField
    { rrRemoteField   :: !RemoteField
    , rrField         :: !Field
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

getExecPlanPartial
  :: (MonadError QErr m)
  => UserInfo
  -> SchemaCache
  -> Bool
  -> GQLReqParsed
  -> m (Seq.Seq QExecPlanPartial)
getExecPlanPartial userInfo sc enableAL req = do

  -- check if query is in allowlist
  when enableAL checkQueryInAllowlist

  (gCtx, _)  <- flip runStateT sc $ getGCtx role gCtxRoleMap
  queryParts <- flip runReaderT gCtx $ VQ.getQueryParts req

  topFields <- runReaderT (VQ.validateGQ queryParts) gCtx
  let varDefs = G._todVariableDefinitions $ VQ.qpOpDef queryParts
  return $
    fmap
      (\case
          VQ.HasuraLocatedTopField hasuraTopField ->
            ExPHasuraPartial (gCtx, hasuraTopField, varDefs)
          VQ.RemoteLocatedTopField remoteTopField -> ExPRemotePartial remoteTopField)
      topFields
  where
    role = userRole userInfo
    gCtxRoleMap = scGCtxMap sc

    checkQueryInAllowlist =
      -- only for non-admin roles
      when (role /= adminRole) $ do
        let notInAllowlist =
              not $ VQ.isQueryInAllowlist (_grQuery req) (scAllowlist sc)
        when notInAllowlist $ modifyQErr modErr $ throwVE "query is not allowed"

    modErr e =
      let msg = "query is not in any of the allowlists"
      in e{qeInternal = Just $ J.object [ "message" J..= J.String msg]}


-- An execution operation, in case of
-- queries and mutations it is just a transaction
-- to be executed
data ExecOp
  = ExOpQuery !LazyRespTx !(Maybe EQ.GeneratedSqlMap)
  | ExOpMutation !LazyRespTx
  | ExOpSubs !EL.LiveQueryOp

getOpTypeFromExecOp :: ExecOp -> G.OperationType
getOpTypeFromExecOp = \case
  ExOpQuery _ _ -> G.OperationTypeQuery
  ExOpMutation _ -> G.OperationTypeMutation
  ExOpSubs _ -> G.OperationTypeSubscription

getExecPlan
  :: (MonadError QErr m, MonadIO m)
  => PGExecCtx
  -> EP.PlanCache
  -> UserInfo
  -> SQLGenCtx
  -> Bool
  -> SchemaCache
  -> SchemaCacheVer
  -> GQLReqUnparsed
  -> m (Seq.Seq QExecPlan)
getExecPlan pgExecCtx planCache userInfo sqlGenCtx enableAL sc scVer reqUnparsed = do
  planM <-
    liftIO $ EP.getPlan scVer (userRole userInfo) opNameM queryStr planCache
  let usrVars = userVars userInfo
  case planM
    -- plans are only for queries and subscriptions
        of
    Just plan ->
      -- pure $ pure $ Leaf (ExPHasura <$>
      case plan of
        EP.RPQuery queryPlan -> do
          (tx, genSql) <- EQ.queryOpFromPlan usrVars queryVars queryPlan
          pure $ Seq.singleton (Leaf (ExPHasura (ExOpQuery tx (Just genSql))))
        EP.RPSubs subsPlan -> do
          liveQueryOp <- EL.subsOpFromPlan pgExecCtx usrVars queryVars subsPlan
          pure $ Seq.singleton (Leaf (ExPHasura (ExOpSubs liveQueryOp)))
    Nothing -> noExistingPlan
  where
    GQLReq opNameM queryStr queryVars = reqUnparsed
    addPlanToCache plan =
      -- liftIO $
      EP.addPlan scVer (userRole userInfo) opNameM queryStr plan planCache
    noExistingPlan = do
      req <- toParsed reqUnparsed
      partialExecPlans <- getExecPlanPartial userInfo sc enableAL req
      forM partialExecPlans $ \partialExecPlan ->
        case partialExecPlan of
          ExPRemotePartial r -> pure (Leaf $ ExPRemote r)
          ExPHasuraPartial (gCtx, rootSelSet, varDefs) -> do
            case rootSelSet of
              VQ.HasuraTopMutation field ->
                Leaf . ExPHasura . ExOpMutation <$>
                getMutOp gCtx sqlGenCtx userInfo (pure field)
              VQ.HasuraTopQuery originalField -> do
                case rebuildFieldStrippingRemoteRels originalField of
                  Nothing -> do
                    (queryTx, _planM, genSql) <- getQueryOp gCtx sqlGenCtx userInfo (pure originalField) varDefs

                    -- TODO: How to cache query for each field?
                    -- mapM_ (addPlanToCache . EP.RPQuery) planM
                    pure $ Leaf . ExPHasura $ ExOpQuery queryTx (Just genSql)
                  Just (newHasuraField, remoteRelFields) -> do
                    -- trace
                    --   (unlines
                    --      [ "originalField = " ++ show originalField
                    --      , "newField = " ++ show newField
                    --      , "cursors = " ++ show (fmap rrRelFieldPath cursors)
                    --      ])
                    (queryTx, _planM, genSql) <- getQueryOp gCtx sqlGenCtx userInfo (pure newHasuraField) varDefs
                    let unresolvedPlans = mkUnresolvedPlans remoteRelFields
                    pure $ Tree (ExPHasura $ ExOpQuery queryTx (Just genSql)) unresolvedPlans
              VQ.HasuraTopSubscription fld -> do
                (lqOp, _planM) <-
                  getSubsOp
                    pgExecCtx
                    gCtx
                    sqlGenCtx
                    userInfo
                    reqUnparsed
                    varDefs
                    fld

                -- TODO: How to cache query for each field?
                -- mapM_ (addPlanToCache . EP.RPSubs) planM
                pure $ Leaf . ExPHasura $ ExOpSubs lqOp

    mkUnresolvedPlans :: NonEmpty RemoteRelField -> NonEmpty QExecPlanUnresolved
    mkUnresolvedPlans = fmap (\remoteRelField -> QExecPlanUnresolved remoteRelField  (getRsi remoteRelField))
      where
        getRsi remoteRel =
          case Map.lookup
                 (rtrRemoteSchema
                    (rmfRemoteRelationship
                       (rrRemoteField remoteRel)))
                 (scRemoteSchemas sc) of
            Just remoteSchemaCtx -> rscInfo remoteSchemaCtx

-- Rebuild the field with remote relationships removed, and paths that
-- point back to them.
rebuildFieldStrippingRemoteRels ::
     VQ.Field -> Maybe (VQ.Field, NonEmpty RemoteRelField)
rebuildFieldStrippingRemoteRels =
  extract . flip runState mempty . rebuild 0 mempty
  where
    extract (field, remoteRelFields) =
      fmap (field, ) (NE.nonEmpty remoteRelFields)
    rebuild idx0 parentPath field0 = do
      selSetEithers <-
        traverse
          (\(idx, subfield) ->
             case _fRemoteRel subfield of
               Nothing -> fmap Right (rebuild idx thisPath subfield)
               Just remoteField -> do
                 modify (remoteRelField :)
                 pure (Left remoteField)
                 where remoteRelField =
                         RemoteRelField
                           { rrRemoteField = remoteField
                           , rrField = subfield
                           , rrRelFieldPath = thisPath
                           , rrAlias = _fAlias subfield
                           , rrAliasIndex = idx
                           , rrPhantomFields =
                               map
                                 G.unName
                                 (filter
                                    (\name ->
                                       notElem
                                         name
                                         (map _fName (toList (_fSelSet field0))))
                                    (map
                                       (G.Name . getFieldNameTxt)
                                       (toList
                                          (rtrHasuraFields
                                             (rmfRemoteRelationship remoteField)))))
                           })
          (zip [0..] (toList (_fSelSet field0)))
      let fields = rights selSetEithers
      pure
        field0
          { _fSelSet =
              Seq.fromList
                (concatMap
                   (\case
                      Right field -> pure field
                        where _ = _fAlias field
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
                          (map
                             (G.Name . getFieldNameTxt)
                             (toList
                                (rtrHasuraFields
                                   (rmfRemoteRelationship remoteField)))))
                   (toList selSetEithers))
          }
      where
        thisPath = parentPath <> RelFieldPath (pure (idx0, _fAlias field0))

-- | Get a list of fields needed from a hasura result.
neededHasuraFields
  :: RemoteField -> [FieldName]
neededHasuraFields remoteField = toList (rtrHasuraFields remoteRelationship)
  where
    remoteRelationship = rmfRemoteRelationship remoteField

-- remote result = {"data":{"result_0":{"name":"alice"},"result_1":{"name":"bob"},"result_2":{"name":"alice"}}}

-- | Join the data from the original hasura with the remote values.
joinResults :: [(Batch, EncJSON)]
            -> GQRespValue
            -> GQRespValue
joinResults paths hasuraValue0 =
  let remoteValues :: [(Batch, GQRespValue)] =
        map
          (\(batch, encJson) ->
             case OJ.eitherDecode (encJToLBS encJson) >>= parseGQRespValue of
               Left err ->
                 ( batch
                 , appendJoinError
                     emptyResp
                     (GQJoinError ("joinResults: eitherDecode: " <> T.pack err)))
               Right gqResp@(GQRespValue mdata _merrors) ->
                 case mdata of
                   Nothing ->
                     ( batch
                     , appendJoinError
                         gqResp
                         (GQJoinError ("could not find join key")))
                   Just _ -> (batch, gqResp))
          paths
   in foldl
        (\resp (batch, remoteResp) ->
           insertBatchResults
             remoteResp
             batch
             resp
        )
        hasuraValue0
        remoteValues

emptyResp :: GQRespValue
emptyResp = GQRespValue Nothing Nothing

-- | Insert at path, index the value in the larger structure.
insertBatchResults ::
     GQRespValue
  -> Batch
  -> GQRespValue
  -> GQRespValue
insertBatchResults remoteValue batch hasuraResp =
  case inHashmap (batchRelFieldPath batch) hasuraHash0 remoteHash0 of
    Left err  -> appendJoinError hasuraResp (GQJoinError (T.pack err))
    Right val -> GQRespValue (Just (fst val)) (gqRespErrors hasuraResp)
  where
    hasuraHash0 = fromMaybe OJ.empty (gqRespData hasuraResp)
    -- The 'sortOn' below is not strictly necessary by the spec, but
    -- implementations may not guarantee order of results matching
    -- order of query.
    remoteHash0 =
      fromMaybe mempty $ fmap (sortOn fst . OJ.toList) (gqRespData remoteValue)
    cardinality = biCardinality (batchInputs batch)
    inHashmap ::
         RelFieldPath
      -> OJ.Object
      -> [(Text, OJ.Value)]
      -> Either String (OJ.Object, [(Text, OJ.Value)])
    inHashmap (RelFieldPath Seq.Empty) hasuraHash remoteHash =
      case cardinality of
        One ->
          case remoteHash of
            [] ->
              Left
                ("Expected one remote object but got none, while traversing " <>
                 show (OJ.toEncJSON (OJ.Object hasuraHash)))
            [theRemoteHash] ->
              Right
                ( (foldl'
                     (flip OJ.delete)
                     (OJ.insert
                        ( batchRelationshipKeyIndex batch
                        , batchRelationshipKeyToMake batch)
                        (peelOffNestedFields
                           (batchNestedFields batch)
                           (snd $ theRemoteHash))
                        hasuraHash)
                     (batchPhantoms batch))
                , mempty)
            _ ->
              Left
                ("Expected one remote object but got many, while traversing " <>
                 show (OJ.toEncJSON (OJ.Object hasuraHash)))
        Many ->
          case remoteHash of
            [] ->
              Left
                ("Expected many objects but got none, while traversing " <>
                 show (OJ.toEncJSON (OJ.Object hasuraHash)))
            (theRemoteHash:remainingRemoteHashes) ->
              Right
                ( (foldl'
                     (flip OJ.delete)
                     (OJ.insert
                        ( batchRelationshipKeyIndex batch
                        , batchRelationshipKeyToMake batch)
                        (peelOffNestedFields
                           (batchNestedFields batch)
                           (snd (theRemoteHash)))
                        hasuraHash)
                     (batchPhantoms batch))
                , remainingRemoteHashes)
    inHashmap (RelFieldPath ((idx, G.Alias (G.Name key)) Seq.:<| rest)) hasuraHash remoteHash =
      case OJ.lookup key hasuraHash of
        Nothing ->
          Left
            ("Couldn't find expected key " <> show key <> " in " <>
             show (OJ.toEncJSON (OJ.Object hasuraHash)) <>
             ", while traversing " <>
             show (OJ.toEncJSON (OJ.Object hasuraHash0)))
        Just currentValue ->
          fmap
            (\(newValue, remoteCandidates) ->
               (OJ.insert (idx, key) newValue hasuraHash, remoteCandidates))
            (inValue rest currentValue remoteHash)
    inValue ::
         Seq.Seq (Int, G.Alias)
      -> OJ.Value
      -> [(Text, OJ.Value)]
      -> Either String (OJ.Value, [(Text, OJ.Value)])
    inValue path currentValue remoteHash =
      case currentValue of
        OJ.Object hasuraRowHash ->
          fmap
            (\(hasuraRowHash', remainingRemotes) ->
               (OJ.Object hasuraRowHash', remainingRemotes))
            (inHashmap (RelFieldPath path) hasuraRowHash remoteHash)
        OJ.Array values ->
          case path of
            Seq.Empty ->
              case cardinality of
                Many -> do
                  (hasuraArray, remainingRemotes) <-
                    (foldl
                       (\eitherRows hasuraRowValue ->
                          case eitherRows of
                            Left err -> Left err
                            Right (hasuraRowsSoFar, remainingRemotes) ->
                              case hasuraRowValue of
                                OJ.Object hasuraRowHash ->
                                  case remainingRemotes of
                                    [] ->
                                      Left
                                        ("no remote objects left for joining at " <>
                                         show (OJ.toEncJSON hasuraRowValue))
                                    (x:xs) -> do
                                      let peeledRemoteValue =
                                            peelOffNestedFields
                                              (batchNestedFields batch)
                                              (snd x)
                                          phantoms = batchPhantoms batch
                                      pure
                                        ( hasuraRowsSoFar <>
                                          [ (OJ.Object
                                               (foldl'
                                                  (flip OJ.delete)
                                                  (OJ.insert
                                                     ( batchRelationshipKeyIndex
                                                         batch
                                                     , batchRelationshipKeyToMake
                                                         batch)
                                                     peeledRemoteValue
                                                     hasuraRowHash)
                                                  phantoms))
                                          ]
                                        , xs)
                                _ -> Left "expected object here")
                       (pure ([], remoteHash))
                       (toList values))
                  pure (OJ.Array (Vec.fromList hasuraArray), remainingRemotes)
                One ->
                  Left
                    "Cardinality mismatch: found array in hasura value, but expected object"
            nonEmptyPath -> do
              (hasuraArray, remainingCandidates) <-
                (foldl
                   (\eitherRows hasuraRowValue ->
                      case eitherRows of
                        Left err -> Left err
                        Right (hasuraRowsSoFar, remaining) ->
                          case hasuraRowValue of
                            OJ.Object object -> do
                              (hasuraRowHash, remainder) <-
                                inHashmap
                                  (RelFieldPath nonEmptyPath)
                                  object
                                  remaining
                              pure
                                ( hasuraRowsSoFar <> [OJ.Object hasuraRowHash]
                                , remainder)
                            _ ->
                              Left
                                ("expected array of objects in " <>
                                 show (OJ.toEncJSON hasuraRowValue)))
                   (pure ([], remoteHash))
                   (toList values))
              pure (OJ.Array (Vec.fromList hasuraArray), remainingCandidates)
        OJ.Null -> pure (OJ.Null, remoteHash)
        _ ->
          Left
            ("Expected object or array in hasura value but got: " <>
             show (OJ.toEncJSON currentValue))

-- | The drop 1 in here is dropping the first level of nesting. The
-- top field is already aliased to e.g. foo_idx_1, and that layer is
-- already peeled off. So here we are just peeling nested fields.
peelOffNestedFields :: NonEmpty G.Name -> OJ.Value -> OJ.Value
peelOffNestedFields xs toplevel = go (drop 1 (toList xs)) toplevel
  where
    go [] value = value
    go (G.Name key:rest) value =
      case value of
        OJ.Object hashmap ->
          case OJ.lookup key hashmap of
            -- TODO: Return proper error
            Nothing     -> OJ.Null
              -- Left $ GQJoinError
              --   (T.pack ("No " <> show key <> " in " <> show value <> " from " <>
              --    show toplevel <>
              --    " with " <>
              --    show xs))
            Just value' -> go rest value'
        _ -> OJ.Null
            -- TODO: Return proper error
          -- Left $ GQJoinError
          --   (T.pack ("No! " <> show key <> " in " <> show value <> " from " <>
          --    show toplevel <>
          --    " with " <>
          --    show xs))

-- | Produce the set of remote relationship batch requests.
produceBatches ::
     G.OperationType
  -> [( RemoteRelField
    , RemoteSchemaInfo
    , BatchInputs)]
  -> [Batch]
produceBatches opType =
  fmap
    (\(remoteRelField, remoteSchemaInfo, rows) ->
       produceBatch opType remoteSchemaInfo remoteRelField rows)

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
produceBatch opType remoteSchemaInfo remoteRelField inputs =
  Batch
    { batchRemoteTopQuery = remoteTopQuery
    , batchRelFieldPath = path
    , batchIndices = resultIndexes
    , batchRelationshipKeyToMake = G.unName (G.unAlias (rrAlias remoteRelField))
    , batchRelationshipKeyIndex = rrAliasIndex remoteRelField
    , batchInputs = inputs
    , batchPhantoms = rrPhantomFields remoteRelField
    , batchNestedFields =
        fmap
          fcName
          (rtrRemoteFields
             (rmfRemoteRelationship (rrRemoteField remoteRelField)))
    }
  where
    remoteTopQuery =
      VQ.RemoteTopField
        { rtqRemoteSchemaInfo = remoteSchemaInfo
        , rtqFields =
            fmap
              (\(i, variables) ->
                 fieldCallsToField
                   (Just (arrayIndexAlias i))
                   (_fArguments originalField)
                   variables
                   (_fSelSet originalField)
                   (rtrRemoteFields remoteRelationship))
              indexedRows
         , rtqOperationType = opType
        }
    indexedRows = zip (map ArrayIndex [0 :: Int ..]) (toList rows)
    rows = biRows inputs
    resultIndexes = map fst indexedRows
    remoteRelationship = rmfRemoteRelationship (rrRemoteField remoteRelField)
    path = rrRelFieldPath remoteRelField
    originalField = rrField remoteRelField

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
     Maybe G.Alias
  -> Map.HashMap G.Name AnnInpVal
  -> Map.HashMap G.Variable G.ValueConst
  -> SelSet
  -> NonEmpty FieldCall
  -> Field
fieldCallsToField mindexedAlias0 userProvidedArguments variables finalSelSet =
  nest mindexedAlias0
  where
    nest mindexedAlias (fieldCall :| rest) =
      Field
        { _fAlias =
            case mindexedAlias of
              Just indexedAlias -> indexedAlias
              Nothing           -> G.Alias (fcName fieldCall)
        , _fName = fcName fieldCall
        , _fType = G.NamedType (G.Name "unknown_type")
        , _fArguments =
            let templatedArguments =
                  createArguments variables (fcArguments fieldCall)
             in case NE.nonEmpty rest of
                  Just {} -> templatedArguments
                  Nothing ->
                    Map.unionWith
                      mergeAnnInpVal
                      userProvidedArguments
                      templatedArguments
        , _fSelSet =
            case NE.nonEmpty rest of
              Nothing    -> finalSelSet
              Just calls -> pure (nest Nothing calls)
        , _fRemoteRel = Nothing
        }

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
    (\xs -> Map.fromList (map (\(G.ObjectFieldG key val) -> (key, valueConstToAnnInpVal val)) xs))
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
     RemoteSchemaMap
  -> EncJSON
  -> [RemoteRelField]
  -> Either GQRespValue ( GQRespValue , [ BatchInputs ])
extractRemoteRelArguments remoteSchemaMap hasuraJson rels =
  case OJ.eitherDecode (encJToLBS hasuraJson) >>= parseGQRespValue of
    Left err ->
      Left $
      appendJoinError emptyResp (GQJoinError ("decode error: " <> T.pack err))
    Right gqResp@(GQRespValue mdata _merrors) ->
      case mdata of
        Nothing ->
          Left $
          appendJoinError
            gqResp
            (GQJoinError ("no hasura data to extract for join"))
        Just value -> do
          let hashE =
                execStateT
                  (extractFromResult One keyedRemotes (OJ.Object value))
                  mempty
          case hashE of
            Left err -> Left $ appendJoinError gqResp err
            Right hash -> do
              remotes <-
                Map.traverseWithKey
                  (\key rows ->
                     case Map.lookup key keyedMap of
                       Nothing ->
                         Left $
                         appendJoinError
                           gqResp
                           (GQJoinError
                              "failed to associate remote key with remote")
                       Just remoteRel ->
                         case Map.lookup
                                (rtrRemoteSchema
                                   (rmfRemoteRelationship
                                      (rrRemoteField remoteRel)))
                                remoteSchemaMap of
                           Just remoteSchemaCtx ->
                             pure (remoteRel, (rscInfo remoteSchemaCtx), rows)
                           Nothing ->
                             Left $
                             appendJoinError
                               gqResp
                               (GQJoinError "could not find remote schema info"))
                  hash
              pure (gqResp, map (\(_, _, batchInputs ) -> batchInputs) $ Map.elems remotes)
  where
    keyedRemotes = zip (fmap RemoteRelKey [0 ..]) rels
    keyedMap = Map.fromList (toList keyedRemotes)

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
                      GQJoinError
                        ("Expected key " <> key <> " at this position: " <>
                         (T.pack (show (OJ.toEncJSON value))))))
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
        (toList (fmap peelRemoteKeys keyedRemotes))

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

-- Monad for resolving a hasura query/mutation
type E m =
  ReaderT ( UserInfo
          , OpCtxMap
          , TypeMap
          , FieldMap
          , OrdByCtx
          , InsCtxMap
          , SQLGenCtx
          ) (ExceptT QErr m)

runE
  :: (MonadError QErr m)
  => GCtx
  -> SQLGenCtx
  -> UserInfo
  -> E m a
  -> m a
runE ctx sqlGenCtx userInfo action = do
  res <- runExceptT $ runReaderT action
    (userInfo, opCtxMap, typeMap, fldMap, ordByCtx, insCtxMap, sqlGenCtx)
  either throwError return res
  where
    opCtxMap = _gOpCtxMap ctx
    typeMap = _gTypes ctx
    fldMap = _gFields ctx
    ordByCtx = _gOrdByCtx ctx
    insCtxMap = _gInsCtxMap ctx

getQueryOp
  :: (MonadError QErr m)
  => GCtx
  -> SQLGenCtx
  -> UserInfo
  -> VQ.SelSet
  -> [G.VariableDefinition]
  -> m (LazyRespTx, Maybe EQ.ReusableQueryPlan, EQ.GeneratedSqlMap)
getQueryOp gCtx sqlGenCtx userInfo fields varDefs =
  runE gCtx sqlGenCtx userInfo $ EQ.convertQuerySelSet varDefs fields

mutationRootName :: Text
mutationRootName = "mutation_root"

resolveMutSelSet
  :: ( MonadError QErr m
     , MonadReader r m
     , Has UserInfo r
     , Has OpCtxMap r
     , Has FieldMap r
     , Has OrdByCtx r
     , Has SQLGenCtx r
     , Has InsCtxMap r
     )
  => VQ.SelSet
  -> m LazyRespTx
resolveMutSelSet fields = do
  aliasedTxs <- forM (toList fields) $ \fld -> do
    fldRespTx <- case VQ._fName fld of
      "__typename" -> return $ return $ encJFromJValue mutationRootName
      _            -> liftTx <$> GR.mutFldToTx fld
    return (G.unName $ G.unAlias $ VQ._fAlias fld, fldRespTx)

  -- combines all transactions into a single transaction
  return $ toSingleTx aliasedTxs
  where
    -- A list of aliased transactions for eg
    -- [("f1", Tx r1), ("f2", Tx r2)]
    -- are converted into a single transaction as follows
    -- Tx {"f1": r1, "f2": r2}
    toSingleTx :: [(Text, LazyRespTx)] -> LazyRespTx
    toSingleTx aliasedTxs =
      fmap encJFromAssocList $
      forM aliasedTxs $ \(al, tx) -> (,) al <$> tx

getMutOp
  :: (MonadError QErr m)
  => GCtx
  -> SQLGenCtx
  -> UserInfo
  -> VQ.SelSet
  -> m LazyRespTx
getMutOp ctx sqlGenCtx userInfo selSet =
  runE ctx sqlGenCtx userInfo $ resolveMutSelSet selSet

getSubsOpM
  :: ( MonadError QErr m
     , MonadReader r m
     , Has OpCtxMap r
     , Has FieldMap r
     , Has OrdByCtx r
     , Has SQLGenCtx r
     , Has UserInfo r
     , MonadIO m
     )
  => PGExecCtx
  -> GQLReqUnparsed
  -> [G.VariableDefinition]
  -> VQ.Field
  -> m (EL.LiveQueryOp, Maybe EL.SubsPlan)
getSubsOpM pgExecCtx req varDefs fld =
  case VQ._fName fld of
    "__typename" ->
      throwVE "you cannot create a subscription on '__typename' field"
    _            -> do
      astUnresolved <- GR.queryFldToPGAST fld
      EL.subsOpFromPGAST pgExecCtx req varDefs (VQ._fAlias fld, astUnresolved)

getSubsOp
  :: ( MonadError QErr m
     , MonadIO m
     )
  => PGExecCtx
  -> GCtx
  -> SQLGenCtx
  -> UserInfo
  -> GQLReqUnparsed
  -> [G.VariableDefinition]
  -> VQ.Field
  -> m (EL.LiveQueryOp, Maybe EL.SubsPlan)
getSubsOp pgExecCtx gCtx sqlGenCtx userInfo req varDefs fld =
  runE gCtx sqlGenCtx userInfo $ getSubsOpM pgExecCtx req varDefs fld

execRemoteGQ
  :: ( MonadIO m
     , MonadError QErr m
     , MonadReader ExecutionCtx m
     )
  => RequestId
  -> UserInfo
  -> [N.Header]
  -> G.OperationType -- This should come from Field
  -> RemoteSchemaInfo
  -> Either GQLReqUnparsed [Field]
  -> m (HttpResponse EncJSON)
execRemoteGQ reqId userInfo reqHdrs opType rsi bsOrField = do
  execCtx <- ask
  let logger = _ecxLogger execCtx
      manager = _ecxHttpManager execCtx
  hdrs <- getHeadersFromConf hdrConf
  let confHdrs = map (\(k, v) -> (CI.mk $ CS.cs k, CS.cs v)) hdrs
      clientHdrs = bool [] filteredHeaders fwdClientHdrs
      -- filter out duplicate headers
      -- priority: conf headers > resolved userinfo vars > client headers
      hdrMaps =
        [ Map.fromList confHdrs
        , Map.fromList userInfoToHdrs
        , Map.fromList clientHdrs
        ]
      finalHdrs = foldr Map.union Map.empty hdrMaps
      options = wreqOptions manager (Map.toList finalHdrs)
  jsonbytes <-
    case bsOrField of
      Right field -> do
        gqlReq <- fieldsToRequest opType field
        let jsonbytes = encJToLBS (encJFromJValue gqlReq)
        pure jsonbytes
      Left unparsedQuery -> do
        liftIO $ logGraphqlQuery logger $ QueryLog unparsedQuery Nothing reqId
        pure (J.encode $ J.toJSON unparsedQuery)
  res <- liftIO $ try $ Wreq.postWith options (show url) jsonbytes
  resp <- either httpThrow return res
  let cookieHdr = getCookieHdr (resp ^? Wreq.responseHeader "Set-Cookie")
      respHdrs = Just $ mkRespHeaders cookieHdr
  return $ HttpResponse (encJFromLBS $ resp ^. Wreq.responseBody) respHdrs
  where
    (RemoteSchemaInfo url hdrConf fwdClientHdrs) = rsi
    httpThrow :: (MonadError QErr m) => HTTP.HttpException -> m a
    httpThrow err = throw500 $ T.pack . show $ err
    userInfoToHdrs =
      map (\(k, v) -> (CI.mk $ CS.cs k, CS.cs v)) $ userInfoToList userInfo
    filteredHeaders = filterUserVars $ filterRequestHeaders reqHdrs
    filterUserVars hdrs =
      let txHdrs = map (\(n, v) -> (bsToTxt $ CI.original n, bsToTxt v)) hdrs
       in map (\(k, v) -> (CI.mk $ CS.cs k, CS.cs v)) $
          filter (not . isUserVar . fst) txHdrs
    getCookieHdr = maybe [] (\h -> [("Set-Cookie", h)])
    mkRespHeaders hdrs =
      map (\(k, v) -> Header (bsToTxt $ CI.original k, bsToTxt v)) hdrs

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
fieldToField field = do
  args <- traverse makeArgument (Map.toList (VQ._fArguments field))
  selections <- traverse fieldToField (VQ._fSelSet field)
  pure $ G.Field
    { _fAlias = Just (VQ._fAlias field)
    , _fName = VQ._fName field
    , _fArguments = args
    , _fDirectives = []
    , _fSelectionSet = fmap G.SelectionField (toList selections)
    }

makeArgument :: (G.Name, AnnInpVal) -> Either Text G.Argument
makeArgument (gname, annInpVal) =
  do v <- annInpValToValue annInpVal
     pure $ G.Argument {_aName = gname, _aValue = v}

annInpValToValue :: AnnInpVal -> Either Text G.Value
annInpValToValue = annGValueToValue . _aivValue

annGValueToValue :: AnnGValue -> Either Text G.Value
annGValueToValue =
  \case
    AGScalar _ty mv ->
      case mv of
        Nothing -> pure G.VNull
        Just pg -> pgcolvalueToGValue pg
    AGEnum _ mval ->
      case mval of
        Nothing        -> pure G.VNull
        Just enumValue -> pure (G.VEnum enumValue)
    AGObject _ mobj ->
      case mobj of
        Nothing -> pure G.VNull
        Just obj -> do
          fields <-
            traverse
              (\(k, av) -> do
                 v <- annInpValToValue av
                 pure (G.ObjectFieldG {_ofName = k, _ofValue = v}))
              (OHM.toList obj)
          pure (G.VObject (G.ObjectValueG fields))
    AGArray _ mvs ->
      case mvs of
        Nothing -> pure G.VNull
        Just vs -> G.VList . G.ListValueG <$> traverse annInpValToValue vs

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
  resp
    { gqRespData = gqRespData resp
    , gqRespErrors =
        pure (gQJoinErrorToValue err : fromMaybe mempty (gqRespErrors resp))
    }


-- getValueAtPath :: J.Object -> [Text] -> Either String J.Value
-- getValueAtPath obj [] = return (J.Object obj)
-- getValueAtPath obj [x] = maybe (Left ("could not find any value at path: " <> T.unpack x)) pure (Map.lookup x obj)
-- getValueAtPath obj (x:xs) = do
--   val <- getValueAtPath obj [x]
--   valObj <- assertObject val
--   getValueAtPath valObj xs

-- setValueAtPathAndRemovePhantoms :: J.Object -> [Text] -> (Text, J.Value) -> [Text] -> Either String J.Object
-- setValueAtPathAndRemovePhantoms obj path (k, newVal) phantoms =
--   case path of
--     [] -> pure $ foldl (flip Map.delete) (Map.insert k newVal obj) phantoms
--     (x:xs) -> do
--       val <- getValueAtPath obj [x]
--       valObj <- assertObject val
--       finalObj <- setValueAtPathAndRemovePhantoms valObj xs (k, newVal) phantoms
--       setValueAtPathAndRemovePhantoms obj [] (x, J.Object finalObj) []

-- assertObject :: J.Value -> Either String J.Object
-- assertObject val = case val of
--   J.Object obj -> pure obj
--   _            -> Left "could not parse as JSON object"

-- assertArray :: J.Value -> Either String J.Array
-- assertArray val = case val of
--   J.Array arr -> pure arr
--   _           -> Left "could not parse as JSON array"
