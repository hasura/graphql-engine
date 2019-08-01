module Hasura.GraphQL.Execute.LiveQuery
  ( RefetchInterval
  , refetchIntervalFromMilli
  , LQM.BatchSize
  , LQM.mkBatchSize
  , LQM.MxOpts
  , LQM.mkMxOpts
  , LQF.FallbackOpts
  , LQF.mkFallbackOpts
  , LQOpts
  , mkLQOpts

  , LiveQueriesState
  , initLiveQueriesState
  , dumpLiveQueriesState

  , LiveQueryOpG(..)

  , LiveQueryOp
  , LiveQueryOpPartial
  , getLiveQueryOpPartial
  , LiveQueryId
  , addLiveQuery
  , removeLiveQuery

  , SubsPlan
  , subsOpFromPlan
  , subsOpFromPGAST

  ) where

import           Data.Has

import qualified Control.Concurrent.STM                       as STM
import qualified Data.Aeson                                   as J
import qualified Data.HashMap.Strict                          as Map
import qualified Data.HashSet                                 as Set
import qualified Database.PG.Query                            as Q
import qualified Database.PG.Query.Connection                 as Q
import qualified Language.GraphQL.Draft.Syntax                as G

import qualified Hasura.GraphQL.Execute.LiveQuery.Fallback    as LQF
import qualified Hasura.GraphQL.Execute.LiveQuery.Multiplexed as LQM
import qualified Hasura.GraphQL.Resolve                       as GR
import qualified Hasura.GraphQL.Transport.HTTP.Protocol       as GH
import qualified Hasura.GraphQL.Validate                      as GV
import qualified Hasura.SQL.DML                               as S

import           Hasura.Db
import           Hasura.EncJSON
import           Hasura.GraphQL.Execute.LiveQuery.Types
import           Hasura.Prelude
import           Hasura.RQL.DML.Select                        (asSingleRowJsonResp)
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value

data LQOpts
  = LQOpts
  { _loMxOpts       :: LQM.MxOpts
  , _loFallbackOpts :: LQF.FallbackOpts
  } deriving (Show, Eq)

-- | Required for logging server configuration on startup
instance J.ToJSON LQOpts where
  toJSON (LQOpts mxOpts fbOpts) =
    J.object [ "multiplexed_options" J..= mxOpts
             , "fallback_options" J..= fbOpts
             ]

mkLQOpts :: LQM.MxOpts -> LQF.FallbackOpts -> LQOpts
mkLQOpts = LQOpts

data LiveQueriesState
  = LiveQueriesState
  { _lqsMultiplexed :: !LQM.LiveQueriesState
  , _lqsFallback    :: !LQF.LiveQueriesState
  , _lqsPGExecTx    :: !PGExecCtx
  }

dumpLiveQueriesState
  :: Bool -> LiveQueriesState -> IO J.Value
dumpLiveQueriesState extended (LiveQueriesState mx fallback _) = do
  mxJ <- LQM.dumpLiveQueriesState extended mx
  fallbackJ <- LQF.dumpLiveQueriesState fallback
  return $ J.object
    [ "fallback" J..= fallbackJ
    , "multiplexed" J..= mxJ
    ]

initLiveQueriesState
  :: LQOpts
  -> PGExecCtx
  -> IO LiveQueriesState
initLiveQueriesState (LQOpts mxOpts fallbackOpts) pgExecCtx = do
  (mxMap, fallbackMap) <- STM.atomically $
    (,) <$> LQM.initLiveQueriesState mxOpts
        <*> LQF.initLiveQueriesState fallbackOpts
  return $ LiveQueriesState mxMap fallbackMap pgExecCtx

data LiveQueryOpG f m
  = LQFallback !f
  | LQMultiplexed !m
  deriving (Show, Eq)

type LiveQueryOp = LiveQueryOpG LQF.FallbackOp LQM.MxOp

data LiveQueryId
  = LQIMultiplexed !LQM.LiveQueryId
  | LQIFallback !LQF.LiveQueryId

addLiveQuery
  :: LiveQueriesState
  -> LiveQueryOp
  -- the action to be executed when result changes
  -> OnChange
  -> IO LiveQueryId
addLiveQuery lqState liveQOp onResultAction =
  case liveQOp of
    LQMultiplexed mxOp ->
      LQIMultiplexed <$> LQM.addLiveQuery pgExecCtx mxMap mxOp onResultAction
    LQFallback fallbackOp ->
      LQIFallback <$> LQF.addLiveQuery
      pgExecCtx fallbackMap fallbackOp onResultAction
  where
    LiveQueriesState mxMap fallbackMap pgExecCtx = lqState

removeLiveQuery
  :: LiveQueriesState
  -- the query and the associated operation
  -> LiveQueryId
  -> IO ()
removeLiveQuery lqState = \case
  LQIMultiplexed lqId -> LQM.removeLiveQuery mxMap lqId
  LQIFallback lqId -> LQF.removeLiveQuery fallbackMap lqId
  where
    LiveQueriesState mxMap fallbackMap _ = lqState

data SubsPlan
  = SubsPlan
  { _sfMxOpCtx       :: !LQM.MxOpCtx
  , _sfVariableTypes :: !GV.VarPGTypes
  }

instance J.ToJSON SubsPlan where
  toJSON (SubsPlan opCtx varTypes) =
    J.object [ "mx_op_ctx"      J..= opCtx
             , "variable_types" J..= varTypes
             ]

collectNonNullableVars
  :: (MonadState GV.VarPGTypes m)
  => GR.UnresolvedVal -> m ()
collectNonNullableVars = \case
  GR.UVPG annPGVal -> do
    let GR.AnnPGVal varM isNullable colTy _ = annPGVal
    case (varM, isNullable) of
      (Just var, False) -> modify (Map.insert var colTy)
      _                 -> return ()
  _             -> return ()

type TextEncodedVariables
  = Map.HashMap G.Variable TxtEncodedPGVal

-- | converts the partial unresolved value containing
-- variables, session variables to an SQL expression
-- referring correctly to the values from '_subs' temporary table
-- The variables are at _subs.result_vars.variables and
-- session variables at _subs.result_vars.user
type FallbackOpPartial = (GR.QueryRootFldUnresolved, Set.HashSet G.Variable)
type MultiplexedOpPartial = (GV.VarPGTypes, Q.Query, TextEncodedVariables)

type LiveQueryOpPartial = LiveQueryOpG FallbackOpPartial MultiplexedOpPartial

-- | Creates a partial live query operation, used in both
-- analyze and execution of a live query
getLiveQueryOpPartial
  :: ( MonadError QErr m
     , MonadIO m
     )

  -- | to validate arguments
  => PGExecCtx

  -- | variable definitions as seen in the subscription, needed in
  -- checking whether the subscription can be multiplexed or not
  -> [G.VariableDefinition]

  -- | The partially processed live query field
  -> GR.QueryRootFldUnresolved

  -> m LiveQueryOpPartial
getLiveQueryOpPartial pgExecCtx varDefs astUnresolved = do
  -- collect the variables (with their types) used inside the subscription
  (_, varTypes) <- flip runStateT mempty $ GR.traverseQueryRootFldAST
                   collectNonNullableVars astUnresolved

  let nonConfirmingVariables = getNonConfirmingVariables varTypes

  -- Can the subscription be multiplexed?
  -- Only if all variables are non null and can be prepared
  if null nonConfirmingVariables
    then do
      let (mxQuery, annVarVals) = LQM.resolveToMxQuery astUnresolved
      -- We need to ensure that the values provided for variables
      -- are correct according to Postgres. Without this check
      -- an invalid value for a variable for one instance of the
      -- subscription will take down the entire multiplexed query
      txtEncodedVars <- validateAnnVarValsOnPg pgExecCtx annVarVals
      return $ LQMultiplexed (varTypes, mxQuery, txtEncodedVars)
    else
      return $ LQFallback (astUnresolved, nonConfirmingVariables)
  where
    -- get the variables which don't conifrm to the
    -- 'non-null scalar' rule
    getNonConfirmingVariables usedVariables =
      let queryVariables = Set.fromList $ map G._vdVariable varDefs
          confirmingVariables = Map.keysSet usedVariables
      in queryVariables `Set.difference` confirmingVariables

-- | Creates a live query operation and if possible, a reusable plan
--
subsOpFromPGAST
  :: ( MonadError QErr m
     , MonadReader r m
     , Has UserInfo r
     , MonadIO m
     )

  -- | to validate arguments
  => PGExecCtx

  -- | used as part of an identifier in the underlying live query systems
  -- to avoid unnecessary load on Postgres where possible
  -> GH.GQLReqUnparsed

  -- | variable definitions as seen in the subscription, needed in
  -- checking whether the subscription can be multiplexed or not
  -> [G.VariableDefinition]

  -- | The alias and the partially processed live query field
  -> (G.Alias, GR.QueryRootFldUnresolved)

  -> m (LiveQueryOp, Maybe SubsPlan)
subsOpFromPGAST pgExecCtx reqUnparsed varDefs (fldAls, astUnresolved) = do
  userInfo <- asks getter

  liveQueryOpPartial <- getLiveQueryOpPartial pgExecCtx varDefs astUnresolved

  case liveQueryOpPartial of
    LQFallback _ -> mkFallbackOp userInfo
    LQMultiplexed (varTypes, mxQuery, txtEncodedVars) ->
      mkMultiplexedOp userInfo varTypes mxQuery txtEncodedVars

  where
    -- multiplexed subscription
    mkMultiplexedOp userInfo varTypes mxQuery txtEncodedVars = do
      let mxOpCtx = LQM.mkMxOpCtx (userRole userInfo)
                    (GH._grQuery reqUnparsed) fldAls mxQuery
      let mxOp = (mxOpCtx, userVars userInfo, txtEncodedVars)
      return (LQMultiplexed mxOp, Just $ SubsPlan mxOpCtx varTypes)

    -- fallback tx subscription
    mkFallbackOp userInfo = do
      (astResolved, prepArgs) <-
        flip runStateT mempty $ GR.traverseQueryRootFldAST
        GR.resolveValPrep astUnresolved
      let tx = withUserInfo userInfo $ liftTx $
               asSingleRowJsonResp (GR.toPGQuery astResolved) $ toList prepArgs
          fallbackOp = LQF.mkFallbackOp userInfo reqUnparsed $ withAlias tx
      return (LQFallback fallbackOp, Nothing)

    fldAlsT = G.unName $ G.unAlias fldAls
    withAlias tx =
      encJFromAssocList . pure . (,) fldAlsT <$> tx

-- | Checks if the provided arguments are valid values for their corresponding types.
-- Generates SQL of the format "select 'v1'::t1, 'v2'::t2 ..."
validateAnnVarValsOnPg
  :: ( MonadError QErr m
     , MonadIO m
     )
  => PGExecCtx
  -> GV.AnnPGVarVals
  -> m TextEncodedVariables
validateAnnVarValsOnPg pgExecCtx annVarVals = do
  let valSel = mkValidationSel $ Map.elems annVarVals

  Q.Discard _ <- runTx' $ liftTx $
    Q.rawQE valPgErrHandler (Q.fromBuilder $ toSQL valSel) [] False
  return $ fmap (txtEncodedPGVal . snd) annVarVals

  where
    mkExtrs = map (flip S.Extractor Nothing . uncurry toTxtValue)
    mkValidationSel vars =
      S.mkSelect { S.selExtr = mkExtrs vars }
    runTx' tx = do
      res <- liftIO $ runExceptT (runLazyTx' pgExecCtx tx)
      liftEither res

-- | The error handler that is used to errors in the validation SQL.
-- It tries to specifically read few PG error codes which indicate
-- that the format of the value provided for a type is incorrect
valPgErrHandler :: Q.PGTxErr -> QErr
valPgErrHandler txErr =
  fromMaybe (defaultTxErrorHandler txErr) $ do
    stmtErr <- Q.getPGStmtErr txErr
    codeMsg <- getPGCodeMsg stmtErr
    (qErrCode, qErrMsg) <- extractError codeMsg
    return $ err400 qErrCode qErrMsg
  where
    getPGCodeMsg pged =
      (,) <$> Q.edStatusCode pged <*> Q.edMessage pged
    extractError = \case
      -- invalid text representation
      ("22P02", msg) -> return (DataException, msg)
      -- invalid parameter value
      ("22023", msg) -> return (DataException, msg)
      -- invalid input values
      ("22007", msg) -> return (DataException, msg)
      _              -> Nothing

-- | Use the existing plan with new variables and session variables
-- to create a live query operation
subsOpFromPlan
  :: ( MonadError QErr m
     , MonadIO m
     )
  => PGExecCtx
  -> UserVars
  -> Maybe GH.VariableValues
  -> SubsPlan
  -> m LiveQueryOp
subsOpFromPlan pgExecCtx usrVars varValsM (SubsPlan mxOpCtx varTypes) = do
  annVarVals <- GV.getAnnPGVarVals varTypes varValsM
  txtEncodedVars <- validateAnnVarValsOnPg pgExecCtx annVarVals
  return $ LQMultiplexed (mxOpCtx, usrVars, txtEncodedVars)
