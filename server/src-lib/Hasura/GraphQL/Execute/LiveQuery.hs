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

  , LiveQueryOp
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
import qualified Data.Text                                    as T
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

data LiveQueryOp
  = LQMultiplexed !LQM.MxOp
  | LQFallback !LQF.FallbackOp

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
toMultiplexedQueryVar
  :: (MonadState GV.AnnPGVarVals m)
  => GR.UnresolvedVal -> m S.SQLExp
toMultiplexedQueryVar = \case
  GR.UVPG annPGVal ->
    let GR.AnnPGVal varM isNullable colTy colVal = annPGVal
    in case (varM, isNullable) of
      -- we don't check for nullability as
      -- this is only used for reusable plans
      -- the check has to be made before this
      (Just var, _) -> do
        modify $ Map.insert var (colTy, colVal)
        return $ fromResVars (PgTypeSimple colTy)
          [ "variables"
          , G.unName $ G.unVariable var
          ]
      _             -> return $ toTxtValue colTy colVal
  GR.UVSessVar ty sessVar ->
    return $ fromResVars ty [ "user", T.toLower sessVar]
  GR.UVSQL sqlExp -> return sqlExp
  where
    fromResVars ty jPath =
      flip S.SETyAnn (S.mkTypeAnn ty) $ S.SEOpApp (S.SQLOp "#>>")
      [ S.SEQIden $ S.QIden (S.QualIden $ Iden "_subs")
        (Iden "result_vars")
      , S.SEArray $ map S.SELit jPath
      ]

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

  -- collect the variables (with their types) used inside the subscription
  (_, varTypes) <- flip runStateT mempty $ GR.traverseQueryRootFldAST
                   collectNonNullableVars astUnresolved

  -- Can the subscription be multiplexed?
  -- Only if all variables are non null and can be prepared
  if Set.fromList (Map.keys varTypes) == allVars
    then mkMultiplexedOp userInfo varTypes
    else mkFallbackOp userInfo
  where
    allVars = Set.fromList $ map G._vdVariable varDefs

    -- multiplexed subscription
    mkMultiplexedOp userInfo varTypes = do
      (astResolved, annVarVals) <-
        flip runStateT mempty $ GR.traverseQueryRootFldAST
        toMultiplexedQueryVar astUnresolved
      let mxOpCtx = LQM.mkMxOpCtx (userRole userInfo)
                    (GH._grQuery reqUnparsed) fldAls $
                    GR.toPGQuery astResolved

      -- We need to ensure that the values provided for variables
      -- are correct according to Postgres. Without this check
      -- an invalid value for a variable for one instance of the
      -- subscription will take down the entire multiplexed query
      txtEncodedVars <- validateAnnVarValsOnPg pgExecCtx annVarVals
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
