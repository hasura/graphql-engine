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

  -- TODO: we shouldn't be exporting
  -- the construction of LiveQuery
  , LiveQuery(..)
  , LiveQueryOp
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

mkLQOpts :: LQM.MxOpts -> LQF.FallbackOpts -> LQOpts
mkLQOpts = LQOpts

data LiveQueriesState k
  = LiveQueriesState
  { _lqsMultiplexed :: !(LQM.LiveQueriesState k)
  , _lqsFallback    :: !(LQF.LiveQueriesState k)
  , _lqsPGExecTx    :: !PGExecCtx
  }

dumpLiveQueriesState
  :: (J.ToJSON k) => LiveQueriesState k -> IO J.Value
dumpLiveQueriesState (LiveQueriesState mx fallback _) = do
  mxJ <- LQM.dumpLiveQueriesState mx
  fallbackJ <- LQF.dumpLiveQueriesState fallback
  return $ J.object
    [ "fallback" J..= fallbackJ
    , "multiplexed" J..= mxJ
    ]

initLiveQueriesState
  :: LQOpts
  -> PGExecCtx
  -> IO (LiveQueriesState k)
initLiveQueriesState (LQOpts mxOpts fallbackOpts) pgExecCtx = do
  (mxMap, fallbackMap) <- STM.atomically $
    (,) <$> LQM.initLiveQueriesState mxOpts
        <*> LQF.initLiveQueriesState fallbackOpts
  return $ LiveQueriesState mxMap fallbackMap pgExecCtx

data LiveQueryOp
  = LQMultiplexed !LQM.MxOp
  | LQFallback !LQF.FallbackOp

addLiveQuery
  :: (Eq k, Hashable k)
  => LiveQueriesState k
  -- the query
  -> LiveQuery
  -> LiveQueryOp
  -- a unique operation id
  -> k
  -- the action to be executed when result changes
  -> OnChange
  -> IO ()
addLiveQuery lqState liveQ liveQOp k onResultAction =
  case liveQOp of
    LQMultiplexed mxOp ->
      LQM.addLiveQuery
      pgExecCtx mxMap liveQ mxOp k onResultAction
    LQFallback fallbackOp ->
      LQF.addLiveQuery
      pgExecCtx fallbackMap liveQ fallbackOp k onResultAction
  where
    LiveQueriesState mxMap fallbackMap pgExecCtx = lqState

removeLiveQuery
  :: (Eq k, Hashable k)
  => LiveQueriesState k
  -- the query and the associated operation
  -> LiveQuery
  -> k
  -> IO ()
removeLiveQuery lqState liveQ k = do
  LQM.removeLiveQuery mxMap liveQ k
  LQF.removeLiveQuery fallbackMap liveQ k
  where
    LiveQueriesState mxMap fallbackMap _ = lqState

data SubsPlan
  = SubsPlan
  { _sfAlias         :: !G.Alias
  , _sfQuery         :: !Q.Query
  , _sfVariableTypes :: !GV.VarPGTypes
  } deriving (Show, Eq)

instance J.ToJSON SubsPlan where
  toJSON (SubsPlan alias q variables) =
    J.object [ "query"     J..= Q.getQueryText q
             , "alias"     J..= alias
             , "variables" J..= variables
             ]

collectNonNullableVars
  :: (MonadState GV.VarPGTypes m)
  => GR.UnresolvedVal -> m GR.UnresolvedVal
collectNonNullableVars val = do
  case val of
    GR.UVPG annPGVal -> do
      let GR.AnnPGVal varM isNullable colTy _ = annPGVal
      case (varM, isNullable) of
        (Just var, False) -> modify (Map.insert var colTy)
        _                 -> return ()
    _             -> return ()
  return val

type TextEncodedVariables
  = Map.HashMap G.Variable TxtEncodedPGVal

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
        return $ fromResVars colTy
          [ "variables"
          , G.unName $ G.unVariable var
          ]
      _             -> return $ toTxtValue colTy colVal
  -- TODO: check the logic around colTy and session variable's type
  GR.UVSessVar colTy sessVar ->
    return $ fromResVars colTy [ "user", T.toLower sessVar]
  GR.UVSQL sqlExp -> return sqlExp
  where
    fromResVars colTy jPath =
      S.withTyAnn colTy $ S.SEOpApp (S.SQLOp "#>>")
      [ S.SEQIden $ S.QIden (S.QualIden $ Iden "_subs")
        (Iden "result_vars")
      , S.SEArray $ map S.SELit jPath
      ]

subsOpFromPGAST
  :: ( MonadError QErr m
     , MonadReader r m
     , Has UserInfo r
     , MonadIO m
     )
  => PGExecCtx
  -> [G.VariableDefinition]
  -> (G.Alias, GR.QueryRootFldUnresolved)
  -> m (LiveQueryOp, Maybe SubsPlan)
subsOpFromPGAST pgExecCtx varDefs (fldAls, astUnresolved) = do
  userInfo <- asks getter
  (_, varTypes) <- flip runStateT mempty $ GR.traverseQueryRootFldAST
                   collectNonNullableVars astUnresolved
  -- can the subscription be multiplexed?
  if Set.fromList (Map.keys varTypes) == allVars
    then multiplexedOp varTypes
    else fallbackOp userInfo
  where
    allVars = Set.fromList $ map G._vdVariable varDefs

    -- multiplexed subscription
    multiplexedOp varTypes = do
      (astResolved, annVarVals) <-
        flip runStateT mempty $ GR.traverseQueryRootFldAST
        toMultiplexedQueryVar astUnresolved
      let mxQuery = LQM.mkMxQuery $ GR.toPGQuery astResolved
          plan    = SubsPlan fldAls mxQuery varTypes
      txtEncodedVars <- validateAnnVarValsOnPg pgExecCtx annVarVals
      return (LQMultiplexed (fldAls, mxQuery, txtEncodedVars), Just plan)

    -- fallback tx subscription
    fallbackOp userInfo = do
      (astResolved, prepArgs) <-
        flip runStateT mempty $ GR.traverseQueryRootFldAST
        GR.resolveValPrep astUnresolved
      let tx = withUserInfo userInfo $ liftTx $
               asSingleRowJsonResp (GR.toPGQuery astResolved) $ toList prepArgs
      return (LQFallback $ withAlias tx, Nothing)

    fldAlsT = G.unName $ G.unAlias fldAls
    withAlias tx =
      encJFromAssocList . pure . (,) fldAlsT <$> tx

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

-- use the existing plan and new variables to create a pg query
subsOpFromPlan
  :: ( MonadError QErr m
     , MonadIO m
     )
  => PGExecCtx
  -> Maybe GH.VariableValues
  -> SubsPlan
  -> m LiveQueryOp
subsOpFromPlan pgExecCtx varValsM (SubsPlan alias query varTypes) = do
  annVarVals <- GV.getAnnPGVarVals varTypes varValsM
  txtEncodedVars <- validateAnnVarValsOnPg pgExecCtx annVarVals
  return $ LQMultiplexed (alias, query, txtEncodedVars)
