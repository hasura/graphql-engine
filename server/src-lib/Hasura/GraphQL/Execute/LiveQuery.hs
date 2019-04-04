{-# LANGUAGE RankNTypes #-}

module Hasura.GraphQL.Execute.LiveQuery
  ( LiveQueriesState
  , initLiveQueriesState

  -- TODO: we shouldn't be exporting
  -- the construction of LiveQuery
  , LiveQuery(..)
  , LiveQueryOp
  , addLiveQuery
  , removeLiveQuery

  , TxRunner(..)

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
import qualified Language.GraphQL.Draft.Syntax                as G

import qualified Hasura.GraphQL.Execute.LiveQuery.Fallback    as LQF
import qualified Hasura.GraphQL.Execute.LiveQuery.Multiplexed as LQM
import qualified Hasura.GraphQL.Resolve                       as GR
import qualified Hasura.GraphQL.Transport.HTTP.Protocol       as GH
import qualified Hasura.GraphQL.Validate                      as GV
import qualified Hasura.SQL.DML                               as S

import           Hasura.EncJSON
import           Hasura.GraphQL.Execute.LiveQuery.Types
import           Hasura.Prelude
import           Hasura.RQL.DML.Select                        (asSingleRowJsonResp)
import           Hasura.RQL.Types

import           Hasura.SQL.Types
import           Hasura.SQL.Value

-- this has to be in another module
newtype TxRunner
  = TxRunner { unTxRunner :: forall a. LazyTx QErr a -> IO (Either QErr a) }

data LiveQueriesState k
  = LiveQueriesState
  { _lqsMultiplexed :: LQM.LiveQueryMap k
  , _lqsFallback    :: LQF.LiveQueryMap k
  , _lqsTxRunner    :: TxRunner
  }

initLiveQueriesState
  :: (forall a. LazyTx QErr a -> IO (Either QErr a))
  -> IO (LiveQueriesState k)
initLiveQueriesState txRunner = do
  (mxMap, fallbackMap) <- STM.atomically $
    (,) <$> LQM.newLiveQueryMap <*> LQF.newLiveQueryMap
  return $ LiveQueriesState mxMap fallbackMap $ TxRunner txRunner

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
      (unTxRunner txRunner) mxMap liveQ mxOp k onResultAction
    LQFallback fallbackOp ->
      LQF.addLiveQuery
      (unTxRunner txRunner) fallbackMap liveQ fallbackOp k onResultAction
  where
    LiveQueriesState mxMap fallbackMap txRunner = lqState

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
  :: (MonadState TextEncodedVariables m)
  => GR.UnresolvedVal -> m S.SQLExp
toMultiplexedQueryVar = \case
  GR.UVPG annPGVal ->
    let GR.AnnPGVal varM isNullable colTy colVal = annPGVal
    in case (varM, isNullable) of
      -- we don't check for nullability as
      -- this is only used for reusable plans
      -- the check has to be made before this
      (Just var, _) -> do
        modify $ Map.insert var (txtEncodedPGVal colVal)
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
     )
  => [G.VariableDefinition]
  -> (G.Alias, GR.QueryRootFldUnresolved)
  -> m (LiveQueryOp, Maybe SubsPlan)
subsOpFromPGAST varDefs (fldAls, astUnresolved) = do
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
      (astResolved, txtEncodedVars) <-
        flip runStateT mempty $ GR.traverseQueryRootFldAST
        toMultiplexedQueryVar astUnresolved
      let mxQuery = LQM.mkMxQuery $ GR.toPGQuery astResolved
          plan    = SubsPlan fldAls mxQuery varTypes
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

-- use the existing plan and new variables to create a pg query
subsOpFromPlan
  :: (MonadError QErr m)
  => Maybe GH.VariableValues
  -> SubsPlan
  -> m LiveQueryOp
subsOpFromPlan varValsM (SubsPlan alias query varTypes) = do
  validatedVars <- GV.getAnnPGVarVals varTypes varValsM
  let txtEncodedVars = fmap txtEncodedPGVal validatedVars
  return $ LQMultiplexed (alias, query, txtEncodedVars)
