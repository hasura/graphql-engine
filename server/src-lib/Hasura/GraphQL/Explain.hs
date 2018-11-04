{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Hasura.GraphQL.Explain
  ( explainGQLQuery
  , GQLExplain
  ) where

import qualified Data.Aeson                             as J
import qualified Data.Aeson.Casing                      as J
import qualified Data.Aeson.TH                          as J
import qualified Data.HashMap.Strict                    as Map
import qualified Database.PG.Query                      as Q
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Text.Builder                           as TB
import qualified Data.ByteString.Lazy                   as BL

import           Hasura.GraphQL.Resolve.Context
import           Hasura.GraphQL.Schema
import           Hasura.GraphQL.Validate.Field
import           Hasura.Prelude
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value

import qualified Hasura.GraphQL.Resolve.Select          as RS
import qualified Hasura.GraphQL.Transport.HTTP.Protocol as GH
import qualified Hasura.GraphQL.Validate                as GV
import qualified Hasura.RQL.DML.Select                  as RS
import qualified Hasura.Server.Query                    as RQ

data GQLExplain
  = GQLExplain
  { _gqeQuery :: !GH.GraphQLRequest
  , _gqeUser  :: !(Maybe (Map.HashMap Text Text))
  } deriving (Show, Eq)

$(J.deriveJSON (J.aesonDrop 4 J.camelCase){J.omitNothingFields=True}
  ''GQLExplain
 )

data FieldPlan
  = FieldPlan
  { _fpField :: !G.Name
  , _fpSql   :: !(Maybe Text)
  , _fpPlan  :: !(Maybe [Text])
  } deriving (Show, Eq)

$(J.deriveJSON (J.aesonDrop 3 J.camelCase) ''FieldPlan)

type Explain =
  (ReaderT (FieldMap, OrdByCtx) (Except QErr))

runExplain
  :: (MonadError QErr m)
  => (FieldMap, OrdByCtx) -> Explain a -> m a
runExplain ctx m =
  either throwError return $ runExcept $ runReaderT m ctx

explainField
  :: UserInfo -> GCtx -> Field -> Q.TxE QErr FieldPlan
explainField userInfo gCtx fld =
  case fName of
    "__type"     -> return $ FieldPlan fName Nothing Nothing
    "__schema"   -> return $ FieldPlan fName Nothing Nothing
    "__typename" -> return $ FieldPlan fName Nothing Nothing
    _            -> do
      opCxt <- getOpCtx fName
      sel <- runExplain (fldMap, orderByCtx) $ case opCxt of
        OCSelect tn permFilter permLimit hdrs -> do
          validateHdrs hdrs
          RS.mkSQLSelect False <$>
            RS.fromField txtConverter tn permFilter permLimit fld
        OCSelectPkey tn permFilter hdrs -> do
          validateHdrs hdrs
          RS.mkSQLSelect True <$>
            RS.fromFieldByPKey txtConverter tn permFilter fld
        OCSelectAgg tn permFilter permLimit hdrs -> do
          validateHdrs hdrs
          RS.mkAggSelect <$>
            RS.fromAggField txtConverter tn permFilter permLimit fld
        _ -> throw500 "unexpected mut field info for explain"

      let selectSQL = TB.run $ toSQL sel
          withExplain = "EXPLAIN (FORMAT TEXT) " <> selectSQL
      planLines <- liftTx $ map runIdentity <$>
        Q.listQE dmlTxErrorHandler (Q.fromText withExplain) () True
      return $ FieldPlan fName (Just selectSQL) $ Just planLines
  where
    fName = _fName fld
    txtConverter = return . txtEncoder . snd
    opCtxMap = _gOpCtxMap gCtx
    fldMap = _gFields gCtx
    orderByCtx = _gOrdByCtx gCtx

    getOpCtx f =
      onNothing (Map.lookup f opCtxMap) $ throw500 $
      "lookup failed: opctx: " <> showName f

    validateHdrs hdrs = do
      let receivedHdrs = userVars userInfo
      forM_ hdrs $ \hdr ->
        unless (isJust $ getVarVal hdr receivedHdrs) $
        throw400 NotFound $ hdr <<> " header is expected but not found"

explainGQLQuery
  :: (MonadError QErr m, MonadIO m)
  => Q.PGPool
  -> Q.TxIsolation
  -> GCtxMap
  -> GQLExplain
  -> m BL.ByteString
explainGQLQuery pool iso gCtxMap (GQLExplain query userVarsRaw)= do
  (opTy, selSet) <- runReaderT (GV.validateGQ query) gCtx
  unless (opTy == G.OperationTypeQuery) $
    throw400 InvalidParams "only queries can be explained"
  let tx = mapM (explainField userInfo gCtx) (toList selSet)
  plans <- liftIO (runExceptT $ runTx tx) >>= liftEither
  return $ J.encode plans
  where
    usrVars = mkUserVars $ maybe [] Map.toList userVarsRaw
    userInfo = mkUserInfo (fromMaybe adminRole $ roleFromVars usrVars) usrVars
    gCtx = getGCtx (userRole userInfo) gCtxMap
    runTx tx =
      Q.runTx pool (iso, Nothing) $
      RQ.setHeadersTx (userVars userInfo) >> tx
