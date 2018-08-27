{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module Hasura.GraphQL.Execute.QueryCache
  ( QueryCache
  , getAST
  , addAST
  , getPlan
  , addPlan
  , initQueryCache
  , clearQueryCache
  ) where

import qualified Hasura.GraphQL.Transport.HTTP.Protocol as GH

import qualified Hasura.GraphQL.Execute.Plan            as EP
import qualified Hasura.GraphQL.LRUCache                as LRU
import           Hasura.Prelude
import           Hasura.RQL.Types

type PlanCache =
  LRU.LRUCache
  (RoleName, Maybe GH.OperationName, GH.GQLExecDoc)
  EP.QueryPlan

initPlanCache :: IO PlanCache
initPlanCache = LRU.initLRUCache 100

type ASTCache =
  LRU.LRUCache Text GH.GQLExecDoc

initASTCache :: IO ASTCache
initASTCache = LRU.initLRUCache 100

newtype QueryCache
  = QueryCache (ASTCache, PlanCache)

initQueryCache :: IO QueryCache
initQueryCache =
  fmap QueryCache $ (,) <$> initASTCache <*> initPlanCache

getAST
  :: Text -> QueryCache -> IO (Maybe GH.GQLExecDoc)
getAST q queryCache =
  LRU.lookup astCache q
  where
    QueryCache (astCache, _) = queryCache

addAST
  :: Text -> GH.GQLExecDoc -> QueryCache -> IO ()
addAST q ast queryCache =
  LRU.insert astCache q ast
  where
    QueryCache (astCache, _) = queryCache

getPlan
  :: RoleName -> GH.GQLReqParsed -> QueryCache -> IO (Maybe EP.QueryPlan)
getPlan rn (GH.GQLReq opNameM q _) queryCache =
  LRU.lookup planCache (rn, opNameM, q)
  where
    QueryCache (_, planCache) = queryCache

addPlan
  :: RoleName -> GH.GQLReqParsed -> EP.QueryPlan -> QueryCache -> IO ()
addPlan rn (GH.GQLReq opNameM q _) queryPlan queryCache =
  LRU.insert planCache (rn, opNameM, q) queryPlan
  where
    QueryCache (_, planCache) = queryCache

clearQueryCache :: QueryCache -> IO ()
clearQueryCache (QueryCache (astCache, planCache)) =
  LRU.clearLRUCache astCache >> LRU.clearLRUCache planCache
