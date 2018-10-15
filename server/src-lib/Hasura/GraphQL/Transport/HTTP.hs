{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}

module Hasura.GraphQL.Transport.HTTP
  ( runGQ
  ) where

import           Control.Exception                      (try)
import           Control.Lens
import           Debug.Trace
import           Hasura.Prelude

import qualified Data.ByteString.Lazy                   as BL
import qualified Data.Text                              as T
import qualified Data.Text.IO                           as T
import qualified Database.PG.Query                      as Q
import qualified Language.GraphQL.Draft.Parser          as G
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.Wreq                           as Wreq

import           Hasura.GraphQL.Schema
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.RQL.Types

import qualified Hasura.GraphQL.Resolve                 as R
import qualified Hasura.GraphQL.Validate                as VQ
import qualified Hasura.GraphQL.Validate.Field          as VF
import qualified Hasura.Server.Query                    as RQ


runGQ
  :: (MonadIO m, MonadError QErr m)
  => Q.PGPool -> Q.TxIsolation
  -> UserInfo -> GCtxMap
  -> HTTP.Manager
  -> GraphQLRequest
  -> BL.ByteString
  -> m BL.ByteString
runGQ pool isoL userInfo gCtxMap manager req rawReq = do
  --liftIO $ print req
  ghSchemaT <- liftIO $ T.readFile "ws/local-schema.graphql"
  -- FIXME: parseSchemaDoc doesn't work if there's a `schema` keyword present
  -- FIXME: should be able to deserialize JSON to schema as well
  ghSchema <- either (throw400 ParseFailed) return $ G.parseSchemaDoc ghSchemaT
  let mergedSchema = CombinedSchema gCtxMap [ghSchema]
      typeNames = getTopLevelNodes ghSchema -- all the valid top-level nodes in the schema

  -- FIXME: better way to retrieve all top-level nodes of the current query
  let (GraphQLRequest opNameM q _) = req
      (selSets, opDefs, _) = G.partitionExDefs $ unGraphQLQuery q
  opDef <- VQ.getTypedOp opNameM selSets opDefs
  let selSet = G._todSelectionSet opDef
      topLevelNodes = map (\(G.SelectionField f) -> G._fName f) selSet
      -- find out if any or all of the top-nodes in the query are part of the
      -- remote schema
      isany = any (`elem` typeNames) topLevelNodes
      isall = all (`elem` typeNames) topLevelNodes

  liftIO $ print typeNames
  liftIO $ print topLevelNodes
  liftIO $ print (isany, isall)

  case (isany, isall) of
    (True, True)  -> runRemoteGQ manager rawReq
    (True, False) ->
      throw400 UnexpectedPayload "can't mix remote and hasura nodes"
    (False, _)    -> runHasuraGQ pool isoL userInfo gCtxMap req

getTopLevelNodes :: G.SchemaDocument -> [G.Name]
getTopLevelNodes (G.SchemaDocument schema) =
  join $ map extractFieldNames queryMutationDefinition
  where
    extractFieldNames objectdef =
      let fields = G._otdFieldsDefinition objectdef
      in map G._fldName fields

    queryMutationDefinition =
      -- trim the sum type tag for object definition
      map (\(G.TypeDefinitionObject d) -> d) objectdefs
      where
      -- get only the object definitions which are called Query or Mutation
        objectdefs = flip filter schema $
          \case
            G.TypeDefinitionObject d ->
              (G._otdName d == "Query") || (G._otdName d == "Mutation")
            _ -> False

getTypeNames :: G.SchemaDocument -> [G.Name]
getTypeNames (G.SchemaDocument schema) = flip map schema $ \case
  G.TypeDefinitionScalar d      -> G._stdName d
  G.TypeDefinitionObject d      -> G._otdName d
  G.TypeDefinitionInterface d   -> G._itdName d
  G.TypeDefinitionUnion d       -> G._utdName d
  G.TypeDefinitionEnum  d       -> G._etdName d
  G.TypeDefinitionInputObject d -> G._iotdName d


runHasuraGQ
  :: (MonadIO m, MonadError QErr m)
  => Q.PGPool -> Q.TxIsolation
  -> UserInfo -> GCtxMap
  -> GraphQLRequest
  -> m BL.ByteString
runHasuraGQ pool isoL userInfo gCtxMap req = do
  (opTy, fields) <- runReaderT (VQ.validateGQ req) gCtx
  when (opTy == G.OperationTypeSubscription) $ throw400 UnexpectedPayload
    "subscriptions are not supported over HTTP, use websockets instead"
  let tx = R.resolveSelSet userInfo gCtx opTy fields
  resp <- liftIO (runExceptT $ runTx tx) >>= liftEither
  return $ encodeGQResp $ GQSuccess resp
  where
    gCtx = getGCtx (userRole userInfo) gCtxMap
    runTx tx =
      Q.runTx pool (isoL, Nothing) $
      RQ.setHeadersTx userInfo >> tx


runRemoteGQ
  :: (MonadIO m, MonadError QErr m)
  => HTTP.Manager
  -> BL.ByteString
  -- ^ the raw request string
  -> m BL.ByteString
runRemoteGQ manager q = do
  liftIO $ putStrLn "running remote graphql query"
  liftIO $ print q
  let options = Wreq.defaults
              & Wreq.headers .~ [("content-type", "application/json")]
              & Wreq.checkResponse ?~ (\_ _ -> return ())
              & Wreq.manager .~ Right manager

  res  <- liftIO $ try $ Wreq.postWith options "http://localhost:3000" q
  resp <- either logAndThrow return res
  return $ resp ^. Wreq.responseBody
  where
    logAndThrow :: (MonadError QErr m) => HTTP.HttpException -> m a
    logAndThrow err = throw500 $ T.pack . show $ err
