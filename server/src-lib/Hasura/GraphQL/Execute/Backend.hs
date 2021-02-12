{-# LANGUAGE AllowAmbiguousTypes #-}

module Hasura.GraphQL.Execute.Backend where

import                          Hasura.Prelude

import                qualified Data.Aeson                             as J
import                qualified Data.Environment                       as Env
import                qualified Language.GraphQL.Draft.Syntax          as G
import                qualified Network.HTTP.Client                    as HTTP
import                qualified Network.HTTP.Types                     as HTTP

import                          Data.Kind                              (Type)

import                qualified Hasura.GraphQL.Transport.HTTP.Protocol as GH

import                          Hasura.EncJSON
import                          Hasura.GraphQL.Context
import {-# SOURCE #-}           Hasura.GraphQL.Execute.Action
import                          Hasura.GraphQL.Parser                  hiding (Type)
import                          Hasura.RQL.IR.RemoteJoin
import                          Hasura.RQL.Types.Common
import                          Hasura.RQL.Types.Error
import                          Hasura.RQL.Types.RemoteSchema
import                          Hasura.SQL.Backend
import                          Hasura.Server.Version                  (HasVersion)
import                          Hasura.Session


-- | This typeclass enacapsulates how a given backend translates a root field into an execution
-- plan. For now, each root field maps to one execution step, but in the future, when we have
-- a client-side dataloader, each root field might translate into a multi-step plan.
class Backend b => BackendExecute (b :: BackendType) tx where
  -- generated query information
  type PreparedQuery b :: Type
  getRemoteJoins :: PreparedQuery b -> [RemoteJoin b]

  -- execution plan generation
  mkDBQueryPlan
    :: forall m
     . ( MonadError QErr m
       , HasVersion
       )
    => Env.Environment
    -> HTTP.Manager
    -> [HTTP.Header]
    -> UserInfo
    -> [G.Directive G.Name]
    -> SourceConfig b
    -> QueryDB b (UnpreparedValue b)
    -> m (ExecutionStep tx)
  mkDBMutationPlan
    :: forall m
     . ( MonadError QErr m
       , HasVersion
       )
    => Env.Environment
    -> HTTP.Manager
    -> [HTTP.Header]
    -> UserInfo
    -> Bool
    -> SourceConfig b
    -> MutationDB b (UnpreparedValue b)
    -> m (ExecutionStep tx)


-- | One execution step to processing a GraphQL query (e.g. one root field).
data ExecutionStep tx where
  ExecStepDB
    :: forall (b :: BackendType) tx
     . (BackendExecute b tx, Monad tx)
    => SourceConfig b
    -> Maybe (PreparedQuery b)
    -> HTTP.ResponseHeaders
    -> tx EncJSON
    -> ExecutionStep tx
  -- ^ A query to execute against the database
  ExecStepAction
    :: ActionExecutionPlan
    -> HTTP.ResponseHeaders
    -> ExecutionStep tx
  -- ^ Execute an action
  ExecStepRemote
    :: !RemoteSchemaInfo
    -> !GH.GQLReqOutgoing
    -> ExecutionStep tx
  -- ^ A graphql query to execute against a remote schema
  ExecStepRaw
    :: J.Value
    -> ExecutionStep tx
  -- ^ Output a plain JSON object

-- | The series of steps that need to be executed for a given query. For now, those steps are all
-- independent. In the future, when we implement a client-side dataloader and generalized joins,
-- this will need to be changed into an annotated tree.
type ExecutionPlan tx = InsOrdHashMap G.Name (ExecutionStep tx)
