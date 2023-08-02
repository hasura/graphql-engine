-- | Module related to async action query subscriptions
module Hasura.GraphQL.Execute.Action.Subscription
  ( asyncActionSubscriptionsProcessor,
  )
where

import Control.Concurrent.Extended qualified as C
import Control.Concurrent.STM qualified as STM
import Data.List.NonEmpty qualified as NE
import Hasura.GraphQL.Execute.Action
import Hasura.GraphQL.Execute.Subscription.State
import Hasura.GraphQL.Execute.Subscription.TMap qualified as TMap
import Hasura.Metadata.Class
import Hasura.Prelude

{- Note [Async action subscriptions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We have two kinds of async action query execution based on the defined relationships
on output object type. See Note [Resolving async action query]. Actions with table
relationships on output object type have to a generate SQL select query with the action
webhook response embedded in so as to execute on the source database that emits client
response with joined relationship rows (see Note [Resolving async action query]).
We can multiplex this queries and run a live subscription just like our usual database
queries. But the action log from the metadata storage is subjected to change after
calling action webhook in two ways,
1. When the webhook returns successful response, 'response_payload' column value isn't null
2. When any exception occurs in the process, 'errors' column value isn't null

So, we have a background thread (@'asyncActionSubscriptionsProcessor') which
constantly fetches the action log response from the metadata storage and restarts the
live query to reflect those changes in subscriptions.

In case of action queries without relationships, there are no SQL queries associated with.
We can't multiplex them. The thread, @'asyncActionSubscriptionsProcessor' also handles these
action subscriptions by sending the responses to the websocket client after fetching the
action log response.

We can't support multiple async query fields of different kinds in a single subscription.
-}

-- | A forever running thread which processes async action subscriptions.
-- See Note [Async action subscriptions]
asyncActionSubscriptionsProcessor ::
  ( MonadIO m,
    MonadMetadataStorage m
  ) =>
  AsyncActionSubscriptionState ->
  m void
asyncActionSubscriptionsProcessor subsState = forever do
  -- Collect all active async query subscription operations
  opList <- liftIO $ STM.atomically do
    l <- TMap.toList subsState
    onNothing (NE.nonEmpty l) STM.retry -- Continue only if there are any active subscription operations
  for_ opList $ \(opId, AsyncActionQueryLive actionIds onError op) -> do
    -- Fetch action webhook responses from the metadata storage
    actionLogMapE <- runExceptT $ fetchActionLogResponses $ toList actionIds
    liftIO $ case actionLogMapE of
      Left err -> do
        onError err
        removeAsyncActionLiveQuery subsState opId
      Right (actionLogMap, actionsComplete) ->
        case op of
          LAAQNoRelationships (LiveAsyncActionQueryWithNoRelationships sendResp sendCompleted) -> do
            sendResp actionLogMap
            when actionsComplete $ do
              sendCompleted
              removeAsyncActionLiveQuery subsState opId
          LAAQOnSourceDB (LiveAsyncActionQueryOnSource currLqId prevLogMap lqRestarter) -> do
            -- Actions webhook responses aren't updated in metadata storage, hence no need to restart the live query
            unless (prevLogMap == actionLogMap) $ do
              maybeNewLqId <- lqRestarter currLqId actionLogMap
              if actionsComplete
                then removeAsyncActionLiveQuery subsState opId
                else do
                  case maybeNewLqId of
                    Nothing ->
                      -- Happens only when restarting a live query fails.
                      -- There's no point in holding the operation in the state.
                      removeAsyncActionLiveQuery subsState opId
                    Just newLqId ->
                      addAsyncActionLiveQuery subsState opId actionIds onError
                        $ LAAQOnSourceDB
                        $ LiveAsyncActionQueryOnSource newLqId actionLogMap lqRestarter
  -- Sleep for a second
  liftIO $ C.sleep $ seconds 1
