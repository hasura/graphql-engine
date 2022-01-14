This note is in [Hasura.GraphQL.Execute.Action.Subscription](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Execute/Action/Subscription.hs#L16).
It is referenced at:
  - line 42 of [Hasura.GraphQL.Execute.Action.Subscription](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Execute/Action/Subscription.hs#L42)

# Async action subscriptions

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

