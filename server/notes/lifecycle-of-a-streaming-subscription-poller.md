This note is in [Hasura.GraphQL.Execute.Subscription.Poll.StreamingQuery](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Execute/Subscription/Poll/StreamingQuery.hs#L142).

# Lifecycle of a streaming subscription poller

  +-------------------------------------------+           +-----------------------------------------------------------------+
  | Execute multiplexed query in the database | ------->  | Parse the response, every row of response contains three things:|
  +-------------------------------------------+           |  1. Cohort ID                                                   |
             ^                                            |  2. Cohort's Response                                           |
             |                                            |  3. Cohort's latest cursor value                                |
             |                                            +-----------------------------------------------------------------+
             |                                                                            |
             |                                                                            |
             |                                                                            v
             |                                            +------------------------------------------------------------------------+
             |                                            | Processing of the response:                                            |
             |                                            | 1. Send the result to the subscribers                                  |
   +------------------------------------+                 | 2. Update the cursor value in the mutable reference                    |
   | Rebuild the cohort map             |                 |    of the snapshot so that the next poll uses this value               |
   +------------------------------------+        <------  +------------------------------------------------------------------------+


