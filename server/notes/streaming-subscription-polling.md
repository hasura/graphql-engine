This note is in [Hasura.GraphQL.Execute.Subscription.Poll.StreamingQuery](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Execute/Subscription/Poll/StreamingQuery.hs#L103).

# Streaming subscription polling


Every cohort of a streaming subscription is associated with a mutable latest
cursor value reference, which contains the current value of the cursor.

After a poll, the mutable cursor value gets updated if its value is a non-null
one, null value means that there were no rows to get the min/max value from.

After this, the cohort map associated with the poller is also rebuilt, which
will make sure that the cohort map's key contains the latest cursor values. So,
any new subscriber will get added to an existing cohort if the new subscriber's
cohort key matches with any of the existing cohorts. We *need* to rebuild the
cohort map, because, say if we don't rebuild the cohort map then a new
subscriber may get added to a cohort which has been streaming for a while now,
then the new subscriber will get the responses according to the cursor value
stored in the cohort, instead of the initial value specified by the client. For
example:

Client 1 started streaming a query at t1, say:

```
   subscription {
      log_stream(cursor: {initial_value: {created_at: "2020-01-01"}}, batch_size: 1000) {
         id
         level
         detail
     }
   }
```

Client 2 starts streaming at t2 with the same query (where t2 > t1), if the
cohort map is not rebuilt (to reflect cursor value in the cohort key), then
client 2 and client 1 will both start getting the same responses, which is wrong
because client 2 should start streaming from the initial value it provided.


