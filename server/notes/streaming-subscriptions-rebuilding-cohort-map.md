This note is in [Hasura.GraphQL.Execute.Subscription.Poll.StreamingQuery](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Execute/Subscription/Poll/StreamingQuery.hs#L42).

# Streaming subscriptions rebuilding cohort map


When the multiplexed query is polled, the cohort is snapshotted to get the
existing and newly added subscribers (if any), let's call these subscribers as
current poll subscribers.

Every cohort is associated with a cohort key and the cohort key is basically the
variables the cohort uses. The cohort variables contains multiple types of
variables in it, session variables, query variables, synthetic variables and
cursor variables, out of all these, the cursor variable may change with every
poll. So, we rebuild the cohort map at the end of every poll, see Note
[Streaming subscription polling]. But, rebuilding the cohort map is not straight
forward because there are race conditions that need to be taken care of. Some of
the race conditions which can happen when the current cohorts are processing:

1. A cohort is removed concurrently
2. A subscriber is removed concurrently
3. A new subscriber has started a subscription and it should be placed in the correct cohort

In the current code, the rebuilding of the cohort map goes as the follows:

1. After snapshotting the cohorts, we build a cohort map out of those cohorts,
   in the code these are called as "processedCohorts", it's important to note
   that these are not retrieved from the mutable "CohortMap", these are the
   snapshotted cohorts which were processed in the current poll. The reason we
   maintain the snapshotted cohorts is because it is later used while rebuilding
   the cohort map.

2. We create a processed cohort map which looks like HashMap CohortKey (Cohort
   'Streaming, CohortKey). The key of the map is the CohortKey which was
   associated with the cohort during the poll and the CohortKey in the value
   type is the cohort key after updating the cursor value. Note that both the
   values may or may not be equal.

3. We atomically get the list of the cohorts from the cohort map (mutable
reference), let's call it current cohorts and then traverse over it.

   1. Lookup with the given cohort key into the processed cohort map

      a. If no cohort is found, it means that the cohort with the given cohort
         key has been added while we were polling. So, we keep this as it is.

      b. If a processed cohort is found:

         i. We have to see if any new subscribers have been added to the current
            cohort, this is calculated using the diff of existing subscribers in
            the current cohort and the existing cohort, if there are any then we
            create a new cohort which includes only the newly added subscribers
            and add the new cohort into the cohort map.

         ii. We only retain the subscribers found in the processed cohort which
             exist in the current cohort. We do this because it is possible that
             a subscriber has been stopped their subscription in the time
             between the cohorts were snapshotted for processing and the time
             the cohort map is rebuilt.

         iii. Insert the processed cohort with the updated cohort key into the
         cohort map.

