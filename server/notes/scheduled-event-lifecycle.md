This note is in [Hasura.Eventing.ScheduledTrigger](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Eventing/ScheduledTrigger.hs#L453).

# Scheduled event lifecycle

Scheduled events move between six different states over the course of their
lifetime, as represented by the following flowchart:
  ┌───────────┐      ┌────────┐      ┌───────────┐
  │ scheduled │─(a)─→│ locked │─(b)─→│ delivered │
  └───────────┘      └────────┘      └───────────┘
          ↑              │           ┌───────┐
          └────(c)───────┼─────(d)──→│ error │
                         │           └───────┘
                         │           ┌──────┐
                         └─────(e)──→│ dead │
                                     └──────┘

When a scheduled event is first created, it starts in the 'scheduled' state,
and it can transition to other states in the following ways:
  a. When graphql-engine fetches a scheduled event from the database to process
     it, it sets its state to 'locked'. This prevents multiple graphql-engine
     instances running on the same database from processing the same
     scheduled event concurrently.
  b. When a scheduled event is processed successfully, it is marked 'delivered'.
  c. If a scheduled event fails to be processed, but it hasn’t yet reached
     its maximum retry limit, its retry counter is incremented and
     it is returned to the 'scheduled' state.
  d. If a scheduled event fails to be processed and *has* reached its
     retry limit, its state is set to 'error'.
  e. If for whatever reason the difference between the current time and the
     scheduled time is greater than the tolerance of the scheduled event, it
     will not be processed and its state will be set to 'dead'.

