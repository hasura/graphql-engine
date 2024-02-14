module Hasura.Tracing (module Tracing) where

import Hasura.Tracing.Class as Tracing
import Hasura.Tracing.Context as Tracing
import Hasura.Tracing.Monad as Tracing
import Hasura.Tracing.Propagator as Tracing
import Hasura.Tracing.Propagator.B3 as Tracing
import Hasura.Tracing.Propagator.W3CTraceContext as Tracing
import Hasura.Tracing.Reporter as Tracing
import Hasura.Tracing.Sampling as Tracing
import Hasura.Tracing.TraceId as Tracing
import Hasura.Tracing.TraceState as Tracing
import Hasura.Tracing.Utils as Tracing

{- Note [Tracing]

\## Usage

The Tracing library allows us to trace arbitrary pieces of our code, providing
that the current monad implements 'MonadTrace'.

    newTrace "request" do
      userInfo    <- newSpan "authentication" retrieveUserInfo
      parsedQuery <- newSpan "parsing" $ parseQuery q
      result      <- newSpan "execution" $ runQuery parsedQuery userInfo
      pure result

\## Trace and span

Each _trace_ is distinct, and is composed of one or more _spans_. Spans are
organized as a tree: the root span covers the entire trace, and each sub span
keeps track of its parent.

We report each span individually, and to each of them we associate a
'TraceContext', that contains:
  - a trace id, common to all the spans of that trace
  - a unique span id, generated randomly
  - the span id of the parent span, if any
  - whether that trace was sampled (see "Sampling").

All of this can be retrieved for the current span with 'currentContext'.

Starting a new trace masks the previous one; in the following example, "span2"
is associated to "trace2" and "span1" is associated to "trace1"; the two trees
are distinct:

    newTrace "trace1" $
      newSpan "span1" $
        newTrace "trace2" $
          newSpan "span2"

Lastly, a span that is started outside of a root trace is, for now, silently
ignored, as it has no trace id to attach to. This is a design decision we may
revisit.

\## Metadata

Metadata can be attached to the current span with 'attachMetadata', as a list
of pair of text key and text values.

\## Reporters

'TraceT' is the de-facto implementation of 'MonadTrace'; but, in practice, it
only does half the job: once a span finishes, 'TraceT' delegates the job of
actually reporting / exporting all relevant information to a 'Reporter'. Said
reporter must be provided to 'runTraceT', and is a wrapper around a function in
IO that processes the span.

In practice, 'TraceT' is only a reader that keeps track of the reporter, the
default sampling policy, and the current trace.

\## Sampling

To run 'TraceT', you must also provide a 'SamplingPolicy': an IO action that,
when evaluated, will decide whether an arbitrary trace should be reporter or
not. This decision is only made once per trace: every span within a trace will
use the same result: they're either all reporter, or none of them are.

When starting a trace, the default sampling policy can be overriden. You can for
instance run 'TraceT' with an action that, by default, only reports one out of
every ten traces, but use 'newTraceWithPolicy sampleAlways' when sending
critical requests to your authentication service.

Note that sampling and reporting are distinct: using 'sampleAlways' simply
guarantees that the 'Reporter' you provided will be called.

-}
