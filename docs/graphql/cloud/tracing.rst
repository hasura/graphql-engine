.. meta::
   :description: Distributed tracing with Hasura Cloud
   :keywords: hasura, docs, cloud, tracing

.. _tracing:

Distributed tracing
=======================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

Hasura Cloud has support for *distributed tracing*, a technique for
debugging Hasura in production in the context of the services it works
with. These services might include your own Postgres database, any
remote schemas, event trigger webhook providers, action providers or
authentication hooks. Distributed tracing attempts to give a unified
view into the performance characteristics of all of these components of
your architecture.

Visualizing traces
------------------

The Hasura Pro console makes it possible to view Hasura's own tracing
data, by opening the details view for an operation in the Operations
tab:

.. thumbnail:: /img/graphql/cloud/tracing/tracing-operations-timing.png
   :alt: View timing data in the Operations tab

Given that other system components will report their own tracing data to
your APM system, and not to Hasura, it is not possible to give a
complete picture of a trace, but since Hasura sits in a central position
in the architecture of many systems, it can often give a reasonably
comprehensive view of the provenance of data in your system.

For example, Hasura can report interactions with Postgres, remote
schemas, event trigger webhooks and action handlers.

APM system integration
----------------------

Hasura will report trace information to your APM or *application
performance monitoring* system, where it can be correlated with similar
sources of data from other components of your service architecture.

If you are considering integrating Hasura with your APM system, please
get in touch so that we can help to coordinate that effort.

Trace propagation
-----------------

At the boundaries between different services, tracing information needs
to be shared in order for trace fragments from different systems to be
correlated with each other in the APM system. This is called *trace
propagation*.

There are several subtly-incompatible proposals for trace propagation,
which can make it difficult to arrange for any two services to work
together.

Propagation to web services
~~~~~~~~~~~~~~~~~~~~~~~~~~~

For propagation during a call to a web service over HTTP, Hasura
currently implements the `B3 propagation
specification <https://github.com/openzipkin/b3-propagation>`__. This
means that we send trace information in various HTTP headers, which
should be read and handled by any compatible web services.

If you are unsure how to implement B3 propagation in your own web
service, the simplest thing to do is to read these headers and pass them
along to any HTTP services you call which also support B3 propagation,
including Hasura itself.

In particular, if an event trigger webhook or action handler propagates
these B3 headers back to Hasura, we will be able to trace the entire
interaction.

Propagation via Postgres
~~~~~~~~~~~~~~~~~~~~~~~~

There is no standard method for trace propagation via Postgres
transactions. For example, event triggers can be invoked by mutations,
and so their traces should be correlated.

For this reason, we have adopted our own method of propagating a trace
context in Postgres transactions.

The trace context will be serialized during mutations as a
transaction-local variable, ``hasura.tracecontext``. This value has the
Postgres type ``json``, and it can be read in trigger functions and
propagated to any downstream services.