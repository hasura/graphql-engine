.. meta::
   :description: Authorization in your remote schema server with Hasura
   :keywords: hasura, docs, remote schema, authorization

.. _schema_auth:

Authorization in your remote schema server
------------------------------------------

Hasura will forward the resolved ``x-hasura-*`` values as headers to your remote
schema. You can use this information to apply authorization rules in your
server. You don't have to redo authentication in your remote schema server.

You can also configure Hasura to have (as shown :ref:`here <merge_remote_schema>`):

1. static header values that are sent to the remote server
2. forward all headers from the client (like ``Authorization``, ``Cookie`` headers etc.)

In case there are multiple headers with same name, the order of precedence is:
configuration headers > resolved user (``x-hasura-*``) variables > client headers.

So for example, if the client sends an ``Authorization`` header, and the
configuration also has an ``Authorization`` header, the configuration header value
will selected.

.. note::

   The headers from the client behave similarly to the authorization system. If
   ``x-hasura-admin-secret`` is sent, then all ``x-hasura-*`` values from the
   client are respected, otherwise they are ignored.

Cookie header from your remote GraphQL servers
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
``Set-Cookie`` headers from your remote schema servers are sent back to the
client over HTTP transport. **Over websocket transport there exists no means 
of sending headers after a query/mutation and hence the ``Set-Cookie`` headers are 
not sent to the client.** Use HTTP transport if your remote servers set cookies.
