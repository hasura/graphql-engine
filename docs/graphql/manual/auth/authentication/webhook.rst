Authentication using webhooks
=============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

You can configure GraphQL engine to use a webhook to authenticate all incoming requests to the Hasura GraphQL engine server. 

.. thumbnail:: ../../../../img/graphql/manual/auth/webhook-auth.png

.. admonition:: Prerequisite
   
   It is mandatory to first :doc:`secure your GraphQL endpoint <../../deployment/securing-graphql-endpoint>` for the webhook mode to take effect.

In webhook mode, on a secured endpoint:

- The configured webhook is  **called** when ``X-Hasura-Admin-Secret`` header is not found in the request.
- The configured webhook is **ignored** when ``X-Hasura-Admin-Secret`` header is found in the request and
  admin access is granted.

Configuring webhook mode
------------------------

* You can configure Hasura to run in webhook mode by running GraphQL engine with the ``--auth-hook`` flag or the ``HASURA_GRAPHQL_AUTH_HOOK`` environment variable (see :doc:`GraphQL engine server options <../../deployment/graphql-engine-flags/reference>`), the value of which is the webhook endpoint.

* You can configure Hasura to send either a ``GET`` or a ``POST`` request to your auth webhook. The default configuration is ``GET`` and you can override this with ``POST`` by using the ``--auth-hook-mode`` flag or the ``HASURA_GRAPHQL_AUTH_HOOK_MODE`` environment variable (*in addition to those specified above; see* :doc:`GraphQL engine server options <../../deployment/graphql-engine-flags/reference>`).

Spec for the webhook
--------------------

Request
^^^^^^^


GET request
+++++++++++

.. code-block:: http

   GET https://<your-custom-webhook>/ HTTP/1.1
   <Header-Key>: <Header-Value>


If you configure your webhook to use ``GET``, then Hasura **will forward all client headers except**:

- ``Content-Length``
- ``Content-Type``
- ``Content-MD5``
- ``User-Agent``
- ``Host``
- ``Origin``
- ``Referer``
- ``Accept``
- ``Accept-Encoding``
- ``Accept-Language``
- ``Accept-Datetime``
- ``Cache-Control``
- ``Connection``
- ``DNT``

POST request
++++++++++++

.. code-block:: http

   POST https://<your-custom-webhook>/ HTTP/1.1
   Content-Type: application/json

   {
    "headers": {
        "header-key1": "header-value1",
        "header-key2": "header-value2"
      }
   }

If you configure your webhook to use ``POST``, then Hasura **will send all client headers in payload**

Response
^^^^^^^^

Success
+++++++
To allow the GraphQL request to go through, your webhook must return a ``200`` status code.
You should send the ``X-Hasura-*`` "session variables" to your permission rules in Hasura.

.. code-block:: http

   HTTP/1.1 200 OK
   Content-Type: application/json

   {
       "X-Hasura-User-Id": "25",
       "X-Hasura-Role": "user",
       "X-Hasura-Is-Owner": "true",
       "X-Hasura-Custom": "custom value"
   }

.. note::
   All values should be ``String``. They will be converted to the right type automatically.

Failure
+++++++
If you want to deny the GraphQL request return a ``401 Unauthorized`` exception.

.. code-block:: http

   HTTP/1.1 401 Unauthorized

.. note::
   Anything other than a ``200`` or ``401`` response from webhook makes server raise a ``500 Internal Server Error``
   exception.

Auth webhook samples
--------------------

We have put together a `GitHub Node.js repo <https://github.com/hasura/graphql-engine/tree/master/community/boilerplates/auth-webhooks/nodejs-express>`__ that has some sample auth
webhooks configured.

You can deploy these samples using `glitch <https://glitch.com/>`__:

.. image:: https://raw.githubusercontent.com/hasura/sample-auth-webhook/master/assets/deploy-glitch.png
   :width: 200px
   :alt: deploy_auth_webhook_with_glitch
   :class: no-shadow
   :target: http://glitch.com/edit/#!/import/github/hasura/sample-auth-webhook

Once deployed, you can use any of the following endpoints as your auth webhook in the GraphQL engine:

- ``/simple/webhook``  (`View source <https://github.com/hasura/graphql-engine/blob/master/community/boilerplates/auth-webhooks/nodejs-express/server.js>`__)
- ``/firebase/webhook`` (`View source <https://github.com/hasura/graphql-engine/blob/master/community/boilerplates/auth-webhooks/nodejs-express/firebase/firebaseHandler.js>`__)

.. note::

   If you are using ``firebase`` you will have to set the associated environment variables.