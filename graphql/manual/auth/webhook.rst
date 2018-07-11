Authorization Webhook
=====================

You can configure a webhook (see :doc:`GraphQL Server Options <../deployment/options>`) to authenticate all incoming requests to Hasura GraphQL server.

.. image:: ../../../img/graphql/manual/auth/webhook-auth.png

.. note::
   Configuring webhook requires Hasura to run with an access key (``--access-key``).
..   :doc:`Read more<config>`.


- The configured webhook is  **called** when ``X-Hasura-Access-Key`` header is not found in the request.
- The configured webhook is **ignored** when ``X-Hasura-Access-Key`` header is found in the request.


Spec for the webhook
--------------------

Request
^^^^^^^
Hasura will send ``GET`` request to you webhook with **all headers it received from client**

.. code-block:: http

   GET https://<your-custom-webhook>/ HTTP/1.1
   <Header-Key>: <Header-Value>

Response
^^^^^^^^

Success
+++++++
To allow the GraphQL request to go through, your webhook must return a ``200`` status code.
You should send the ``X-Hasura-*`` "session variables" your permission rules in Hasura.

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
   All values should be ``String``, they will be converted to the right type automatically.

Failure
+++++++
If you want to deny the GraphQL request return a ``401 Unauthorized`` exception.

.. code-block:: http

   HTTP/1.1 401 Unauthorized

.. note::
   Anything other than ``200`` or ``401`` response from webhook then server raises ``500 Internal Server Error`` exception

Sample Auth Webhook
-------------------

We have a sample webhook written in NodeJS that you can deploy with a single click. You can either `check out the code here <https://github.com/hasura/sample-auth-webhook/blob/master/server.js#L25>`_ or click on the button below to directly deploy it with `glitch <https://glitch.com/>`_.

.. image:: https://raw.githubusercontent.com/hasura/sample-auth-webhook/master/assets/deploy-glitch.png
  :width: 200px
  :alt: deploy_auth_webhook_with_glitch
  :class: no-shadow
  :target: http://glitch.com/edit/#!/import/github/hasura/sample-auth-webhook

Once deployed, you can use any of the following endpoints as your auth webhook in the GraphQL engine:

- ``/simple/webhook``
- ``/auth0/webhook``
- ``/firebase/webhook``

.. note::

    If you are using ``auth0`` or ``firebase`` you will have to set the associated environment variables.
