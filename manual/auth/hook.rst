Authorization Webhook
=====================

You can configure a webhook (see :doc:`GraphQL Server Options <../installation/ravenOpts>`) to authenticate all incoming requests to Hasura GraphQL server.

.. note::
   1. If webhook is not configured then server expects required ``X-Hasura-*`` headers
   2. Configuring webhook requires Access key (``--access-key``) to be set


- Configured webook is called when ``X-Hasura-Access-Key`` header is not found in the request
- Configured webhook is ignored when ``X-Hasura-Access-Key`` header is found in the request.


Spec for the webhook
--------------------

Request
^^^^^^^
Server will send ``GET`` request to you webhook with **all headers it received from client**

.. code-block:: http

   GET https://<your-custom-webhook>/ HTTP/1.1
   <Header-Key>: <Header-Value>

Response
^^^^^^^^

Success
+++++++
Successful responsne means the webhook authorized request and server will continue to perform query. Only ``200`` response is considered successful.

If request is successful you should send required ``X-Hasura-*`` headers in parameters object in response.

.. code-block:: http

   HTTP/1.1 200 OK
   Content-Type: application/json

   {
       "parameters": {
           "X-Hasura-User-Id": "25",
           "X-Hasura-Role": "user",
           "X-Hasura-Is-Owner": "true",
           "X-Hasura-Custom": "custom value",
       }
   }

.. note::
   All values in ``parameters`` object should be ``String``

Failure
+++++++
Failure response means the webhook unauthorized the request. Then raises ``401 Unauthorized`` exception. 

.. code-block:: http

   HTTP/1.1 401 Unauthorized

If anything other than ``200`` or ``401`` response from webhook then server raises ``500 Internal Server Error`` exception
