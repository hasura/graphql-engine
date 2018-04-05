User Login
==========

The request payload of the login endpoint is similar to the signup endpoint.

1. ``provider`` : A name of the provider to be used.
2. ``data``: A JSON payload which is specific to each provider. See provider
   examples.


   Make a request to the endpoint: ``/v1/login``.

.. code-block:: http

   GET auth.<cluster-name>.hasura-app.io/v1/login HTTP/1.1
   Content-Type: application/json

   {
     "provider": "username",
     "data": {
       "username": "johnsmith",
       "password": "js@hasura"
     }
   }


Typical response is :

.. code-block:: http

   HTTP/1.1 200 OK
   Content-Type: application/json

   {
     "auth_token": "f876712c2fdbfdbbcbcvbcvbvc8606931324bfee",
     "username": "johnsmith",
     "hasura_id": 2,
     "hasura_roles": [
       "user"
     ]
   }


Pending Verification
^^^^^^^^^^^^^^^^^^^^
If the provider is ``email`` or ``mobile-password`` and the user has not
verified their email/mobile; then the response from ``/v1/login`` endpoint
would be an error indicating email/mobile verification is pending.

