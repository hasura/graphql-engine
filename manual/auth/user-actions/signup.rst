User Signup
===========

The signup endpoint of Hasura Auth works in the following way. It takes two
parameters.

1. ``provider`` : A name of the provider to be used.
2. ``data``: A JSON payload which is specific to each provider. See provider
   examples.

If the signup request is successful:

1. If the provider doesn't have a verification step (e.g ``username``,
   ``google``), it will login the user and return the session token in the
   response.
2. If the provider has any verification step (e.g. ``mobile``, ``email``), it
   will not login the user, and will return ``null`` as the session token.

   Make a request to the endpoint: ``/v1/signup``.

.. code-block:: http

   GET auth.<cluster-name>.hasura-app.io/v1/signup HTTP/1.1
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
     "auth_token": "f876712c2fdbb3xcvxcvxvc2b376a3ad2dg31324bfee",
     "username": "johnsmith",
     "hasura_id": 2,
     "hasura_roles": [
       "user"
     ]
   }

