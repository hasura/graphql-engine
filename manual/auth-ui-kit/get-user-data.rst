.. .. meta::
   :description: Using Hasura Auth UI Kit and getting user information after logging in.
   :keywords: hasura, users, auth, uikit, login, user_information


.. _uikit-usecase-get-user-data:

Get User Data
=============

Once the user is redirected to your application from Auth UI Kit after login flow, you would want to fetch further information about the user from the database (typically a users or profile table in your application).

To get the logged in user's details, or to check if a session token is valid
you can use this endpoint.

Make a request to the endpoint: ``/v1/user/info``.

.. code-block:: http

   GET auth.<cluster-name>.hasura-app.io/v1/user/info HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <auth_token>


Typical response is :

.. code-block:: http

   HTTP/1.1 200 OK
   Content-Type: application/json

   {
     "auth_token": "b4b345f980ai4acua671ac7r1c37f285f8f62e29f5090306",
     "email": "johndoe@example.com",
     "hasura_id": 79,
     "hasura_roles": [
       "user"
     ]
   }

* ``auth_token``  is the authentication token of the user for the current
  session.
* ``hasura_roles``  is an array of all roles assigned to the user.

* ``hasura_id``  is the hasura identifier of the user.

Now using the ``hasura_id`` from the response, you can make further calls to Data API for fetching profile information.



