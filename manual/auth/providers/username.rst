.. .. meta::
   :description: Hasura Auth username provider
   :keywords: hasura, users, signup, login, username


Username based authentication
=============================

This provider supports basic username/password based authentication. The
identity of the user is not verified in this method.

.. _signup:

Signup
------

To signup a user, make a request to the signup endpoint : ``/v1/signup``.

.. code-block:: http

   POST auth.<cluster-name>.hasura-app.io/v1/signup HTTP/1.1
   Content-Type: application/json

   {
     "provider" : "username",
     "data" : {
        "username": "johnsmith",
        "password": "somepass123"
     }
   }

If the signup request is successful, the user is logged in.

Typical response of the ``/v1/signup`` request is :

.. code-block:: http

   HTTP/1.1 200 OK
   Content-Type: application/json


   {
     "auth_token": "b4b345f980ai4acua671ac7r1c37f285f8f62e29f5090306",
     "username": "johnsmith",
     "hasura_id": 79,
     "hasura_roles": [
         "user"
     ]
   }


* ``auth_token``  is the authentication token of the user for the current
  session.
* ``hasura_roles``  is an list of all roles assigned to the user.

* ``hasura_id``  is the hasura identifier of the user.


Login
------

To login a user make a request to the login endpoint: ``/v1/login``.

.. code-block:: http

   POST auth.<cluster-name>.hasura-app.io/v1/login HTTP/1.1
   Content-Type: application/json

   {
     "provider" : "username",
     "data" : {
        "username": "johnsmith",
        "password": "somepass123"
     }
   }

Typical response of the ``/v1/login`` request is :

.. code-block:: http

   HTTP/1.1 200 OK
   Content-Type: application/json

   {
     "auth_token": "b4b345f980ai4acua671ac7r1c37f285f8f62e29f5090306",
     "username": "johnsmith",
     "hasura_id": 79,
     "hasura_roles": [
         "user"
     ]
   }

* ``auth_token``  is the authentication token of the user for the current
  session.
* ``hasura_roles``  is an array of all roles assigned to the user.

* ``hasura_id``  is the hasura identifier of the user.


Getting user info
------------------
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
     "username": "johnsmith",
     "hasura_id": 79,
     "hasura_roles": [
         "user"
     ]
   }


* ``auth_token``  is the authentication token of the user for the current
  session.
* ``hasura_roles``  is an array of all roles assigned to the user.

* ``hasura_id``  is the hasura identifier of the user.


Logout
------

To logout a user, make the following request.

.. code-block:: http

   POST auth.<cluster-name>.hasura-app.io/v1/user/logout HTTP/1.1
   Authorization: Bearer <auth_token>

.. note::
    The logout request is a POST request with an empty body.

 
Changing Password
-----------------

If the user is logged in, they can change their password using the following
endpoint.

.. code-block:: http

   POST auth.<cluster-name>.hasura-app.io/v1/user/change-password HTTP/1.1
   Authorization: Bearer <auth_token>

   {
     "old_password": "oldpassword",
     "new_password": "newpassword"
   }


Forgot Password
---------------

The Username provider does not have a forgot password feature. This is by
design, as it does not have any channel (like email) to send a forgot password
token to the user.

If you want this feature, use either Email or Mobile/Password provider.
