Admin: Add role to user
=======================

To add/assign a role to a user, make a request to the admin endpoint : ``/v1/admin/user/add-role``

.. code-block:: http

   POST auth.<cluster-name>.hasura-app.io/v1/admin/user/add-role HTTP/1.1
   Content-Type: application/json

   {
      "hasura_id": 3,
      "role": "customer"
   }


If the request is successful, the API will return response with user details.

Typical response of the ``/v1/admin/user/add-role`` request is :

.. code-block:: http

   HTTP/1.1 200 OK
   Content-Type: application/json

   {
      "hasura_id": 3,
      "hasura_roles": [
        "user", "merchant", "something", "customer"
      ],
      "auth_token": null,
      "email": "something@email.com",
      "mobile": "919999999999",
      "username": "something123",
      "created": "2014-11-05T08:15:30-05:00",
      "last_login": "2014-11-05T08:15:30-05:00",
      "is_active": true,
   }


* ``auth_token``  is the authentication token of the user for the current session.
  This is null because admin add role user api will not create session for new user created.

* ``hasura_roles``  is a list of all roles assigned to the user. Here, we can see role added to the user.

* ``hasura_id``  is the hasura identifier of the user.

* ``is_active``  gives status of the user.

