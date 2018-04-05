Get user info (as admin)
========================

To get a user info, make a get request to the admin get user endpoint : ``/v1/admin/user/<user_id>``

.. code-block:: http

   GET auth.<cluster-name>.hasura-app.io/v1/admin/user/<user_id> HTTP/1.1

* ``user_id``  is the hasura_id of a user.

If request is successful, the API will return response with user details.

Typical response of the ``/v1/admin/user/<user_id>`` request is :

.. code-block:: http

  HTTP/1.1 200 OK
  Content-Type: application/json

  {
     "hasura_id": 2,
     "hasura_roles": [
       "user", "merchant", "something"
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
  This is null because admin get user api will not create session for new user created.

* ``hasura_roles``  is a list of all roles assigned to the user.

* ``hasura_id``  is the hasura identifier of the user.

* ``is_active``  gives status of the user.


