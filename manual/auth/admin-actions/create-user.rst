Admin: Create user
==================

To create a new user, make a request to the admin create-user endpoint : ``/v1/admin/create-user``.
It is provider specific endpoint. Specify ``provider`` in request payload.

For Username provider :-

.. code-block:: http

    POST auth.<cluster-name>.hasura-app.io/v1/admin/create-user HTTP/1.1
    Content-Type: application/json

    {
      "provider": "username",
      "data":{
        "username": "something123",
        "password": "somepass124",
      },
      "is_active": true,
      "roles": ["user", "merchant", "something"]
    }

For Email provider :-

.. code-block:: http

    POST auth.<cluster-name>.hasura-app.io/v1/admin/create-user HTTP/1.1
    Content-Type: application/json

    {
      "provider": "email",
      "data":{
        "email": "something@email.com",
        "password": "somepass124",
      },
      "is_active": true,
      "roles": ["user", "merchant", "something"]
    }

For Mobile-Password provider :-

.. code-block:: http

    POST auth.<cluster-name>.hasura-app.io/v1/admin/create-user HTTP/1.1
    Content-Type: application/json

    {
      "provider": "mobile-password",
      "data":{
        "mobile": "9999999999",
        "country_code": "91",
        "password": "somepass124",
      },
      "is_active": true,
      "roles": ["user", "merchant", "something"]
    }

For Mobile Only provider :-

.. code-block:: http

    POST auth.<cluster-name>.hasura-app.io/v1/admin/create-user HTTP/1.1
    Content-Type: application/json

    {
      "provider": "mobile",
      "data":{
        "mobile": "9999999999",
        "country_code": "91",
      },
      "is_active": true,
      "roles": ["user", "merchant", "something"]
    }

If request is successful, the API will return response with user details.

Response is same for all providers. Typical response of the ``/v1/admin/create-user`` request is :

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
      "extra_info": {}
   }


* ``auth_token``  is the authentication token of the user for the current session.
  This is null because admin create user api will not create session for new user created.

* ``hasura_roles``  is a list of all roles assigned to the user.

* ``hasura_id``  is the hasura identifier of the user.

* ``is_active``  gives status of the user.

* ``extra_info``  gives extra information of the user from custom provider. For default providers it is null.

