.. .. meta::
    :description: Hasura Auth Admin APIs
    :keywords: hasura, admin, users, remove session, add session, add user, remove user, user activate, user deactivate


Admin Endpoints
===============

Hasura Auth provides admin APIs to manage users, user sessions, roles etc. As name suggests
these APIs are open to users with admin role.


Get User Info
-------------

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



Create User
-----------

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


Delete User
-----------

To remove existing user, make a request to the delete-user endpoint : ``/v1/admin/delete-user``.
Hasura Auth deletes user in all providers.

.. code-block:: http

   POST auth.<cluster-name>.hasura-app.io/v1/admin/delete-user HTTP/1.1
   Content-Type: application/json

   {
      "hasura_id": 2
   }

If request is successful, the API will return a success message.

Typical response of the ``/v1/admin/delete-user`` request is :

.. code-block:: http

   HTTP/1.1 200 OK
   Content-Type: application/json

   {
      "details":{
          "username": "deleted",
          "email": "not-found",
          "mobile": "not-found",
          "mobile-password": "not-found",
          "google": "not-found",
          "facebook": "not-found",
          "github": "not-found",
          "linkedin": "not-found",
       },
      "message": "User with hasura_id = 2 is deleted"
   }

* ``hasura_id``  is the hasura identifier of the user.
* ``details``  gives delete status of all providers. Status messages are ``deleted``, ``not-found``, ``not-deleted`` or HTTP exceptions from delete hooks.


User Activate
-------------

To activate a user, make a request to the admin endpoint : ``/v1/admin/user/activate``

.. code-block:: http

   POST auth.<cluster-name>.hasura-app.io/v1/admin/user/activate HTTP/1.1
   Content-Type: application/json

   {
      "hasura_id": 3
   }


If the request is successful, the API will return response with user details.

Typical response of the ``/v1/admin/user/activate`` request is :

.. code-block:: http

   HTTP/1.1 200 OK
   Content-Type: application/json

   {
      "hasura_id": 3,
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
  This is null because admin activate user api will not create session for new user created.

* ``hasura_roles``  is a list of all roles assigned to the user.

* ``hasura_id``  is the hasura identifier of the user.

* ``is_active``  gives status of the user. Here, it is true.


User Deactivate
-------------

To deactivate a user, make a request to the admin endpoint : ``/v1/admin/user/deactivate``

.. code-block:: http

   POST auth.<cluster-name>.hasura-app.io/v1/admin/user/deactivate HTTP/1.1
   Content-Type: application/json

   {
      "hasura_id": 3
   }


If the request is successful, the API will return response with user details.

Typical response of the ``/v1/admin/user/deactivate`` request is :

.. code-block:: http

   HTTP/1.1 200 OK
   Content-Type: application/json

   {
      "hasura_id": 3,
      "hasura_roles": [
        "user", "merchant", "something"
      ],
      "auth_token": null,
      "email": "something@email.com",
      "mobile": "919999999999",
      "username": "something123",
      "created": "2014-11-05T08:15:30-05:00",
      "last_login": "2014-11-05T08:15:30-05:00",
      "is_active": false,
   }


* ``auth_token``  is the authentication token of the user for the current session.
  This is null because admin deactivate user api will not create session for new user created.

* ``hasura_roles``  is a list of all roles assigned to the user.

* ``hasura_id``  is the hasura identifier of the user.

* ``is_active``  gives status of the user. Here, it is false.


User Add Role
-------------

To add a role to a user, make a request to the admin endpoint : ``/v1/admin/user/add-role``

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



User Remove Role
-------------

To remove a role to a user, make a request to the admin endpoint : ``/v1/admin/user/remove-role``

.. code-block:: http

   POST auth.<cluster-name>.hasura-app.io/v1/admin/user/remove-role HTTP/1.1
   Content-Type: application/json

   {
      "hasura_id": 3,
      "role": "customer"
   }


If the request is successful, the API will return response with user details.

Typical response of the ``/v1/admin/user/remove-role`` request is :

.. code-block:: http

   HTTP/1.1 200 OK
   Content-Type: application/json

   {
      "hasura_id": 3,
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
  This is null because admin remove role user api will not create session for new user created.

* ``hasura_roles``  is a list of all roles assigned to the user. Here, we can see role removed to the user.

* ``hasura_id``  is the hasura identifier of the user.

* ``is_active``  gives status of the user.


User Create Session
-------------------

To create a session to a user, make a request to the admin endpoint : ``/v1/admin/user/create-session``

.. code-block:: http

   POST auth.<cluster-name>.hasura-app.io/v1/admin/user/create-session HTTP/1.1
   Content-Type: application/json

   {
      "hasura_id": 3,
      "expiry_time": 89371
   }

* ``expiry_time`` is an integer field in seconds. But it is optional.

If the request is successful, the API will return response with user details.

Typical response of the ``/v1/admin/user/create-session`` request is :

.. code-block:: http

   HTTP/1.1 200 OK
   Content-Type: application/json

   {
      "hasura_id": 3,
      "hasura_roles": [
        "user", "merchant", "something"
      ],
      "auth_token": "Asjowjj21oid32on54mcuwoeADSjsah78fiSjseoi28",
      "email": "something@email.com",
      "mobile": "919999999999",
      "username": "something123",
      "created": "2014-11-05T08:15:30-05:00",
      "last_login": "2014-11-05T08:15:30-05:00",
      "is_active": true,
   }


* ``auth_token``  is the authentication token of the user for the current session.

* ``hasura_roles``  is a list of all roles assigned to the user.

* ``hasura_id``  is the hasura identifier of the user.

* ``is_active``  gives status of the user.


User Remove Session
-------------------

To remove a session of a user, make a request to the admin endpoint : ``/v1/admin/user/remove-session``

.. code-block:: http

   POST auth.<cluster-name>.hasura-app.io/v1/admin/user/remove-session HTTP/1.1
   Content-Type: application/json

   {
      "hasura_id": 3,
      "session_id": "Asjowjj21oid32on54mcuwoeADSjsah78fiSjseoi28"
   }

You have to provide session token of given user.

If the request is successful, the API will return response with user details.

Typical response of the ``/v1/admin/user/remove-session`` request is :

.. code-block:: http

   HTTP/1.1 200 OK
   Content-Type: application/json

   {
      "hasura_id": 3,
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
  Here, session is removed.

* ``hasura_roles``  is a list of all roles assigned to the user.

* ``hasura_id``  is the hasura identifier of the user.

* ``is_active``  gives status of the user.



User Remove All Sessions
-------------------

To remove all sessions of a user, make a request to the admin endpoint : ``/v1/admin/user/remove-all-session``

.. code-block:: http

   POST auth.<cluster-name>.hasura-app.io/v1/admin/user/remove-all-sessions HTTP/1.1
   Content-Type: application/json

   {
      "hasura_id": 3
   }

If the request is successful, the API will return response with user details.

Typical response of the ``/v1/admin/user/remove-all-sessions`` request is :

.. code-block:: http

   HTTP/1.1 200 OK
   Content-Type: application/json

   {
      "hasura_id": 3,
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
  Here, all sessions are removed.

* ``hasura_roles``  is a list of all roles assigned to the user.

* ``hasura_id``  is the hasura identifier of the user.

* ``is_active``  gives status of the user.


User reset Password
-------------------

To reset password of a user, make a request to the admin endpoint : ``/v1/admin/user/reset-password``

.. code-block:: http

   POST auth.<cluster-name>.hasura-app.io/v1/admin/user/reset-password HTTP/1.1
   Content-Type: application/json

   {
      "hasura_id": 3,
      "new_password": "newpass123",
      "admin_password": "adminpass123"
   }

You have to provide admin password in order to reset users password.

If the request is successful, the API will return success message.

Typical response of the ``/v1/admin/user/remove-all-sessions`` request is :

.. code-block:: http

   HTTP/1.1 200 OK
   Content-Type: application/json

   {
      "message": "password updated"
   }
