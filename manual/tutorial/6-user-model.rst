.. .. meta::
   :description: Part 2 of a set of learning exercises meant for exploring Hasura in detail. This part introduces the Auth microservice's User & Session management model.
   :keywords: hasura, getting started, step 2

==================================
Part VI: Users, roles and sessions
==================================

..
   `User management on Hasura <https://www.youtube.com/watch?v=Qbon0cgbneo>`_
   The video is an introduction to user-management via the
   project console and basic user-management APIs. **After going through the 2 min video,
   you should read this page too!**

Let's list out the user management features required in our app:

* User registration, login, logout, reset password etc.
* There are two classes of visitors, logged in users and anonymous users. We need roles to model this.

Before we implement the functionality needed for our ether app, let's understand what the hasura platform offers.

Before you begin: Open the API console
--------------------------------------

Run:

.. code-block:: bash

   # Run this command inside your project directory
   $ hasura api-console

This will open up the ``api-console`` and show you the ``API explorer`` page which you can use to understand the APIs.

.. admonition:: Note

   You can try out all of the API examples below in the API explorer


The ``auth`` microservice
-------------------------

The ``auth`` microservice is a part of every hasura project. It has extensive APIs to manage users, roles and integrates with various social login providers. These are the endpoints that we are interested in to build our ether app.

#. Register a user

   .. code-block:: http

     POST auth.<cluster-name>.hasura-app.io/signup HTTP/1.1
     Content-Type: application/json

     {
       "provider" : "username",
       "data" : {
          "username": "johnsmith",
          "password": "somepass123"
       }
     }


   The response will be as follows:

   .. code-block:: http

      HTTP/1.1 200 OK
      Content-Type: application/json

      {
          "auth_token": "pand3yofqfqrz7kab8i7n4w9n2wgc6bg",
          "hasura_id": 2,
          "hasura_roles": [
             "user"
          ]
      }

   Every user created with the ``/signup`` endpoint is assigned the role ``user``.

#. Login

   .. code-block:: http

      POST auth.<cluster-name>.hasura-app.io/login HTTP/1.1
      Content-Type: application/json

      {
        "provider" : "username",
        "data" : {
           "username": "johnsmith",
           "password": "somepass123"
        }
      }


   The response will be as follows:

   .. code-block:: http
      :emphasize-lines: 3, 6

      HTTP/1.1 200 OK
      Content-Type: application/json
      Set-Cookie: dinoisses=pand3yofqfqrz7kab8i7n4w9n2wgc6bg; Domain=.<cluster-name>.hasura-app.io:01:34 GMT; httponly; Max-Age=1814400; Path=/

      {
          "auth_token": "pand3yofqfqrz7kab8i7n4w9n2wgc6bg",
          "hasura_id": 2,
          "hasura_roles": [
             "user"
          ]
      }

   When a user logs in, a new session token is created for the user. It is set as cookie and also returned in ``auth_token`` of the response. In the above example, the session token is ``pand3yofqfqrz7kab8i7n4w9n2wgc6bg``. The token and the associated user's information is stored in the session store.

#. User info

   .. code-block:: http
      :emphasize-lines: 2

      GET auth.<cluster-name>.hasura-app.io/user/account/info HTTP/1.1
      Authorization: Bearer pand3yofqfqrz7kab8i7n4w9n2wgc6bg

   Note the session token in ``Authorization`` header. If the ``Authorization`` header is absent, session token is read from cookie. The response will be as follows:

   .. code-block:: http

      HTTP/1.1 200 OK
      Content-Type: application/json

      {
          "auth_token": "pand3yofqfqrz7kab8i7n4w9n2wgc6bg",
          "hasura_roles": [
              "user"
          ],
          "mobile": null,
          "hasura_id": 2,
          "username": "hasura-test",
          "email": null
      }

   Now let's get the ``admin`` user's information. The admin token is in fact the ``admin`` user's session token. Let's use the admin token in the ``Authorization`` header.

   .. code-block:: http
      :emphasize-lines: 2

      GET auth.<cluster-name>.hasura-app.io/user/account/info HTTP/1.1
      Authorization: Bearer <admin-token>

   .. code-block:: http
      :emphasize-lines: 7

      HTTP/1.1 200 OK
      Content-Type: application/json

      {
          "auth_token": "<admin-token>",
          "hasura_roles": [
              "admin",
              "user"
          ],
          "mobile": "9999999999",
          "hasura_id": 1,
          "username": "admin",
          "email": "admin@admin.com"
      }

   You can see that the ``admin`` user has an ``admin`` role in addition to the ``user`` role.

#. Logout

   .. code-block:: http
      :emphasize-lines: 2

      GET auth.<cluster-name>.hasura-app.io/user/logout HTTP/1.1
      Authorization: Bearer pand3yofqfqrz7kab8i7n4w9n2wgc6bg

   .. code-block:: http
      :emphasize-lines: 3

      HTTP/1.1 200 OK
      Content-Type: application/json
      Set-Cookie: dinoisses=; Domain=.<cluster-name>.hasura-app.io; expires=Thu, 01-Jan-1970 00:00:00 GMT; Max-Age=0; Path=/

      {
          "message": "Logged out"
      }

   When the user logs out, the session token is deleted from the session store and the cookie is set to expire.

A bit about sessions
--------------------

Every microservice benefits from having the user's information (id and roles) with each request. In hasura platform, as mentioned earlier, every request goes through the gateway. So, the gateway integrates with the session store to act as a session middleware for all microservices.

When the gateway receives a request, it looks for a session token in the ``Bearer`` token of ``Authorization`` header or in the cookie. It then retrieves the user's id and roles attached to this session token from the session store. This information is sent as ``X-Hasura-User-Id`` and ``X-Hasura-Role`` headers to the upstream microservice.

When the session token is absent from both header and cookie, the gateway considers it an anonymous request and adds the header ``X-Hasura-Role: anonymous``. The ``X-Hasura-User-Id`` header is **not** set in this case.

For example, say there are 2 different kinds incoming requests to ``data.test42.hasura-app.io`` from an HTTP client:

.. image:: session-middleware.png

Next: Create your data models
-----------------------------

Next, head to :doc:`Data modelling for the blog app <7-data-modelling>`.
