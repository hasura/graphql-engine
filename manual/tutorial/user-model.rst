Part V: Users, roles and sessions
=================================

..
   `User management on Hasura <https://www.youtube.com/watch?v=Qbon0cgbneo>`_
   The video is an introduction to user-management via the
   api-console and basic user-management APIs. **After going through the 2 min video,
   you should read this page too!**

Let's list out the user management features required in our app:

* User registration, login, logout, user-info etc.
* There are two classes of visitors, logged in users and anonymous users. We need roles to model this.

Before we implement the functionality needed for our ``blog`` app, let's understand what the hasura platform offers.


The Hasura Auth Workflow
------------------------

When a user signs up using the ``/signup`` endpoint, they are assigned a unique identity i.e. a unique integer called ``hasura_id``. This unique integer can be used to link the user to the rest of the data in the database. Also, a role called ``user`` is assigned to every newly logged-in user. All users on a Hasura platform are identified with their ``hasura_id`` and their role.

Whenever a new user signs up or an existing user logs in, a session token called ``auth_token`` is generated that represents the user session. This ``auth_token`` is mapped with the user's information and stored in the *session_store*.


A bit about sessions
--------------------

Every microservice benefits from having the user's information (id and roles) with each request. In hasura platform, every request goes through the Hasura api gateway. So, the gateway integrates with the session store to act as a session middleware for all microservices.

When the gateway receives a request, it looks for a session token in the ``Bearer`` token of ``Authorization`` header or in the ``cookie``. It then retrieves the user's id and roles attached to this session token from the session store. This information is sent as ``X-Hasura-User-Id`` and ``X-Hasura-Role`` headers to the upstream microservice.
If the ``X-Hasura-Role`` header is passed with the request, its value is passed to the upstream service if the user has that particular role or else the request is rejected with a ``403 Forbidden`` response.

When the session token is absent from both header and cookie, the gateway considers it an anonymous request and adds the header ``X-Hasura-Role: anonymous``. The ``X-Hasura-User-Id`` header is **not** set in this case.

For example, the image below demonstrates the gateway's behaviour when two different kinds of incoming requests are made to ``data.test42.hasura-app.io`` from an HTTP client:

.. image:: ../../img/manual/auth/session-middleware.png

Before you begin: Open the API console
--------------------------------------

Run:

.. code-block:: bash

   # Run this command inside your project directory
   $ hasura api-console

This will open up the ``api-console`` and show you the ``API explorer`` page which you can use to understand the APIs.


The "auth" microservice
-----------------------

The ``auth`` microservice is a part of every hasura cluster. It has extensive APIs to manage users, roles and integrates with various social login providers.

Lets explore the endpoints that we are interested in to build our blog app using the api console:

#. **Register a user**

   Click on ``Signup`` from the *Username Password* dropdown under ``Auth`` in the left panel. Enter the username and password and hit ``Send``.

   .. image:: ../../img/tutorial-6-username-signup.png

   The response will be as follows:

   .. code-block:: http

      HTTP/1.1 200 OK
      Content-Type: application/json

      {
          "auth_token": "41ddk3k12kdf332397c526e58c24e74b63444baa5770929b",
          "hasura_id": 2,
          "hasura_roles": [
             "user"
          ]
      }

   Every user created with the ``/signup`` endpoint is assigned the role ``user``.

#. **Login**

   Lets try to login with the username and password that we just registered with. Select ``Login`` in the left panel.


   .. image:: ../../img/tutorial-6-username-login.png

   The response will be as follows:

   .. code-block:: http
      :emphasize-lines: 3, 6

      HTTP/1.1 200 OK
      Content-Type: application/json
      Set-Cookie: dinoisses=14e3e76d5bac289f299da3b1e5f86814b401464a99f28e67; Domain=.authorization76.hasura-app.io:01:34 GMT; httponly; Max-Age=1814400; Path=/

      {
          "auth_token": "14e3e76d5bac289f299da3b1e5f86814b401464a99f28e67",
          "hasura_id": 2,
          "hasura_roles": [
             "user"
          ]
      }

   When a user logs in, a new session token is created for the user. It is set as cookie and also returned in ``auth_token`` of the response. In the above example, the session token is ``14e3e76d5bac289f299da3b1e5f86814b401464a99f28e67``. The token and the associated user's information is stored in the session store. Copy this ``auth_token``.

#. **User info**

   You will often need to get user information of the logged in user. Lets try to get the user information of the user that we just registered and logged in. This API can be used only by a logged-in user. Select ``User Information`` under *Logged in User Actions* in the left panel.

   Copy the ``auth_token`` from the login response and paste it in the ``Authorization`` header as shown in the image below.

    If the ``Authorization`` header is absent, session token is read from cookie.

   .. image:: ../../img/tutorial-6-user-info.png

   Now let's get the ``admin`` user's information. The admin token is in fact the ``admin`` user's session token. Let's use the admin token in the ``Authorization`` header.

   .. image:: ../../img/tutorial-6-admin-user-info.png

   You can see that the ``admin`` user has an ``admin`` role in addition to the ``user`` role.

#. **Logout**

   To logout the logged in user, use the ``/user/logout`` endpoint. Select the ``Logout`` API from the *Logged in User Actions* dropdown in the left panel.

   .. image:: ../../img/tutorial-6-logout.png

   When the user logs out, the session token is deleted from the session store and the cookie is set to expire.

   .. code-block:: http
      :emphasize-lines: 3

      HTTP/1.1 200 OK
      Content-Type: application/json
      Set-Cookie: dinoisses=; Domain=.authorization76.hasura-app.io; expires=Thu, 01-Jan-1970 00:00:00 GMT; Max-Age=0; Path=/

      {
          "message": "Logged out"
      }


Generate API Code
-----------------

The API-console has a code-generator that can automatically generate the API code for all the above API (and more) in most popular languages. To generate API code, click on the ``Generate API Code`` button next to the ``Send`` button and choose your desired language.

Other Login Providers
---------------------

You can also perform auth with ``Mobile``, ``Email``, ``Google`` and several more providers. Read more :ref:`here <providers>`.


Next: Create your data models
-----------------------------

Next, head to :doc:`data-modelling`.
