.. .. meta::
   :description: Hasura Auth user sessions
   :keywords: hasura, users, sessions


Understanding user sessions
===========================

Whenever a user logs in, a session is created and attached to the user.

A session is identified by an ``auth_token``, which is a unique, un-guessable
identifier attached to that user's account. This way a user can make subsequent
requests without having to authenticate with credentials on every request. Instead,
on every request, the user can present the ``auth_token`` to identify themself.
Every new session of a user will have a new ``auth_token`` created for it.
This ``auth_token`` is mapped with the user's information and stored in the session store.

Every microservice benefits from having the user's information (id and roles) with
each request. In the Hasura platform, every request goes through the :doc:`API Gateway <../gateway/index>`.
The API Gateway integrates with the session store to act as a
session middleware for all microservices.

When the gateway receives a request, it looks for a session auth token in the
``Bearer`` token of the ``Authorization`` header or in the ``cookie``. It then
retrieves the user ``hasura_id`` and roles attached to this user from the
session store. This information is sent as ``X-Hasura-User-Id`` and
``X-Hasura-Role`` headers to the upstream microservice.
If the ``X-Hasura-Role`` header is passed with the request, its value is passed to the upstream service if the
user has that particular role or else the request is rejected with a ``403 Forbidden`` response.

When the session token is absent from both the Authorization header and cookie, the gateway
considers it as an ``anonymous`` request and adds the header ``X-Hasura-Role:
anonymous``. The ``X-Hasura-User-Id`` header is **not set** in this case.

For example, the image below demonstrates the gateway's behaviour when two different kinds of incoming requests are made to ``data.test42.hasura-app.io`` from an HTTP client:

.. image:: ../../img/manual/auth/session-middleware.png

.. _session-expiry:

Session Expiry
--------------
A user session will expire:

* when a logout action is requested
* password is changed
* role is assigned or unassigned
* sessions are expired explicitly by any admin user

Handling/Storing session tokens
-------------------------------

Web-apps
~~~~~~~~
If you are building browser-based apps, then you don't have to do any additional work to
manage sessions tokens. Hasura Auth APIs send appropriate cookie headers on session creation.
The browser then handles setting the cookie for you on subsequent requests.

Mobile / other device apps
~~~~~~~~~~~~~~~~~~~~~~~~~~
If you are building mobile/device apps, then you have to make your own
mechanism for managing session tokens. That is - storing the token when a session is created
and deleting it when a session has expired.
