.. meta::
   :description: Overview of core Hasura Auth concepts- Identity (verified identity & social login based identity), Roles and Session management (cookies & auth tokens) 
   :keywords: hasura, docs, auth, introduction, identity, roles, sessions

Introduction and Core Concepts
===============================

Hasura Auth is an HTTP API for authentication, authorization and for
creating and managing Hasura identities. Each identity is also assigned a role.

You can tie these identities and roles to your own concept of users in your
database.

If you have worked with any auth service before, be it third-party or your own,
most of the flows and concepts are similar, like user registration, user login,
email/mobile verification, password reset functionality etc.

What is a Hasura Identity?
--------------------------

A Hasura Identity is a simple way to attach some kind of a "verified identity"
to every user. This "verification" is done by Hasura Auth and it attaches a
Hasura identity for every logged-in user.

  .. image:: ../images/HasuraIdentity.png


A Hasura identity is an integer. You can use this value in your application to
tie your application's user to this identity.

What are Roles?
---------------

Every request to the Hasura Auth platform gets a role attached to it. If the
request has no existing session, then the role ``anonymous`` is attached. If
the request has an existing session already, then atleast the ``user`` role is
attached. Depending on the user's already assigned roles, a user may also have
the ``admin`` role.

There are some pre-defined roles in the system. They are ``anonymous``, ``user``
and ``admin``. You can add more roles to the auth system by using it's APIs.
You can also assign/unassign roles to an existing user using the APIs.

What are sessions?
------------------

When a user is logged-in, along with its Hasura identity, a session is attached.

A session is nothing but a unique, un-guessable identifier  attached to that
Hasura identity (referred to as ``auth_token``) for that session. This way an
user can make subsequent requests without having to authenticate, with
credentials, on every request. Instead, on every request the user can present
the ``auth_token`` to identify themself.

.. _session-expiry:

A user session will expire:

* when a logout action is requested
* password is changed
* role is assigned or unassigned
* sessions are expired explicitly by any admin user

If you are building browser-based apps, then Hasura Auth already sends
appropriate cookie headers to manage the session. You don't have to do any
additional work to manage sessions, except making the appropriate API calls.

If you are building mobile/device apps, then you have to device your own
mechanism of storing the authentication tokens (``auth_token``) and managing
them. That means, storing and updating them whenever a Hasura Auth API returns
a new authentication token, and remove all existing authentication tokens
(``auth_token``) on the above :ref:`conditions <session-expiry>`.

