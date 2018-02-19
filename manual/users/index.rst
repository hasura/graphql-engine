.. .. meta::
   :description: Learn how to use Hasura Users
   :keywords: hasura, users, signup, login, email, mobile, email verification, mobile verification, password forgot, password reset, social login, google, facebook, github, linkedin


Users & Authentication
======================

Hasura Auth APIs let's you create, authenticate and manage user accounts on
your Hasura project. It also let's you manage sessions and roles for users.

Like any other authentication API, Hasura Auth has support for multiple ways to
authenticate an user (e.g. username-based, email-based, mobile-based etc.).
Hasura Auth calls each authentication method a "provider".

Once a user is registered (or signed-up) on Hasura, it attaches a Hasura
Identity or (``hasura_id``) to every user.  A Hasura identity is an integer.
You can use this value in your application to tie your application's user to
this identity.

Hasura Auth APIs also has a bunch of admin APIs to perform administrative tasks
on your user accounts.

Accessing the API
-----------------

The base URL for the Auth microservice would be:
``https://auth.<cluster-name>.hasura-app.io``.

All APIs accept and respond only in JSON. Which means that you have to make
sure that all requests you send, should contain the ``Content-Type:
application/json`` header.

API Console
~~~~~~~~~~~
Use the API console to browse various Auth APIs. ``cd`` to the project
directory and run:

.. code-block:: bash

  $ hasura api-console



.. _providers:

Providers
---------

Hasura Auth has providers to support different modes of authentication.
It has the following providers by default.

.. list-table::
   :header-rows: 1

   * - Provider
     - Provider Name (in API)
     - Description
   * - Username
     - ``username``
     - Basic username and password based authentication.
   * - Email 
     - ``email``
     - Email and password based authentication, with email verification.
   * - Mobile/OTP
     - ``mobile``
     - Mobile-based password-less authentication. For signup and login an OTP is sent to the mobile number.
   * - Mobile/Password
     - ``mobile-password``
     - Mobile and password based authentication, with mobile verification.
   * - Google
     - ``google``
     - Google login based authentication.
   * - Facebook
     - ``facebook``
     - Facebook login based authentication.
   * - Linkedin
     - ``linkedin``
     - Linkedin login based authentication.
   * - Github
     - ``github``
     - Github login based authentication.


Choose a provider from above based on your requirement.

You can enable/disable providers in your auth configuration. Once a provider is
enabled you can use them to signup your users. You **can** have multiple
providers enabled at the same time.

You can also create your custom provider (i.e if you have any custom
authentication logic) and configure it with Hasura Auth.


Signing Up
----------

The signup endpoint of Hasura Auth works in the following way. It takes two
parameters.

1. ``provider`` : A name of the provider to be used.
2. ``data``: A JSON payload which is specific to each provider. See provider
   examples.

If the signup request is successful:

1. If the provider doesn't have a verification step (e.g ``username``,
   ``google``), it will login the user and return the session token in the
   response.
2. If the provider has any verification step (e.g. ``mobile``, ``email``), it
   will not login the user, and will return ``null`` as the session token.
   
   Make a request to the endpoint: ``/v1/user/signup``.

.. code-block:: http

   GET auth.<cluster-name>.hasura-app.io/v1/user/signup HTTP/1.1
   Content-Type: application/json
   
   {
     "provider": "username",
     "data": {
       "username": "johnsmith",
       "password": "js@hasura"
     }
   }


Typical response is :

.. code-block:: http

   HTTP/1.1 200 OK
   Content-Type: application/json

   {
     "auth_token": "f876712c2fdbb3xcvxcvxvc2b376a3ad2dg31324bfee",
     "username": "johnsmith",
     "hasura_id": 2,
     "hasura_roles": [
       "user"
     ]
   }


Logging In
----------

The request payload of the login endpoint is similar to the signup endpoint.

1. ``provider`` : A name of the provider to be used.
2. ``data``: A JSON payload which is specific to each provider. See provider
   examples.
   
   
   Make a request to the endpoint: ``/v1/user/login``.

.. code-block:: http

   GET auth.<cluster-name>.hasura-app.io/v1/user/login HTTP/1.1
   Content-Type: application/json
   
   {
     "provider": "username",
     "data": {
       "username": "johnsmith",
       "password": "js@hasura"
     }
   }


Typical response is :

.. code-block:: http

   HTTP/1.1 200 OK
   Content-Type: application/json

   {
     "auth_token": "f876712c2fdbfdbbcbcvbcvbvc8606931324bfee",
     "username": "johnsmith",
     "hasura_id": 2,
     "hasura_roles": [
       "user"
     ]
   }


Pending Verification
^^^^^^^^^^^^^^^^^^^^
If the provider is ``email`` or ``mobile-password`` and the user has not
verified their email/mobile; then the response from ``/v1/login`` endpoint
would be an error indicating email/mobile verification is pending.


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


Logout
------

To logout a user, make the following request.

.. code-block:: http

   POST auth.<cluster-name>.hasura-app.io/v1/user/logout HTTP/1.1
   Authorization: Bearer <auth_token>

.. note::
    The logout request is a POST request with an empty body.



.. toctree::
   :maxdepth: 2
   :hidden:

   username
   email
   mobile
   mobile_password
   extra-fields
   sessions
   google
   facebook
   github
   linkedin
   custom_provider
   config
   admin_endpoints
   uikit


.. _recaptcha: https://www.google.com/recaptcha/intro/index.html
.. _Google: https://google.com
.. _Facebook: https://facebook.com
.. _LinkedIn: https://linkedin.com
.. _Github: https://github.com
