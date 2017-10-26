.. meta::
   :description: Learn how to use Hasura Users
   :keywords: hasura, users, signup, login, email, mobile, email verification, mobile verification, password forgot, password reset, social login, google, facebook, github, linkedin


Users
=====

A Hasura Identity (or ``hasura_id``) is a simple way to attach some kind of a
"verified identity" to every user. This "verification" is done by Hasura Auth
and it attaches a Hasura identity for every logged-in user. Each identity is
also assigned a role.

  .. image:: ../images/HasuraIdentity.png

A Hasura identity is an integer. You can use this value in your application to
tie your application's user to this identity.

The base URL for the Auth service would be:
`https://auth.<project-name>.hasura-app.io`. This is also mentioned in your
project console.

All APIs accept and respond only in JSON. Which means that you have to make
sure that all requests you send, should contain the ``Content-Type:
application/json`` header.

.. _providers:

Providers
---------

Providers denote the specific mode of authentication. Hasura Auth supports the following providers by default
and allows allows to create href{custom providers}:

* **username**
* **email**
* **mobile** ( OTP based login )
* **mobile-password**
* **google**
* **facebook**
* **linkedin**
* **github**

.. _signup:

Signup
------

To signup a user, make a request to the signup endpoint : ``/v2/signup``.

Mandatory parameters in the request body are:

* Provider: ``provider``
* Data required by the specific provider: ``data``

.. code-block:: http

   POST auth.<project-name>.hasura-app.io/v2/signup HTTP/1.1
   Content-Type: application/json

   {
     "provider" : "username",
     "data" : {
        "username": "jsmith123456",
        "password": "somepass123"
     }
   }

If the signup request is successful, the user is logged in automatically depending on the provider else the user needs to login separately.

Typical response of the ``/v2/signup`` request is :

.. code-block:: http

   HTTP/1.1 200 OK
   Content-Type: application/json

   {
     "auth_token": "tpdq0m9whrj7i4vcjn48zq43bqx2",
     "hasura_roles": [
       "user"
     ],
     "hasura_id": 79
   }


* ``auth_token``  is the authentication token of the user for the current
  session.
* ``hasura_roles``  is an list of all roles assigned to the user.

* ``hasura_id``  is the hasura identifier of the user.


.. note::
  The ``auth_token``  is only returned if the user is logged in. If the user is
  not logged in, based on the provider type or the admin user
  disabling the user etc., then the value of the ``auth_token`` will be ``null``.


Login
------

To login a user make a request to the login endpoint: ``/v2/login``.

Mandatory parameters in the request body are:

* Provider: ``provider``
* Data required by the specific provider: ``data``

.. code-block:: http

   POST auth.<project-name>.hasura-app.io/v2/login HTTP/1.1
   Content-Type: application/json

   {
     "provider" : "email",
     "data" : {
        "email": "johndoe@gmail.com",
        "password": "somepass123"
     }
   }

Typical response of the ``/v2/login`` request is :

.. code-block:: http

   HTTP/1.1 200 OK
   Content-Type: application/json

   {
     "auth_token": "tpdq0m9whrj7i4vcjn48zq43bqx2",
     "hasura_roles": [
       "user"
     ],
     "hasura_id": 79
   }

* ``auth_token``  is the authentication token of the user for the current
  session.
* ``hasura_roles``  is an array of all roles assigned to the user.

* ``hasura_id``  is the hasura identifier of the user.


Pending Verification
^^^^^^^^^^^^^^^^^^^^
If the provider is ``email``/``mobile-password`` and the user has not verified their
email/mobile; then the response from ``/v2/login`` endpoint would be an error
indicating email/mobile verification is pending.



Sessions
--------

When a user is logged-in, along with its Hasura identity, a session is attached.

A session is nothing but a unique, un-guessable identifier  attached to that
Hasura identity (referred to as ``auth_token``) for that session. This way a
user can make subsequent requests without having to authenticate with
credentials on every request. Instead, on every request the user can present
the ``auth_token`` to identify themself.

Every service benefits from having the user's information (id and roles) with each request. In hasura platform, as mentioned earlier, every request goes through the gateway. So, the gateway integrates with the session store to act as a session middleware for all services.

When the gateway receives a request, it looks for a session token in the ``Bearer`` token of ``Authorization`` header or in the cookie. It then retrieves the user's id and roles attached to this session token from the session store. This information is sent as ``X-Hasura-User-Id`` and ``X-Hasura-Role`` headers to the upstream service.

When the session token is absent from both header and cookie, the gateway considers it an anonymous request and adds the header ``X-Hasura-Role: anonymous``. The ``X-Hasura-User-Id`` header is **not** set in this case.


.. _session-expiry:

Session Expiry
^^^^^^^^^^^^^^
A user session will expire:

* when a logout action is requested
* password is changed
* role is assigned or unassigned
* sessions are expired explicitly by any admin user

Sessions are managed by Hasura Auth and the Gateway. Whenever a request is
made Hasura Gateway resolves the session from Authorization header or Cookies.

If you are building browser-based apps, then Hasura Auth already sends
appropriate cookie headers to. You don't have to do any additional work to
manage sessions, except making the appropriate API calls.

If you are building mobile/device apps, then you have to device your own
mechanism of storing the authentication tokens (``auth_token``) and managing
them. That means, storing and updating them whenever a Hasura Auth API returns
a new authentication token, and remove all existing authentication tokens
(``auth_token``) on :ref:`these conditions <session-expiry>`.


Verifying Email
---------------

To verify the email address upon signup, Hasura Auth will send an email with a unique
token to the user's email address. The email template can be configured in auth.yaml
and must include the complete verification link alongwith the ``token`` parameter. Within a configurable amount of time, the
user has to hit the verification endpoint to verify the email
address.

.. note::
  For emails to be sent, you have to enable an email provider in Hasura Notify service.

.. code-block:: http

   GET auth.<project-name>.hasura-app.io/v2/providers/email/verify-email?token=<token> HTTP/1.1


The response of the email verification endpoint indicates success or failure.
If it is successful, then your application should ask the user to login.

.. code-block:: http

   HTTP/1.1 200 OK
   Content-Type: application/json

   {
     "message"   : "success"
   }


Verifying Mobile
----------------

To verify the mobile number, Hasura Auth will send a SMS with a one time
password or OTP to the user's mobile number, and within a configurable amount of
time, the user has to submit the OTP to a Hasura Auth API endpoint to verify
the mobile number.

.. note::
  For OTP to be sent, you have to enable SMS provider in Hasura Notify service.

For ``mobile-password`` provider, the request is:

.. code-block:: http

   POST auth.<project-name>.hasura-app.io/v2/providers/mobile-password/verify-otp HTTP/1.1
   Content-Type: application/json

   {
     "mobile": "9876543210",
     "country_code": 91,
     "otp"   : 123456
   }

The response of the mobile verification endpoint indicates success or failure.
If it is successful, then your application should ask the user to login.

.. code-block:: http

   HTTP/1.1 200 OK
   Content-Type: application/json

   {
     "message" : "success"
   }

For ``mobile`` provider, the verification is done during signup itself.

.. code-block:: http

   POST auth.<project-name>.hasura-app.io/v2/signup HTTP/1.1
   Content-Type: application/json

   {
     "provider": "mobile"
     "data": {
        "mobile": "9876543210",
        "country_code": 91,
        "otp"   : 123456
      }
   }

If it is successful, the signup response is sent to the user:

.. code-block:: http

   HTTP/1.1 200 OK
   Content-Type: application/json

   {
     "auth_token": "tpdq0m9whrj7i4vcjn48zq43bqx2",
     "hasura_roles": [
       "user"
     ],
     "hasura_id": 79
   }


Forgot password / Password reset
--------------------------------

If a user has forgotten their password, it can be reset if they have an email
address associated with the account.

.. note::
  This flow is meant for users who have forgotten their password and can't
  login. For logged-in user to change their password use
  ``/v2/user/change-password`` endpoint.

For ``email`` provider:

To reset a password first a reset token has to be obtained. This is done by
sending a forgot password email to the user's email address.

To send a forgot password email make a request to ``/v2/providers/email/forgot-password`` endpoint
with the user's email address.

.. code-block:: http

   POST auth.<project-name>.hasura-app.io/v2/providers/email/forgot-password HTTP/1.1
   Content-Type: application/json

   {
     "email" : "johnsmith@example.com"
   }

This will send a reset password email with a unique, random token to the user's
email address.

You have to configure the email templates in auth.yaml to include a
link to your application in the email content.  This link will include a
``token`` parameter, that your application has to retrieve. After obtaining the
``token``, your application should make auth API call to
``/v2/providers/email/reset-password`` endpoint to reset the user's password.

The reset password endpoint takes the ``token`` and the new password of the
user.

.. code-block:: http

   POST auth.<project-name>.hasura-app.io/v2/providers/email/reset-password HTTP/1.1
   Content-Type: application/json

   {
     "token": "<token-sent-in-the-email>",
     "password": "newpass123"
   }

For ``mobile-password`` provider:

To reset a password first a reset OTP has to be obtained. This is done by sending
a forgot password SMS to the user's mobile.

To send a forgot password SMS make a request to ``/v2/providers/mobile-password/forgot-password`` endpoint
with the user's mobile number.

.. code-block:: http

   POST auth.<project-name>.hasura-app.io/v2/providers/mobile-password/forgot-password HTTP/1.1
   Content-Type: application/json

   {
     "mobile" : "9876543210",
     "country_code" : 91
   }

After obtaining the OTP, your application should make auth API call to
``/v2/providers/mobile-password/reset-password`` endpoint to reset the user's password.

The reset password endpoint takes the OTP and the new password of the user.

.. code-block:: http

   POST auth.<project-name>.hasura-app.io/v2/providers/mobile-password/reset-password HTTP/1.1
   Content-Type: application/json

   {
     "country_code" : 91,
     "mobile" : "9876543210",
     "otp": 1231,
     "password": "newpass123"
   }





Social login providers
----------------------

Hasura supports integrating third-party providers for identity management.
Currently, `Google`_, `Facebook`_, `Github`_ and `LinkedIn`_ are supported.
There are plans for supporting other providers in the roadmap.

Integrating social login with Hasura
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Irrespective of the provider you use, there is a general setup required for
integrating with third-party providers.

* Create an application with the provider you want (e.g. Google, Facebook,
  LinkedIn).

* Once you create an application, the provider will provide a unique ID and a
  secret associated with the application you created. It is usually called APP
  ID or CLIENT ID, and APP SECRET or CLIENT SECRET respectively.

* You should also set "scope" or "permissions" in the application settings to
  be "email" and "profile" info (which is to say - you want to access your
  users' email and profile information). This step is important, as otherwise
  Hasura won't be able to fetch the user's email address.

* Now, you should configure Hasura Auth to enable that particular provider and
  use the CLIENT ID and CLIENT SECRET you obtained in the second step.

Now, once you have created and configured your application (with the provider)
**and** configured Hasura Auth to use those credentials; you have to setup your
application to perform the actual login mechanism, which are different for
different providers. Let's look at them in details.

.. toctree::
   :maxdepth: 1

   google
   facebook
   github
   linkedin


.. _recaptcha: https://www.google.com/recaptcha/intro/index.html
.. _Google: https://google.com
.. _Facebook: https://facebook.com
.. _LinkedIn: https://linkedin.com
.. _Github: https://github.com


