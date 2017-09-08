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


.. _signup:

Signup
------

To signup a user make a request to the signup endpoint.

The signup endpoint is ``/signup``.

Mandatory parameters in the request body are:

* Username of the user: ``username``
* Password of the user: ``password``

Optional parameters are ``email`` and  ``mobile``. You can also setup
*verification* of user's Email and Mobile. Once you enable email or mobile
verification, those parameters also become mandatory.

.. code-block:: http

   POST auth.<project-name>.hasura-app.io/signup HTTP/1.1
   Content-Type: application/json

   {
     "username" : "johnsmith",
     "password" : "jsmith123456"
   }

If the signup request is successful, the user is logged in.

Typical response of the ``/signup`` request is :

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
  not logged in, due to email or mobile verification pending, or the admin user
  disabling the user etc., then the value of the ``auth_token`` will be ``null``.


Enabling other fields in signup
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Email and Mobile fields during signup are optional or mandatory based on your
configuration.

.. note::
  IMPORTANT: The mobile number has to be sent with country code prefixed (with
  + and not 00). If any country code is **not** given, it is assumed to be an
  Indian phone number.


Email/Mobile not enabled
++++++++++++++++++++++++

If you have not enabled email or mobile login/verification, you can pass
those fields **optionally**:

.. code-block:: http

   POST auth.<project-name>.hasura-app.io/signup HTTP/1.1
   Content-Type: application/json

   {
     "username" : "johnsmith",
     "password" : "jsmith123456",
     "email"    : "johnsmith@example.com",
     "mobile"   : "+919876543210"
   }


Email/Mobile enabled
++++++++++++++++++++

If you have enabled email or mobile verification/login, then that field becomes
**mandatory**. For e.g, if you have enabled email verification, then the the email
field is mandatory.

.. note::

  If verification is enabled, then the response from ``/signup`` endpoint would
  return ``auth_token`` as ``null``.

  .. code-block:: http

     HTTP/1.1 200 OK
     Content-Type: application/json

     {
       "auth_token": null,
       "hasura_roles": [
         "user"
       ],
       "hasura_id": 79
     }


Recaptcha
+++++++++

You can also setup `recaptcha`_ in the sign up process.

If you have enabled recaptcha, then you have to send another parameter in the
signup request: ``g-recaptcha-response`` containing the recaptcha value from
Google.

.. code-block:: http

   POST auth.<project-name>.hasura-app.io/signup HTTP/1.1
   Content-Type: application/json

   {
     "username" : "johnsmith",
     "password" : "jsmith123456",
     "email"    : "johnsmith@example.com",
     "g-recaptcha-response" : "<recaptcha-response-received-from-google>"
   }


To configure recaptcha for your project, you can to your project console.
Inside Auth -> Sign-In Methods you should find the Recaptcha settings.


Login
------

To login a user make a request to the login endpoint.

The login endpoint is ``/login``.

There are two mandatory parameters in the request body for a login action.

1. Password of the user.
2. The second will be - based on your configuration - username, email or
   mobile of the user. If you have enabled login via email in your project
   console, this will be ``email``. Similarly for mobile. If you have not
   enabled either of them, then this will be ``username``.


.. code-block:: http

   POST auth.<project-name>.hasura-app.io/login HTTP/1.1
   Content-Type: application/json

   {
     "username" : "johnsmith",
     "password" : "jsmith123456"
   }


Typical response of the ``/login`` request is :

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


Verification is enabled
^^^^^^^^^^^^^^^^^^^^^^^
If email/mobile verification is enabled and the user has not verified their
email/mobile; then the response from ``/login`` endpoint would be an error
saying email/mobile verification is pending.



Sessions
----------

When a user is logged-in, along with its Hasura identity, a session is attached.

A session is nothing but a unique, un-guessable identifier  attached to that
Hasura identity (referred to as ``auth_token``) for that session. This way an
user can make subsequent requests without having to authenticate, with
credentials, on every request. Instead, on every request the user can present
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

To verify the email address, Hasura Auth will send an email with an unique
token to the user's email address, and within a stipulated amount of time, the
user has to submit the token to a Hasura Auth API endpoint to verify the email
address.

.. note::
  Enable email verification in the project console.
  Under Auth -> Sign-In Methods.
  (You also have to enable email in Hasura Notify service)


Once email verification is enabled, ``email`` becomes a mandatory parameter in
the :ref:`signup <signup>` request.

.. code-block:: http

   POST auth.<project-name>.hasura-app.io/signup HTTP/1.1
   Content-Type: application/json

   {
     "username" : "johnsmith",
     "email"    : "johnsmith@example.com",
     "password" : "jsmith123456"
   }

Now, when a request is made to the :ref:`signup <signup>` endpoint, the
response will have ``auth_token`` as ``null``, the user won't be logged
in and a verification email will be sent to the user.

.. code-block:: http

   HTTP/1.1 200 OK
   Content-Type: application/json

   {
     "auth_token": null,
     "hasura_roles": [
       "user"
     ],
     "hasura_id": 79
   }


Also, the user will be set as not active. Once the email is verified the user
will be set as active.

To verify the email of the user, you have to configure the email settings to
include a link to your application in the email content.  This link will
include a ``token`` parameter, that your application has to retrieve. After
obtaining the ``token``, your application should make auth API call to the
``/email/confirm`` to verify the user's email.

.. code-block:: http

   GET auth.<project-name>.hasura-app.io/email/confirm?token=<token> HTTP/1.1

The response of the email verification endpoint indicates success or failure.
If it is successful, then your application should ask the user to login.

.. code-block:: http

   HTTP/1.1 200 OK
   Content-Type: application/json

   {
     "hasura_id" : 79,
     "user_email": "johnsmith@example.com",
     "message"   : "Email Verified"
   }


Verifying Mobile
----------------

To verify the mobile number, Hasura Auth will send a SMS with a one time
password or OTP to the user's mobile number, and within a stipulated amount of
time, the user has to submit the OTP to a Hasura Auth API endpoint to verify
the mobile number.

.. note::
  Enable mobile verification in the project console.
  Under Auth -> Sign-In Methods.
  (You also have to enable mobile in Hasura Notify service)


Once mobile verification is enabled, ``mobile`` becomes a mandatory parameter
in the :ref:`signup <signup>` request.

.. code-block:: http

   POST auth.<project-name>.hasura-app.io/signup HTTP/1.1
   Content-Type: application/json

   {
     "username" : "johnsmith",
     "mobile"   : "+919876543210",
     "password" : "jsmith123456"
   }

.. note::
  IMPORTANT: The mobile number has to be sent with country code prefixed (with
  + and not 00). If any country code is **not** given, it is assumed to be an
  Indian phone number.


Now, when a request is made to the :ref:`signup <signup>` endpoint, the
response will have ``auth_token`` as ``null``, the user won't be logged in and
a verification SMS with an OTP will be sent to the user.

.. code-block:: http

   HTTP/1.1 200 OK
   Content-Type: application/json

   {
     "auth_token": null,
     "hasura_roles": [
       "user"
     ],
     "hasura_id": 79
   }


Also, the user will be set as not active. Only, once the mobile is verified the
user will be set as active.

To verify the mobile of the user, you have to configure your mobile settings to
include OTP and the message to send. User will receive the SMS with an OTP. You
should instruct the user to visit your application and enter the OTP. Once your
application receives the OTP, your application should make auth API call to the
``/mobile/confirm`` to verify the user's mobile.

.. code-block:: http

   POST auth.<project-name>.hasura-app.io/mobile/confirm HTTP/1.1
   Content-Type: application/json

   {
     "mobile": "+919876543210",
     "otp"   : "123456"
   }

The response of the mobile verification endpoint indicates success or failure.
If it is successful, then your application should ask the user to login.

.. code-block:: http

   HTTP/1.1 200 OK
   Content-Type: application/json

   {
     "message" : "Mobile verified"
   }



Forgot password / Password reset
--------------------------------

If a user has forgotten their password, it can be reset if they have an email
address associated with the account.

.. note::
  This flow is meant for user's who have forgotten their password and can't
  login. For logged-in user to change their password use
  ``/user/password/change`` endpoint.

To reset a password first a reset token has to be obtained. This is done by
send a reset password email to the user's email address.

To send a forgot password email make a request to ``/password/forgot`` endpoint
with the user's email address.

.. code-block:: http

   POST auth.<project-name>.hasura-app.io/password/forgot HTTP/1.1
   Content-Type: application/json

   {
     "email" : "johnsmith@example.com"
   }

This will send a reset password email with a unique, random token to the user's
email address.

You have to configure the email templates (in the project console) to include a
link to your application in the email content.  This link will include a
``token`` parameter, that your application has to retrieve. After obtaining the
``token``, your application should make auth API call to the
``/password/reset`` endpoint to reset the user's password.

The reset password endpoint takes the ``token`` and the new password of the
user.

.. code-block:: http

   POST auth.<project-name>.hasura-app.io/password/reset HTTP/1.1
   Content-Type: application/json

   {
     "email" : "johnsmith@example.com",
     "token" : "<token-sent-in-the-email>"
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


