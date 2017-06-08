Basic actions & Sessions
========================

Basic actions includes registering and logging in a user. We will see below how
to perform these basic actions.

You can always refer to the full API documentation, available at:
https://hasura.io/_docs/auth/4.0/swagger-ui/ .

The base URL for the Auth service would be:
`https://auth.<project-name>.hasura-app.io`. This is also mentioned in your
project console.

All APIs accept and respond only in JSON. Which means that you have to make
sure that all requests you send, should contain the ``Content-Type:
application/json`` header.


.. _signup:

Registration / Sign Up
----------------------

To register a user use the ``/signup`` endpoint.

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
* ``hasura_roles``  is an array of all roles assigned to the user.

* ``hasura_id``  is the hasura identifier of the user.

The ``auth_token``  is only returned if the user is logged in. If the user is
not logged in, due to email or mobile verification pending, or the admin user
disabling the user etc., then the value of the authentication token will be
``null``.

Enabling other fields in signup
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Email and Mobile fields during signup are optional or mandatory based on your
configuration.

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
     "mobile"   : "9876543210"
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

You can also setup `recaptcha`_ in your sign up process.

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


To configure recaptcha for your project, read :ref:`here <recaptcha>`.


Login
-----

To login a user, use the ``/login`` endpoint.

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
email/mobile; then the response from ``/login`` endpoint would return
``auth_token`` as ``null``:

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


Managing Sessions
-----------------
If you are building browser-based apps, then Hasura Auth already sends
appropriate cookie headers to manage the session. You don't have to do any
additional work to manage sessions, except making the appropriate API calls.

If you are building mobile/device apps, then you have to device your own
mechanism of storing the authentication tokens (``auth_token``) and managing
them. That means, storing and updating them whenever a Hasura Auth API returns
a new authentication token, and remove all existing authentication tokens
(``auth_token``) on :ref:`these conditions <session-expiry>`.


.. _recaptcha: https://www.google.com/recaptcha/intro/index.html
