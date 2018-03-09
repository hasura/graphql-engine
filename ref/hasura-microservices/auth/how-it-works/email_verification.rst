Email Verification
==================

Hasura Auth follows the established, conventional way of verifying a user's
email address.

To verify the email address, Hasura Auth will send an email with an unique
token to the user's email address, and within a stipulated amount of time, the
user has to submit the token to a Hasura Auth API endpoint to verify the email
address.

How to use it
-------------

You can setup verification of user's email by enabling email verification in
the project console. Right now only `SparkPost <https://www.sparkpost.com/>`_
is supported.

To use email verification, you would also need to provide the API key of
SparkPost. See details :ref:`here <email>`.

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
include a link to your application in the email content (see here how to
:ref:`configure <email>`). This link will include a ``token`` parameter, that
your application has to retrieve. After obtaining the ``token``, your
application should make auth API call to the ``/email/confirm`` `endpoint`_ to
verify the user's email.

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


.. _endpoint: https://hasura.io/_docs/auth/4.0/swagger-ui/#!/anonymous/get_email_confirm
