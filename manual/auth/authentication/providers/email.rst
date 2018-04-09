.. .. meta::
   :description: Hasura Auth email provider
   :keywords: hasura, users, signup, login, email, verify email


Email based authentication
==========================

This provider supports email/password based authentication.  The user's email
is verified by sending a verification email. To use this provider there are
extra steps to be performed to verify the user's email.

.. note::

  For this provider to send emails, you have to :doc:`enable an email provider <../../../notify/email/index>` in
  the Hasura Notify microservice.

Configuration
-------------

You can configure email provider settings in the :doc:`conf/auth.yaml <../../../project/directory-structure/conf/auth.yaml>` file in your project. Find a top level key called ``email`` in the ``auth.yaml``. By default the email conf looks like this:

.. snippet:: yaml
    :filename: auth.yaml

    email:
      # email address of the sender for verification emails
      verifyEmailFrom: getstarteduser@hasura.io
      # Name of the sender for verification emails
      verifEmailFromName: Admin
      # Subject for verification emails
      verifyEmailSubject: Verify your account - {{ cluster.name }}
      # Template for verification emails. HTML can be used in the template. The
      # template is a Jinja template. Leave the "{{token}}" as it is. It will be
      # used by the auth service to inject the actual token when sending the email.
      verifyTemplate: |
        Hi, Please click on <br/>
        https://auth.{{ cluster.name }}.hasura-app.io/ui/verify-email?token={{ "{{token}}" }}
        to verify your email.
      # Email verification token expiry time in days
      verifyTokenExpires: "7"

      # email address of the sender for forgot password emails
      forgotPassEmailFrom: getstarteduser@hasura.io
      # Name of the sender for forgot password emails
      forgotPassEmailFromName: Admin
      # Subject for forgot password emails
      forgotPassEmailSubject: Reset password request - {{ cluster.name }}
      # Template for forgot password emails. HTML can be used in the template. The
      # template is a Jinja template. Leave the "{{token}}" as it is. It will be
      # used by the auth service to inject the actual token when sending the email.
      forgotPassTemplate: |
        Hi, <br/> Click on
        https://auth.{{ cluster.name }}.hasura-app.io/ui/reset-password?token={{ "{{token}}" }}
        to reset your password.
      # Forgot password reset token expiry time in days
      resetTokenExpires: "7"

You can modify it as you wish and then apply the modifcations to the cluster by running a git push:

.. code-block:: bash

  $ git add conf/auth.yaml
  $ git commit -m "Changed conf for email provider"
  $ git push hasura master


API
---

Signup
~~~~~~

To signup a user, make a request to the signup endpoint : ``/v1/signup``.

.. code-block:: http

   POST auth.<cluster-name>.hasura-app.io/v1/signup HTTP/1.1
   Content-Type: application/json

   {
     "provider" : "email",
     "data" : {
        "email": "johndoe@example.com",
        "password": "somepass123"
     }
   }


If the request is successful, Hasura Auth will send a verification email to the
given email address and will return a response with user details.

This will not login the user automatically (unlike the ``username`` provider),
because at this point the email verification is pending.

The above user details will not have ``auth_token`` set.

Typical response of the ``/v1/signup`` request is :

.. code-block:: http

   HTTP/1.1 200 OK
   Content-Type: application/json

   {
     "auth_token": null,
     "email": "johndoe@example.com",
     "hasura_roles": [
       "user"
     ],
     "hasura_id": 79
   }


* ``auth_token``  is the authentication token of the user for the current
  session. This is null because at this point the email verification is
  pending, hence no session is created for the user.

* ``hasura_roles``  is an list of all roles assigned to the user.

* ``hasura_id``  is the hasura identifier of the user.


Verifying Email
~~~~~~~~~~~~~~~

To verify the email address upon signup, Hasura Auth will send an email with a
unique token to the user's email address. The email template can be configured
in your project inside ``conf/auth.yaml``. The email template must include the
complete verification link along with the ``token`` parameter.

.. code-block:: http

   GET auth.<cluster-name>.hasura-app.io/v1/providers/email/verify-email?token=<token> HTTP/1.1

The response of the email verification endpoint indicates success or failure.
If it is successful, then your application should ask the user to login.

.. code-block:: http

   HTTP/1.1 200 OK
   Content-Type: application/json

   {
     "message"   : "success"
   }


Login
~~~~~

To login a user make a request to the login endpoint: ``/v1/login``.

.. code-block:: http

   POST auth.<cluster-name>.hasura-app.io/v1/login HTTP/1.1
   Content-Type: application/json

   {
     "provider" : "email",
     "data" : {
        "email": "johndoe@example.com",
        "password": "somepass123"
     }
   }

Typical response of the ``/v1/login`` request is :

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


Get user info
~~~~~~~~~~~~~

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
~~~~~~

To logout a user, make the following request.

.. code-block:: http

   POST auth.<cluster-name>.hasura-app.io/v1/user/logout HTTP/1.1
   Authorization: Bearer <auth_token>

.. note::
    The logout request is a POST request with an empty body.


Change password
~~~~~~~~~~~~~~~

If the user is logged in, they can change their password using the following
endpoint.

.. code-block:: http

   POST auth.<cluster-name>.hasura-app.io/v1/user/change-password HTTP/1.1
   Authorization: Bearer <auth_token>

   {
     "old_password": "oldpassword",
     "new_password": "newpassword"
   }

.. _forgot_password_email:

Forgot password / password reset
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If a user has forgotten their password, it can be reset.

.. note::

  This flow is meant for users who have forgotten their password and
  can't login. For logged-in user to change their password use
  ``/v1/user/change-password`` endpoint.


To reset a password first a reset token has to be obtained. This is done by
sending a forgot password email to the user's email address.

To send a forgot password email make a request to ``/v1/providers/email/forgot-password`` endpoint
with the user's email address.

.. code-block:: http

   POST auth.<cluster-name>.hasura-app.io/v1/providers/email/forgot-password HTTP/1.1
   Content-Type: application/json

   {
     "email" : "johnsmith@example.com"
   }

This will send a reset password email with a unique, random token to the user's
email address.

You have to configure the email templates in ``conf/auth.yaml`` (in your Hasura
project) to include a link to your application in the email content.  This link
will include a ``token`` parameter, that your application has to retrieve.
After obtaining the ``token``, your application should make auth API call to
``/v1/providers/email/reset-password`` endpoint to reset the user's password.

The reset password endpoint takes the ``token`` and the new password of the
user.

.. code-block:: http

   POST auth.<cluster-name>.hasura-app.io/v1/providers/email/reset-password HTTP/1.1
   Content-Type: application/json

   {
     "token": "<token-sent-in-the-email>",
     "password": "newpass123"
   }
