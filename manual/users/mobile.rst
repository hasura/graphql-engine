.. meta::
   :description: Hasura Auth mobile provider
   :keywords: hasura, users, signup, login, mobile, verify mobile


Mobile Passwordless Provider
============================

This provider supports mobile based password-less authentication. The way it
works is, user's mobile number is sent to get an OTP on their mobile phone.
Once they receive an OTP, they can use that to signup or login.

Signup
------

To signup a user, first the user has to get an OTP on their mobile number.

To get an OTP, use the ``/v1/providers/mobile/send-otp`` endpoint.

.. code-block:: http

   POST auth.<project-name>.hasura-app.io/v1/providers/mobile/send-otp HTTP/1.1
   Content-Type: application/json

   {
      "mobile": "9876543210",
      "country_code": 91
   }


.. note::

  For this provider to send SMS, you have to enable a SMS provider in
  the Hasura Notify service.


Once the OTP is obtained, then the user should make a signup request.

.. code-block:: http

   POST auth.<project-name>.hasura-app.io/v1/signup HTTP/1.1
   Content-Type: application/json

   {
     "provider" : "mobile",
     "data" : {
        "mobile": "9876543210",
        "country_code": 91,
        "otp": 123456
     }
   }


If the request is successful, Hasura Auth will create and login the user.

Typical response of the ``/v1/signup`` request is :

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


Login
------

To login a user, first the user has to get an OTP on their mobile number.

To get an OTP, use the ``/v1/providers/mobile/send-otp`` endpoint.

.. code-block:: http

   POST auth.<project-name>.hasura-app.io/v1/providers/mobile/send-otp HTTP/1.1
   Content-Type: application/json

   {
      "mobile": "9876543210",
      "country_code": 91
   }


.. note::

  For this provider to send SMS, you have to enable a SMS provider in
  the Hasura Notify service.


Once the OTP is obtained, then the user should make a login request.

.. code-block:: http

   POST auth.<project-name>.hasura-app.io/v1/login HTTP/1.1
   Content-Type: application/json

   {
     "provider" : "mobile",
     "data" : {
        "mobile": "9876543210",
        "country_code": 91,
        "otp": 123456
     }
   }

If the request is successful, Hasura Auth will login the user.

Typical response of the ``/v1/login`` request is :

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


Getting user info
------------------
To get the logged in user's details, or to check if a session token is valid
you can use this endpoint.

Make a request to the endpoint: ``/v1/user/info``.

.. code-block:: http

   GET auth.<project-name>.hasura-app.io/v1/user/info HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <auth_token>


Typical response of the ``/v1/login`` request is :

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


Logout
------

To logout a user, make the following request.

.. code-block:: http

   POST auth.<project-name>.hasura-app.io/v1/user/logout HTTP/1.1
   Authorization: Bearer <auth_token>

.. note::
    The logout request is a POST request with an empty body.

 
Changing Password
-----------------

If the user is logged in, they can change their password using the following
endpoint.

.. code-block:: http

   POST auth.<project-name>.hasura-app.io/v1/user/change-password HTTP/1.1
   Authorization: Bearer <auth_token>

   {
     "old_password": "oldpassword",
     "new_password": "newpassword"
   }
