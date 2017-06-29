Mobile Verification
===================

Hasura Auth follows the established way of verifying a user's mobile number. To
verify the mobile number, Hasura Auth will send a SMS with a one time password
or OTP to the user's mobile number, and within a stipulated amount of time, the
user has to submit the OTP to a Hasura Auth API endpoint to verify the mobile
number.


How to use it
-------------

You can setup verification of user's mobile by enabling mobile verification in
the project console. Right now only `MSG91 <https://msg91.com/>`_ is supported.

To use mobile verification, you would also need to provide the API key of
MSG91. See details :ref:`here <mobile>`.

Once mobile verification is enabled, ``mobile`` becomes a mandatory parameter
in the :ref:`signup <signup>` request.

.. code-block:: http

   POST auth.<project-name>.hasura-app.io/signup HTTP/1.1
   Content-Type: application/json

   {
     "username" : "johnsmith",
     "mobile"   : "9876543210",
     "password" : "jsmith123456"
   }


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
include OTP and the message to send (see here how to :ref:`configure
<mobile>`). User will receive the SMS with an OTP. You should instruct the user
to visit your application and enter the OTP. Once your application receives the
OTP, your application should make auth API call to the ``/mobile/confirm``
`endpoint`_ to verify the user's mobile.

.. code-block:: http

   POST auth.<project-name>.hasura-app.io/mobile/confirm HTTP/1.1
   Content-Type: application/json

   {
     "mobile": "9876543210",
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


.. _endpoint: https://hasura.io/_docs/auth/4.0/swagger-ui/#!/anonymous/post_mobile_confirm
