User: Change Password
=====================

If the user is logged in, they can change their password using the following
endpoint

.. code-block:: http

   POST auth.<cluster-name>.hasura-app.io/v1/user/change-password HTTP/1.1
   Authorization: Bearer <auth_token>

   {
     "old_password": "oldpassword",
     "new_password": "newpassword"
   }

