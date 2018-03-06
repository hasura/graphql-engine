Admin: Reset user password
==========================

To reset password of a user, make a request to the admin endpoint : ``/v1/admin/user/reset-password``

.. code-block:: http

   POST auth.<cluster-name>.hasura-app.io/v1/admin/user/reset-password HTTP/1.1
   Content-Type: application/json

   {
      "hasura_id": 3,
      "new_password": "newpass123",
      "admin_password": "adminpass123"
   }

You have to provide admin password in order to reset users password.

If the request is successful, the API will return success message.

Typical response of the ``/v1/admin/user/remove-all-sessions`` request is :

.. code-block:: http

   HTTP/1.1 200 OK
   Content-Type: application/json

   {
      "message": "password updated"
   }


