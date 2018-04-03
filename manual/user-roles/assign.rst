Assigning user roles
====================

A user can be assigned roles. This can only be done by the admin user.

To add a role to a user, make a request to ``/admin/user/add-role``
endpoint.

.. code-block:: http

   POST auth.<cluster-name>.hasura-app.io/admin/user/add-role HTTP/1.1
   Content-Type: application/json

   {
     "role" : "merchant",
     "hasura_id": 42
   }
