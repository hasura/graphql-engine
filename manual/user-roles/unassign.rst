Unassigning user roles
======================

A user can be unassigned roles. This can only done by an admin user.

To remove a role from a user, make a request to ``/admin/user/remove-role``
endpoint.

.. code-block:: http

   POST auth.<cluster-name>.hasura-app.io/admin/user/remove-role HTTP/1.1
   Content-Type: application/json

   {
     "role" : "merchant",
     "hasura_id": 42
   }


