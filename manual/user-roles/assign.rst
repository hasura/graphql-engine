Assigning user roles
====================

A user can be assigned or unassigned roles. But this can only done by the admin
user. Otherwise, any user can request for admin or any other role to escalate
privileges for themselves.

To assign role to a user, make a request to ``/admin/user/add-role``
endpoint.

.. code-block:: http

   POST auth.<cluster-name>.hasura-app.io/admin/user/add-role HTTP/1.1
   Content-Type: application/json

   {
     "role" : "merchant",
     "hasura_id": 42
   }

To remove a role from a user, make a request to ``/admin/user/remove-role``
endpoint.

.. code-block:: http

   POST auth.<cluster-name>.hasura-app.io/admin/user/remove-role HTTP/1.1
   Content-Type: application/json

   {
     "role" : "merchant",
     "hasura_id": 42
   }


