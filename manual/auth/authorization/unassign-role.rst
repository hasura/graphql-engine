Removing roles from users
=========================

Roles can be removed from users. This can only be done by an admin user.

To remove a role from a user, make a request to ``/admin/user/remove-role``
endpoint.

.. code-block:: http

   POST auth.<cluster-name>.hasura-app.io/admin/user/remove-role HTTP/1.1
   Content-Type: application/json

   {
     "role" : "merchant",
     "hasura_id": 2
   }


You can also do this from the :doc:`API console <../../api-console/index>`.

1. Go to the ``Auth`` section on top and click on the ``Browse Users`` tab.

2. Click the user that you wish to remove a role from.

.. image:: ../../../img/auth/console-remove-role.png
