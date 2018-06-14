Removing roles from users
=========================

Roles can be removed from users. This can only be done by an admin user.

.. rst-class:: api_tabs
.. tabs::

   .. tab:: API-Console

      1. Go to the ``Auth`` section on top and click on the ``Browse Users`` tab.

      2. Click the user that you wish to remove a role from.

      .. image:: ../../../img/auth/console-remove-role.png

   .. tab:: REST API

      .. code-block:: http

         POST auth.<cluster-name>.hasura-app.io/admin/user/remove-role HTTP/1.1
         Content-Type: application/json

         {
           "role" : "merchant",
           "hasura_id": 2
         }
