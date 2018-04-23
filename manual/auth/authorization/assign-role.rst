Assigning roles to users
==========================

A user can be assigned roles. This can only be done by the admin user.

.. rst-class:: api_tabs
.. tabs::

   .. tab:: API-Console

      1. Go to the ``Auth`` section on top and click on the ``Browse Users`` tab.

      2. Click the user that you wish to assign a role.

      .. image:: ../../../img/auth/console-assign-role.png

   .. tab:: REST API

      .. code-block:: http

         POST auth.<cluster-name>.hasura-app.io/admin/user/add-role HTTP/1.1
         Content-Type: application/json

         {
           "role" : "merchant",
           "hasura_id": 2
         }
