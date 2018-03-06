Creating user roles
===================

Creating roles can be done by users having ``admin`` roles.

To create a role, make a request to the ``/admin/create-role`` endpoint.

.. code-block:: http

   POST auth.<cluster-name>.hasura-app.io/admin/create-role HTTP/1.1
   Content-Type: application/json

   {
     "role" : "merchant"
   }

