Delete user role
================

Deleting roles can be done by users having the ``admin`` role.

To delete a role, make a request to the ``/admin/delete-role`` endpoint.

.. code-block:: http

   POST auth.<cluster-name>.hasura-app.io/admin/delete-role HTTP/1.1
   Content-Type: application/json

   {
     "role" : "merchant"
   }


.. note::

   You cannot delete the default roles, ie: ``anonymous``, ``user`` and ``admin``, from the system.