Deleting user roles
===================

Deleting roles can be done by users having the ``admin`` role.

.. rst-class:: api_tabs
.. tabs::

   .. tab:: API-Console

      Go to the ``auth`` tab on top and click on ``Roles`` on the left panel.

      .. image:: ../../../img/auth/console-delete-role.png

   .. tab:: REST API

      .. code-block:: http

         POST auth.<cluster-name>.hasura-app.io/admin/delete-role HTTP/1.1
         Content-Type: application/json

         {
           "role" : "merchant"
         }

.. note::

   You cannot delete the default roles, ie: ``anonymous``, ``user`` and ``admin``, from the system.
