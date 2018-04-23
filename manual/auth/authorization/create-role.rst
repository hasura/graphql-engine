Creating user roles
===================

Creating roles can be done by users having ``admin`` roles. It can be done in the following ways:

.. rst-class:: api_tabs
.. tabs::

   .. tab:: API-Console

      Go to the ``auth`` tab on top and click on ``Roles`` on the left panel.

      .. image:: ../../../img/auth/console-create-role.png

   .. tab:: REST API


      .. code-block:: http


         POST auth.<cluster-name>.hasura-app.io/admin/create-role HTTP/1.1
         Content-Type: application/json

         {
           "role" : "merchant"
         }
