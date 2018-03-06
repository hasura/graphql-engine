Admin: Delete user
==================

To remove existing user, make a request to the delete-user endpoint : ``/v1/admin/delete-user``.
Hasura Auth deletes user in all providers.

.. code-block:: http

   POST auth.<cluster-name>.hasura-app.io/v1/admin/delete-user HTTP/1.1
   Content-Type: application/json

   {
      "hasura_id": 2
   }

If request is successful, the API will return a success message.

Typical response of the ``/v1/admin/delete-user`` request is :

.. code-block:: http

   HTTP/1.1 200 OK
   Content-Type: application/json

   {
      "details":{
          "username": "deleted",
          "email": "not-found",
          "mobile": "not-found",
          "mobile-password": "not-found",
          "google": "not-found",
          "facebook": "not-found",
          "github": "not-found",
          "linkedin": "not-found",
       },
      "message": "User with hasura_id = 2 is deleted"
   }

* ``hasura_id``  is the hasura identifier of the user.
* ``details``  gives delete status of all providers. Status messages are ``deleted``, ``not-found``, ``not-deleted`` or HTTP exceptions from delete hooks.

