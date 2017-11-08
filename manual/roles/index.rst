.. .. meta::
  :description: Learn how to use Hasura roles
  :keywords: hasura, auth, roles, create role, assign role, add role, delete role


Roles
=====

The Hasura system has roles attached to every user. This helps in grouping
users together and authorizing them for various actions.

The default roles in the system are: ``anonymous``, ``user`` and ``admin``.

The ``anonymous`` role is attached to every unauthenticated user.

``admin`` role has super-user privileges by default. For example, any user with
``admin`` role has all permissions for the data, file services.

Any logged-in user is attached the ``user`` role by default.

Hasura Auth service has APIs to create and manage roles.

Custom roles can be created by using Hasura Auth APIs. Users can be assigned
custom roles as well.


Creating roles
--------------

Creating roles can be done by users having ``admin`` roles.

To create a role, make a request to the ``/admin/create-role`` endpoint.

.. code-block:: http

   POST auth.<cluster-name>.hasura-app.io/admin/create-role HTTP/1.1
   Content-Type: application/json

   {
     "role" : "merchant"
   }


Assigning roles
---------------

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


Deleting roles
--------------

Deleting roles can be done by users having ``admin`` roles.

To delete a role, make a request to the ``/admin/delete-role`` endpoint.

.. code-block:: http

   POST auth.<cluster-name>.hasura-app.io/admin/delete-role HTTP/1.1
   Content-Type: application/json

   {
     "role" : "merchant"
   }



FAQ
----

Why is the user role given by default?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Because, this role can be then leveraged by any service (running inside the
project) to provide authorization and access control mechanisms very easily.

Like, Hasura Auth APIs use this role to allow access to profile management APIs
of an user. (Profile management APIs are any API starting with ``/user``. Like
email change, password reset etc. APIs.)

To illustrate, the role information is passed in the header by the API gateway
when the request reaches the upstream service. This service can then look at
the headers and perform its own authorization and access control easily.

What if the user role is removed from an user?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
The user won't be able to access any profile management(account info, password
change, password reset, email change etc. - basically any API behind
``/user/``) APIs, if the ``user`` role is removed from an user.

What if the user role itself is removed?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
You cannot remove the ``user`` role, it is default to the system.

What if a new role needs to be assigned to a user?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
You can create custom roles, and use the admin APIs of Hasura Auth to assign
roles to an user.

You can also use the project console to assign roles to an user.
