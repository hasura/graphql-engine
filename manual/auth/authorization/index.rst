.. .. meta::
  :description: Learn how to use Hasura roles
  :keywords: hasura, auth, roles, create role, assign role, add role, delete role


User Roles & Authorization
==========================

The Hasura system has roles attached to every user. This helps in grouping
users together and authorizing them for various actions.

The default roles in the system are: ``anonymous``, ``user`` and ``admin``.

Users with ``admin`` role have super-user privileges. For example, any user with
``admin`` role has all permissions for the data, file microservices.
By default, a user called ``admin`` with role ``admin`` is already created with ``hasura_id`` = 1.

Any logged-in user is attached the ``user`` role by default.

Every unauthenticated user, ie: any non logged-in user, is attached the ``anonymous`` role.

New custom roles can be created/deleted and roles can be assigned/unassigned to users by using Hasura Auth APIs.
These actions can be carried out by users with the ``admin`` role.

Roles can be leveraged by any microservice (running inside the
project) to authorize and control access very easily.

For example, the Auth microservice itself uses the ``user`` role to allow access to profile management APIs
of a user. (ie: APIs starting with ``/user``. Like the email change, password reset APIs.)

If the ``user`` role is removed for a user, the user won't be able to access any profile management APIs.

For details on how the microservices receive the roles for a user, see :doc:`../sessions`

See:
^^^^
- :doc:`../admin-actions/create-role`
- :doc:`../admin-actions/assign-role`
- :doc:`../admin-actions/unassign-role`
- :doc:`../admin-actions/delete-role`