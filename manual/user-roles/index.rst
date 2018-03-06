.. .. meta::
  :description: Learn how to use Hasura roles
  :keywords: hasura, auth, roles, create role, assign role, add role, delete role


User Roles & Authorization
==========================

The Hasura system has roles attached to every user. This helps in grouping
users together and authorizing them for various actions.

The default roles in the system are: ``anonymous``, ``user`` and ``admin``.

The ``anonymous`` role is attached to every unauthenticated user.

``admin`` role has super-user privileges by default. For example, any user with
``admin`` role has all permissions for the data, file microservices.

Any logged-in user is attached the ``user`` role by default.

Hasura Auth microservice has APIs to create and manage roles.

Custom roles can be created by using Hasura Auth APIs. Users can be assigned
custom roles as well.

See:
^^^^

.. toctree::
   :maxdepth: 1

   create
   assign
   delete
   FAQ <faq>
