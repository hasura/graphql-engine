.. meta::
   :description: Remote schema relationship permissions
   :keywords: authorization, docs, remote schema relationship, remote schema relationship permissions

.. _remote_schema_relationship_permissions:

Remote schema relationship permissions
======================================

Remote schema relationship permissions are derived from the
:ref:`remote schema permissions <remote_schema_permissions>` defined for the role.
When a remote relationship cannot be derived, the remote relationship field will
not be added to the schema for the role.

Some of the cases in which a remote relationship cannot be derived are:

1. There are no remote schema permissions defined for the role.
2. The role doesn't have access to the types that are used by the
   remote relationship.

.. note::

   Remote relationship permissions are derived from remote schema
   permissions only when the graphql-engine is enabled with
   remote schema permissions. When remote schema permissions are not
   enabled, there will be no restrictions for the roles and therefore
   there will be no restrictions for remote relationships as well.
