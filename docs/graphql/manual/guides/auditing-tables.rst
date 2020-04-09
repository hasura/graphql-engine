.. meta::
   :description: Audit actions on tables in Postgres with Hasura
   :keywords: hasura, docs, guide, postgres, audit table

.. _guides_auditing:

Auditing actions on tables in Postgres
======================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Typically audit logging is added to some of the tables to comply with various certifications.
You may want to capture the user information (role and the session variables) for every change in Postgres that is done through the GraphQL engine.

For every mutation, Hasura roughly executes the following transaction:

.. code-block:: sql

   BEGIN;
   SET local "hasura.user" = '{"x-hasura-role": "role", ... various session variables}'
   SQL related to the mutation
   COMMIT;

This information can therefore be captured in any trigger on the underlying tables by using the ``current_setting`` function as follows:

.. code-block:: sql

   current_setting('hasura.user');

We've set up some utility functions that'll let you quickly get started with auditing in this `repo <https://github.com/hasura/audit-trigger>`__.
