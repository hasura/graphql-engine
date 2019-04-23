.. _pg_dump_api_reference:

pg_dump API
===========

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

This is an admin-only API that can be used to execute ``pg_dump`` on the
Postgres instance that Hasura is configured with. The primary motive of this API
is to provide convenience methods to initialise migrations from an existing
Hasura instance. But the functionality can be later expanded to do other things
such as taking data dump etc.

API Spec
--------

.. code-block:: http

   POST /v1alpha1/pg_dump HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
     "opts": ["-O", "-x", "--schema-only", "--schema", "public"],
     "clean_output": true
   }

- ``opts``: Arguments to be passed to the ``pg_dump`` tool. Represented as array
  of strings. The underlying command that is executed is:

  .. code-block:: bash

     pg_dump $DATABASE_URL $OPTS -f $FILENAME

- ``clean_output``: When this optional argument is set to ``true``, the output SQL from
  the command is cleaned to remove the following:

  -  SQL front matter, like SET statements.
  -  ``CREATE SCHEMA public``.
  -  ``COMMENT ON SCHMEA public is 'standard public schema'``;
  -  Comments (``--``) and empty newlines.
  -  Postgres triggers created by Hasura for event triggers.


Source code for the script that is executed can be found `here <https://github.com/hasura/graphql-engine/tree/master/server/src-rsr/run_pg_dump.sh>`_.

Disabling PG Dump API
---------------------

Since this API can be used to dump all the Postgres data and schema, it can be
disabled, especially in production deployments. ``enabled-apis`` flag can be
used to enable/disable this API. Like other APIs, ``pg_dump`` API is also
enabled by default. To disable it, you need to explicitly state that this API is
not enabled. i.e. enable only ``graphql`` API and disable ``pgdump`` API.

.. code-block:: bash

   # enable only graphql api, disable pgdump and metdata api
   --enabled-apis="graphql"
   HASURA_GRAPHQL_ENABLED_APIS="graphql"
