.. _pg_dump_api_reference:

PG Dump API Reference
=====================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

The PG Dump API is an admin-only endpoint that can be used to execute ``pg_dump`` on the
Postgres instance that Hasura is configured with.

The primary motive of this API is to provide convenience methods to initialise migrations from an
existing Hasura instance. But the functionality can be later expanded to do other things
such as taking data dump etc.

Endpoint
--------

All requests are ``POST`` requests to the ``/v1alpha1/pg_dump`` endpoint.

API Spec
--------

Request
^^^^^^^

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

Sample response
^^^^^^^^^^^^^^^

.. code-block:: http

  HTTP/1.1 200 OK
  Content-Type: application/sql

  CREATE TABLE public.author (
      id integer NOT NULL,
      name text NOT NULL
  );
  CREATE SEQUENCE public.author_id_seq
      AS integer
      START WITH 1
      INCREMENT BY 1
      NO MINVALUE
      NO MAXVALUE
      CACHE 1;
  ALTER SEQUENCE public.author_id_seq OWNED BY public.author.id;
  ALTER TABLE ONLY public.author ALTER COLUMN id SET DEFAULT nextval('public.author_id_seq'::regclass);

Disabling PG Dump API
---------------------

Since this API can be used to dump all the Postgres data and schema, it can be
disabled, especially in production deployments.

The ``enabled-apis`` flag or the ``HASURA_GRAPHQL_ENABLED_APIS`` env var can be used to
enable/disable this API. By default, The PG DumpAPI is enabled. To disable it, you need to explicitly
state that this API is not enabled. i.e. remove it from the list of enabled APIs.

.. code-block:: bash

   # enable only graphql & metadata apis, disable pgdump
   --enabled-apis="graphql,metadata"
   HASURA_GRAPHQL_ENABLED_APIS="graphql,metadata"

See :doc:`../deployment/graphql-engine-flags/reference` for info on setting the above flag/env var