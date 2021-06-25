.. meta::
   :description: Hasura Metadata file format reference
   :keywords: hasura, docs, metadata, file format

.. _metadata_format_v2:

Metadata format reference (config v2)
=====================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

With ``config v2``, the metadata that is exported from the server by the CLI is a
directory of multiple files.

.. note::

  For ``config v1``, see :ref:`metadata_file_format_v1`.

Metadata directory format
-------------------------

The following files will be generated in the ``metadata/`` directory of your project:

.. contents::
  :backlinks: none
  :depth: 1
  :local:

.. note::

  The output of the :ref:`export_metadata <export_metadata>` API is a JSON version of the metadata files.

actions.graphql
^^^^^^^^^^^^^^^

The ``actions.graphql`` file contains all the :ref:`action<actions>` definitions and :ref:`custom type<custom_types>` definitions.

**Example**: A query action called ``greet`` and two custom types called ``SampleInput`` and ``SampleOutput``.

.. code-block:: graphql

  type Query {
    greet (
      arg1: SampleInput!
    ): SampleOutput
  }
  input SampleInput {
    username : String!
  }
  type SampleOutput {
    greetings : String!
  }

actions.yaml
^^^^^^^^^^^^

The ``actions.yaml`` file contains metadata related to :ref:`actions<actions>`.

**Example**: An action called ``greet`` with the ``handler`` set to ``<base_url>/greet`` and two custom types called ``SampleInput`` and ``SampleOutput``.

.. code-block::  yaml

  actions:
  - name: greet
    definition:
      kind: ""
      handler: <base_url>/greet
      forward_client_headers: true
      headers:
      - value: application/json
        name: Content-Type
  custom_types:
    enums: []
    input_objects:
    - name: SampleInput
    objects:
    - name: SampleOutput
    scalars: []

**Example**: Same example as above but with the base URL of the ``handler`` passed as an environment variable.

.. code-block::  yaml

  actions:
  - name: greet
    definition:
      kind: ""
      handler: '{{ACTION_BASE_URL}}/greet'
      forward_client_headers: true
      headers:
      - value: application/json
        name: Content-Type
  custom_types:
    enums: []
    input_objects:
    - name: SampleInput
    objects:
    - name: SampleOutput
    scalars: []

allow_list.yaml
^^^^^^^^^^^^^^^

The ``allow_list.yaml`` file contains the metadata related to :ref:`allow lists<allow_list>`.

**Example**: A query collection called ``allowed-queries`` set as the allow-list.

.. code-block::  yaml

  - collection: allowed-queries

cron_triggers.yaml
^^^^^^^^^^^^^^^^^^

The ``cron_triggers.yaml`` file contains metadata related to :ref:`cron triggers<creating_cron_trigger>`.
The ``webhook`` can be an HTTP endpoint or an environment variable containing the HTTP endpoint.

**Example**: A cron trigger called ``test-trigger``. 

.. code-block::  yaml

  - name: test-trigger
  webhook: <webhook-url>
  schedule: 0 12 * * 1-5
  include_in_metadata: true
  payload: {}
  retry_conf:
    num_retries: 1
    timeout_seconds: 60
    tolerance_seconds: 21600
    retry_interval_seconds: 10

.. note::
  
  The metadata about a cron trigger will not be stored if ``Include this trigger in Hasura Metadata`` is disabled in the advanced option of ``events`` on the console or ``include_in_metadata`` is passed as ``false`` via the API.

functions.yaml
^^^^^^^^^^^^^^

Contains the metadata related to :ref:`custom functions<custom_sql_functions>`.

**Example**: A custom SQL function called ``search_books``.

.. code-block::  yaml

    - function:
      schema: public
      name: search_books

query_collections.yaml
^^^^^^^^^^^^^^^^^^^^^^

The ``query_collections.yaml`` file conatins metadata information about :ref:`query collections<api_query_collections>`.

**Example**: A query collection called ``sample-collection`` which contains two queries ``test`` and ``test2``.

.. code-block::  yaml

  - name: sample-collection
    definition:
      queries:
      - name: test
        query: |-
          query test {
            books {
              id
              author_id
              title
            }
          }
      - name: test2
        query: |-
          query test2 {
              authors{
                  id
                  author_name
              }
          }

remote_schemas.yaml
^^^^^^^^^^^^^^^^^^^

The ``remote_schemas.yaml`` file contains the metadata related to :ref:`remote schemas<remote_schemas>`.

**Example**: A remote schema called ``my-remote-schema`` with URL ``<remote-schema-url>``.

.. code-block::  yaml

    - name: my-remote-schema
      definition:
        url: <remote-schema-url>
        timeout_seconds: 40

**Example**: A remote schema called ``my-remote-schema`` with URL passed as environment variable.

.. code-block:: yaml

    - name: my-remote-schema
      definition:
        url_from_env: REMOTE_SCHEMA
        timeout_seconds: 40

tables.yaml
^^^^^^^^^^^

The ``tables.yaml`` file contains metadata related to :ref:`tables<schema_tables>`.

**Example**: Two tables called ``authors`` and ``books`` including relationships and an event trigger defined on the ``authors`` table.

.. code-block::  yaml

    - table:
        schema: public
        name: authors
      insert_permissions:
      - role: user
        permission:
          check:
            id:
              _eq: X-Hasura-User-Id
          columns:
          - name
          backend_only: false
      select_permissions:
      - role: user
        permission:
          columns:
          - id
          - name
          filter:
            id:
              _eq: X-Hasura-User-Id
      array_relationships:
      - name: books
        using:
          foreign_key_constraint_on:
            column: author_id
            table:
              schema: public
              name: books
      event_triggers:
      - name: event_test
        definition:
          enable_manual: false
          insert:
            columns: '*'
          delete:
            columns: '*'
          update:
            columns:
            - id
            - author_name
        retry_conf:
          num_retries: 1
          interval_sec: 10
          timeout_sec: 60
        webhook: <webhook_url>
    - table:
        schema: public
        name: books
      insert_permissions:
      - role: user
        permission:
          check:
            id:
              _eq: X-Hasura-User-Id
          columns:
          - author_id
          - name
          backend_only: false
      select_permissions:
      - role: user
        permission:
          columns:
          - id
          - name
          filter:
            id:
              _eq: X-Hasura-User-Id
      object_relationships:
      - name: author
        using:
          foreign_key_constraint_on: author_id

version.yaml
^^^^^^^^^^^^
The ``version.yaml`` file contains the metadata format version.

.. code-block:: yaml

    version: 2
    
