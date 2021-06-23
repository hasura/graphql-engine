.. meta::
   :description: Hasura Metadata file format reference
   :keywords: hasura, docs, metadata, file format

.. _metadata_format:

Metadata format reference
=========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

With ``config v3``, the metadata that is exported from the server by the CLI is a
directory of multiple files/directories.

.. note::

  For ``config v2``, see :ref:`metadata_format_v2`.

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

version.yaml
^^^^^^^^^^^^
The ``version.yaml`` file contains the metadata format version.

.. code-block:: yaml

    version: 3

databases
^^^^^^^^^

.. contents::
  :backlinks: none
  :depth: 1
  :local:

.. code-block:: bash

  metadata
  ├── actions.graphql
  ├── actions.yaml
  ├── allow_list.yaml
  ├── cron_triggers.yaml
  ├── databases
  │   ├── databases.yaml
  │   └── default
  │       └── tables
  │           ├── public_t1.yaml
  ├── query_collections.yaml
  ├── remote_schemas.yaml
  └── version.yaml

databases.yaml
^^^^^^^^^^^^^^

.. code-block:: yaml

  - name: default
    configuration:
      connection_info:
        database_url: <database_url>
        pool_settings:
          idle_timeout: 180
          max_connections: 50
          retries: 1
    tables:
    - "!include public_t1.yaml"
    functions: []

.. note::
  ``database_url`` can be set from an environment variable
  by specifying it as:
  
  .. code-block:: yaml

    database_url:
      from_env: SOME_ENVIRONMENT_VARIABLE


public_t1.yaml
^^^^^^^^^^^^^^

.. code-block:: yaml

  table:
  name: t1
  schema: public

