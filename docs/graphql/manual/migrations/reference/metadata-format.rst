.. meta::
   :description: Hasura Metadata file format reference
   :keywords: hasura, docs, metadata, file format, action, cron trigger, table, remote schema, collection, allow list

.. _metadata_format_v2:

Metadata format reference
=========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

With ``config v2``, the metadata that is exported from the server is a directory
of multiple files.

.. note::

  For ``config v1``, see :ref:`metadata_file_format_v1`.

Metadata directory format
-------------------------

The following files will be generated in the ``metadata/`` directory of your project :

version.yaml
^^^^^^^^^^^^^^
Contains the metadata version of the server

..  code-block:: yaml

    version: 2

tables.yaml
^^^^^^^^^^^
Contains the metadata related to tables

..  code-block::  yaml

    - table:
        schema: public
        name: authors
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
      object_relationships:
      - name: author
        using:
          foreign_key_constraint_on: author_id


In above example ``tables.yaml`` contains two tables authors and books. Here webhook_url can be http endpoint that should be triggered or environment variable
which has http endpoints that should be triggered.

remote_schemas.yaml
^^^^^^^^^^^^^^^^^^^

Contains the metadata related to :ref:`remote schemas<remote_schemas>`

..  code-block::  yaml

    - name: local
      definition:
        url_from_env: REMOTE_SCHEMA
        timeout_seconds: 40


In above example ``remote_schemas.yaml`` contains information about remote schema local where local graphql urls are specified via
REMOTE_SCHEMA environment variable.

..  code-block::  yaml

    - name: local
      definition:
        url: <graphql_url>
        timeout_seconds: 40


The above example is same to the previous except the url is specified directly not via environment variable.

functions.yaml
^^^^^^^^^^^^^^

Contains the metadata related to :ref:`custom functions<custom_sql_functions>`

..  code-block::  yaml

    - function:
      schema: public
      name: search_books


In the above example ``functions.yaml`` file consists of search_books custom sql function

query_collections.yaml
^^^^^^^^^^^^^^^^^^^^^^

Conatins the information about query query collections

..  code-block::  yaml

  - name: allowed-queries
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
    
In above example there is only one collection ``allowed_queries`` which contains two queries ``test`` and ``test2`` .


allow_list.yaml
^^^^^^^^^^^^^^^

Contains the metadata related to :ref:`allow lists<allow_list>`

..  code-block::  yaml

  - collection: allowed-queries

The allowed queries are under the collection name ``allowed-queries``
``allow_list.yaml`` contains collection name of allowed queries.



actions.yaml
^^^^^^^^^^^^

Contains the metadata related to :ref:`actions<actions>`

..  code-block::  yaml

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


In above example the url is specified directly to handler field

..  code-block::  yaml

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


The above example is same to the previous except the url is specified directly not via environment variable.

actions.graphql
^^^^^^^^^^^^^^^

Contains all the action definition and custom type definitions where the metadata information about actions will be stored in actions.yaml

..  code-block:: graphql

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


The above example actiions.graphql contains the definition of greet action and custom type  SampleOutput and SampleInput definitions.

cron_triggers.yaml
^^^^^^^^^^^^^^^^^^

Contains metadata related to cron triggers

..  code-block  yaml

      - name: test
          webhook: <webhook_url>
          schedule: 0 12 * * 1-5
          include_in_metadata: true
          payload: {}
          retry_conf:
            num_retries: 1
            timeout_seconds: 60
            tolerance_seconds: 21600
            retry_interval_seconds: 10

In above example ``cron_triggers.yaml`` contains the information about the test cron trigger. Here webhook_url can be http endpoint that should be triggered or environment variable
which has http endpoints that should be triggered.

..  note::
  
  The metadata about cron triggers will not be stored if ``Include this trigger in Hasura Metadata`` is disabled in advanced option of event in console.
