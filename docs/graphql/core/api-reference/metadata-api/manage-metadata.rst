.. meta::
  :description: Manage metadata with the Hasura metadata API
  :keywords: hasura, docs, metadata API, API reference, metadata

.. _metadata_api_manage_metadata:

Metadata API Reference: Manage metadata
=======================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

APIs to manage Hasura metadata which is stored in ``hdb_catalog`` schema.

.. TODO: add other existing APIs

.. _replace_metadata_v2:

replace_metadata
----------------

``replace_metadata`` is used to replace/import metadata into Hasura. Existing
metadata will be replaced with the new one.

.. code-block:: none

    POST /v1/query HTTP/1.1
    Content-Type: application/json
    X-Hasura-Role: admin

    {
        "type" : "replace_metadata",
        "version": 1 | 2
        "args": <replace-metadata-args>
    }

For version 1, this API corresponds to the legacy API documented under  :ref:`replace_metadata_v1` in the ``/v1/query`` endpoint.

.. _replace_metadata_syntax_v2:

Args syntax
^^^^^^^^^^^

For version 2, the following structure is used:

.. code-block:: none

    {
        allow_inconsistent_metadata: Boolean
        metadata: metadata-object
    }

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - allow_inconsistent_metadata
     - false
     - Boolean
     - If set to ``true``, metadata will be replaced with a warning in the response indicating which items are inconsistent (default: ``false``)
   * - metadata
     - true
     - :ref:`export_metadata`
     - The metadata that will replace the current metadata.

If the version is not specified, then it is inferred from the format of ``args``.

Request
^^^^^^^

.. code-block:: none

    POST /v1/metadata HTTP/1.1
    Content-Type: application/json
    X-Hasura-Role: admin

    {
        "type" : "replace_metadata",
        "version": 2
        "args": {
          "allow_inconsistent_metadata": Boolean,
          "metadata": <metadata-object>
        }
    }

Responses
^^^^^^^^^

Version 2 with inconsistencies and allow_inconsistent_metadata=false, or omitted corresponds with the response document in :ref:`replace_metadata_v1`.

Version 2 example with inconsistencies and allow_inconsistent_metadata=true includes an ``is_consistent`` and ``inconsistent_objects`` corresponding to :ref:`get_inconsistent_metadata`.

.. code-block:: none

  HTTP/1.1 400 Bad Request

  {
    "internal": [
      {
        "type": "remote_schema",
        "reason": "HTTP exception occurred while sending the request to http://localhost:5000/hello-graphql",
        "definition": {
          "definition": {
            "url": "http://localhost:5000/hello-graphql",
            "forward_client_headers": false
          },
          "name": "test",
          "permissions": [],
          "comment": "testing replace metadata with remote schemas"
        }
      }, ...
    ]
  }
