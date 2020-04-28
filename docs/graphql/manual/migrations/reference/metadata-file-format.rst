.. meta::
   :description: Hasura Metadata file format reference
   :keywords: hasura, docs, metadata, file format

.. _metadata_file_format:

Metadata file format reference
==============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

Post ``v1.2.0``, the CLI now supports two versions of configuration, ``v1`` and
``v2`` depending on which the structure of the metadata varies.

Metadata format
---------------

With ``config v2``, the metadata that is exported from the server is a directory
of multiple files. When you run ``hasura metadata export``, the following files
will be generated in the ``metadata/`` directory of your project.

- ``version.yaml``: Contains the metadata version of the server
- ``tables.yaml``: Contains the metadata related to tables
- ``remote_schemas.yaml``: Contains the metadata related to :ref:`remote schemas<remote_schemas>`
- ``functions.yaml``: Contains the metadata related to :ref:`custom functions<custom_sql_functions>`
- ``allow_list.yaml``: Contains the metadata related to :ref:`allow lists<allow_list>`
- ``actions.yaml``: Contains the metadata related to :ref:`actions<actions>`
- ``actions.graphql``: Contains all the action definition and custom type definitions


.. note::

  For metadata file structure using ``config v1``, see :ref:`metadata_file_format_v1`