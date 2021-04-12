.. meta::
   :description: Hasura Metadata file format reference
   :keywords: hasura, docs, metadata, file format

.. _metadata_format_v2:

Metadata format reference
=========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

With ``config v2``, the metadata that is exported from the server is a directory
of multiple files.

.. note::

  For ``config v1``, see :ref:`metadata_file_format_v1`.

Metadata directory format
-------------------------

The following files will be generated in the ``metadata/`` directory of your project.

- ``version.yaml``: Contains the metadata version of the server
- ``tables.yaml``: Contains the metadata related to tables
- ``remote_schemas.yaml``: Contains the metadata related to :ref:`remote schemas<remote_schemas>`
- ``functions.yaml``: Contains the metadata related to :ref:`custom functions<custom_sql_functions>`
- ``allow_list.yaml``: Contains the metadata related to :ref:`allow lists<allow_list>`
- ``actions.yaml``: Contains the metadata related to :ref:`actions<actions>`
- ``actions.graphql``: Contains all the action definition and custom type definitions
