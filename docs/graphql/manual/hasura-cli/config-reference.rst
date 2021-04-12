.. meta::
   :description: Haura GarphQL CLI configuration reference 
   :keywords: hasura, docs, CLI, CLI reference, config 

.. _cli_config_reference:

Hasura CLI Configuration Reference 
==================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

Hasura CLI commands can get key values from flags, ENV variables, a ``.env`` file
or the configuration file ``config.yaml``.

Configuration file
------------------
 
In order for the Hasura CLI to work, the ``config.yaml`` file is required
(created automatically via the :ref:`hasura init<hasura_init>` command).
The configuration file can be configured with the following config keys:

.. code-block:: yaml

  version: 
  endpoint: 
  admin_secret:
  insecure_skip_tls_verify:
  certificate_authority:
  access_key: #deprecated
  api_paths:
    query: 
    graphql: 
    config: 
    pg_dump: 
    version: 
  metadata_directory:
  migrations_directory: 
  actions:
    kind: 
    handler_webhook_baseurl: 
    codegen:
      framework:
      output_dir:
      uri:

.. list-table::
   :header-rows: 1

   * - Config Key
     - Required
     - Supported since Config Version
     - Default value
   * - version
     - false
     - 1
     - 2
   * - endpoint
     - true
     - 1
     - http://localhost:8080
   * - admin_secret
     - false
     - 1
     -
   * - access_key
     - false
     - 1 (Deprecated)
     - 
   * - insecure_skip_tls_verify
     - false
     - 1 (added in ``v1.2.0``)
     - false
   * - certificate_authority
     - false
     - 1 (added in ``v1.2.0``)
     -
   * - api_paths
     - false
     - 1
     -
   * - api_paths.query
     - false
     - 1
     - v1/query 
   * - api_paths.graphql
     - false
     - 1
     - v1/graphql
   * - api_paths.config
     - false
     - 1
     - v1alpha1/config
   * - api_paths.pg_dump
     - false
     - 1
     - v1alpha1/pg_dump
   * - api_paths.version
     - false
     - 1
     - v1/version
   * - metadata_directory
     - false
     - 2
     - 
   * - migrations_directory
     - false
     - 1
     - migrations
   * - actions
     - false
     - 2
     -
   * - actions.kind
     - true 
     - 2
     - synchronous
   * - actions.handler_webhook_baseurl
     - true
     - 2
     - http://localhost:3000
   * - actions.codegen
     - false
     - 2
     -   
   * - actions.codegen.framework
     - true
     - 2
     -
   * - actions.codegen.output_dir
     - true 
     - 2
     -
   * - actions.codegen.uri
     - false
     - 2
     -

.. note::

  The above structure is for the ``config v2`` file which is supported since ``v1.2.0``. Refer to :ref:`this page <migrations_upgrade_v2>` on how to upgrade to ``config v2``.

  A ``config v1`` file of your Hasura project would look like:

  .. code-block:: yaml

      endpoint: http://localhost:8080

Environment variables
---------------------

The configuration can also be set in the form of environment variables:

.. list-table::
   :header-rows: 1

   * - ENV variable
     - Config file key
     - Description
   
   * - ``HASURA_GRAPHQL_VERSION``
     - ``version``
     - Config version to be used. 

   * - ``HASURA_GRAPHQL_ENDPOINT``
     - ``endpoint``
     - http(s) endpoint for Hasura GraphQL engine.

   * - ``HASURA_GRAPHQL_ADMIN_SECRET``
     - ``admin_secret``  
     - Admin secret for Hasura GraphQL engine. 

   * - ``HASURA_GRAPHQL_ACCESS_KEY``
     - ``access_key``
     - Access key for Hasura GraphQL engine. Note: Deprecated. Use admin 
       secret instead. 

   * - ``HASURA_GRAPHQL_INSECURE_SKIP_TLS_VERIFY``
     - ``insecure_skip_tls_verify``
     - Skip verifying SSL certificate for the Hasura endpoint. Useful if you have
       a self-singed certificate and don't have access to the CA cert.

   * - ``HASURA_GRAPHQL_CERTIFICATE_AUTHORITY``
     - ``certificate_authority``
     - Path to the CA certificate for validating the self-signed certificate for
       the Hasura endpoint.

   * - ``HASURA_GRAPHQL_API_PATHS_QUERY``
     - ``api_paths.query``
     - Schema/ metadata API endpoint. More details at :ref:`schema_metadata_api`.
     
   * - ``HASURA_GRAPHQL_API_PATHS_GRAPHQL``
     - ``api_paths.graphql``
     - GraphQL API endpoint. More details at :ref:`graphql_api`.
   
   * - ``HASURA_GRAPHQL_API_PATHS_CONFIG``
     - ``api_paths.config``
     - Config API endpoint. More details at :ref:`config_api`.
   
   * - ``HASURA_GRAPHQL_API_PATHS_PG_DUMP``
     - ``api_paths.pg_dump``
     - PG Dump API endpoint. More details at :ref:`pg_dump_api`.

   * - ``HASURA_GRAPHQL_API_PATHS_VERSION``
     - ``api_paths.version``
     - Version API endpoint. More details at :ref:`version_api`.

   * - ``HASURA_GRAPHQL_METADATA_DIRECTORY``
     - ``metadata_directory``
     - Defines the directory where the metadata files were stored.

   * - ``HASURA_GRAPHQL_MIGRATIONS_DIRECTORY``
     - ``migrations_directory``
     - Defines the directory where the migration files were stored.

   * - ``HASURA_GRAPHQL_ACTIONS_KIND``
     - ``actions.kind``
     - Kind to be used for actions.

   * - ``HASURA_GRAPHQL_ACTIONS_HANDLER_WEBHOOK_BASEURL``
     - ``actions.handler_webhook_baseurl``
     - Webhook baseurl to be used for actions. 
   
   * - ``HASURA_GRAPHQL_ACTIONS_CODEGEN_FRAMEWORK``
     - ``actions.codegen.framework``
     - Framework to codegen for actions.
     
   * - ``HASURA_GRAPHQL_ACTION_CODEGEN_OUTPUT_DIR``
     - ``actions.codegen.output_dir``
     - Defines the directory to create the codegen files.

   * - ``HASURA_GRAPHQL_ACTION_CODEGEN_URI``
     - ``actions.codegen.uri``
     - URI to codegen for actions.

CLI flags
---------

The above keys can be set using command-line flags as well. The corresponding flag, 
for the ENV vars or the configuration keys, can be found in the respective commands 
reference manual. 

.env file
---------

Alternatively, environment variables can also be read from the ``.env`` file, created manually 
by the user, at the project root directory. A global flag, ``--envfile``, is available to 
explicitly get the ``.env`` file created by the user, which defaults to ``.env`` if 
no flag is provided. 

It's important to note that this will not override an ENV variable that already exists.

Example:

::

  hasura console --envfile production.env

The above command will read ENV vars from the ``production.env`` file present at the 
project root directory. 

.. admonition:: Supported from

   ``.env`` file is supported in versions ``v.1.2.0`` and above.

.. note::

  Precedence order is flag > ENV vars > ``.env`` file values > configuration file values > default.
