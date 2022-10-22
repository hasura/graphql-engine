.. meta::
   :description: Common syntax definitions for the Hasura schema/metadata API
   :keywords: hasura, docs, schema/metadata API, API reference, syntax definitions

.. _api_metadata_syntax_defs:

API Reference: Common syntax definitions
========================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:


.. _TableName:

TableName
^^^^^^^^^

.. parsed-literal::
   :class: haskell-pre

   String | QualifiedTable_

.. _QualifiedTable:

QualifiedTable
^^^^^^^^^^^^^^

.. parsed-literal::
   :class: haskell-pre

   {
       "name": String,
       "schema": String
   }

.. _SourceName:

SourceName
^^^^^^^^^^

.. parsed-literal::

  String

.. _FunctionName:

FunctionName
^^^^^^^^^^^^

.. parsed-literal::
   :class: haskell-pre

   String | QualifiedFunction_

QualifiedFunction
^^^^^^^^^^^^^^^^^

.. parsed-literal::
   :class: haskell-pre

   {
       "name": String,
       "schema": String
   }

.. _RoleName:

RoleName
^^^^^^^^

.. parsed-literal::

  String

.. _ComputedFieldName:

ComputedFieldName
^^^^^^^^^^^^^^^^^^

.. parsed-literal::

  String

.. _PGConfiguration:

PGConfiguration
^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - connection_info
     - true
     - PGSourceConnectionInfo_
     - Connection parameters for the source
   * - read_replicas
     - false
     - [PGSourceConnectionInfo_]
     - Optional list of read replica configuration *(supported only in cloud/enterprise versions)*

.. _MsSQLConfiguration:

MsSQLConfiguration
^^^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - connection_info
     - true
     - MsSQLSourceConnectionInfo_
     - Connection parameters for the source
   * - read_replicas
     - false
     - [MsSQLSourceConnectionInfo_]
     - Optional list of read replica configuration *(supported only in cloud/enterprise versions)*


.. _BigQueryConfiguration:

BigQueryConfiguration
^^^^^^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - service_account
     - true
     - ``JSON String`` | ``JSON`` | FromEnv_
     - Service account for BigQuery database
   * - project_id
     - true
     - ``String`` | FromEnv_
     - Project Id for BigQuery database
   * - datasets
     - true
     - ``[String]`` | FromEnv_
     - List of BigQuery datasets


.. _PGSourceConnectionInfo:

PGSourceConnectionInfo
^^^^^^^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - database_url
     - true
     - ``String`` | FromEnv_ | PGConnectionParameters_
     - The database connection URL as a string, as an environment variable, or as connection parameters.
   * - pool_settings
     - false
     - PGPoolSettings_
     - Connection pool settings
   * - use_prepared_statements
     - false
     - Boolean
     - If set to ``true`` the server prepares statement before executing on the source database (default: ``false``).
       For more details, refer to the `Postgres docs <https://www.postgresql.org/docs/current/sql-prepare.html>`__
   * - isolation_level
     - false
     - ``read-committed`` | ``repeatable-read`` | ``serializable``
     - The transaction isolation level in which the queries made to the source will be run with (default: ``read-committed``).
   * - ssl_configuration
     - false
     - PGCertSettings_
     - The client SSL certificate settings for the database (*Only available in Cloud*).

.. _MsSQLSourceConnectionInfo:

MsSQLSourceConnectionInfo
^^^^^^^^^^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - connection_string
     - true
     - ``String`` | FromEnv_
     - The database connection string, or as an environment variable
   * - pool_settings
     - false
     - MsSQLPoolSettings_
     - Connection pool settings


.. _FromEnv:

FromEnv
^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - from_env
     - true
     - ``String``
     - Name of the environment variable

.. _PGConnectionParameters:

PGConnectionParameters
^^^^^^^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - username
     - true
     - ``String``
     - The Postgres user to be connected
   * - password
     - false
     - ``String``
     - The Postgres user's password
   * - database
     - true
     - ``String``
     - The database name
   * - host
     - true
     - ``String``
     - The name of the host to connect to
   * - port
     - true
     - ``Integer``
     - The port number to connect with, at the server host


.. _PGPoolSettings:

PGPoolSettings
^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - max_connections
     - false
     - ``Integer``
     - Maximum number of connections to be kept in the pool (default: 50)
   * - idle_timeout
     - false
     - ``Integer``
     - The idle timeout (in seconds) per connection (default: 180)
   * - retries
     - false
     - ``Integer``
     - Number of retries to perform (default: 1)
   * - pool_timeout
     - false
     - ``Integer``
     - Maximum time to wait while acquiring a Postgres connection from the pool, in seconds (default: forever)
   * - connection_lifetime
     - false
     - ``Integer``
     - Time from connection creation after which the connection should be destroyed and a new one
       created. A value of 0 indicates we should never destroy an active connection. If 0 is
       passed, memory from large query results may not be reclaimed. (default: 600 sec)

.. _PGCertSettings:

PGCertSettings
^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - sslmode
     - true
     - ``String``
     - The SSL connection mode. See the libpq ssl `support docs <https://www.postgresql.org/docs/9.1/libpq-ssl.html>` for more details.
   * - sslrootcert
     - true
     - FromEnv_
     - Environment variable which stores trusted certificate authorities.
   * - sslcert
     - true
     - FromEnv_
     - Environment variable which stores the client certificate.
   * - sslkey
     - true
     - FromEnv_
     - Environment variable which stores the client private key.
   * - sslpassword
     - false
     - ``String`` | FromEnv_
     - Password in the case where the sslkey is encrypted.

.. _MsSQLPoolSettings:

MsSQLPoolSettings
^^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - max_connections
     - false
     - ``Integer``
     - Maximum number of connections to be kept in the pool (default: 50)
   * - idle_timeout
     - false
     - ``Integer``
     - The idle timeout (in seconds) per connection (default: 180)


.. _PGColumnType:

PGColumnType
^^^^^^^^^^^^

.. parsed-literal::

  String

1. Numeric types

   .. list-table::
      :widths: 12 10 20
      :header-rows: 1

      * - Type
        - Alias
        - Description

      * - ``serial``
        -
        - autoincrementing integer

      * - ``bigserial``
        -
        - autoincrementing bigint

      * - ``integer``
        -
        - 4 bytes, typical choice for integer

      * - ``smallint``
        -
        - 2 bytes

      * - ``bigint``
        -
        - 8 bytes

      * - ``real``
        - ``float4``
        - 6 decimal digits precision, inexact

      * - ``double precision``
        - ``float8``
        - 15 decimal digits precision, inexact

      * - ``numeric``
        - ``decimal``
        - arbitrary precision, exact

2. Character types

   .. list-table::
      :widths: 8 6 20
      :header-rows: 1

      * - Type
        - Alias
        - Description

      * - ``varchar``
        - ``text``
        - typical choice for storing string types

3. Date/Time types

   .. list-table::
      :widths: 8 6 20
      :header-rows: 1

      * - Type
        - Alias
        - Description

      * - ``timestamp with time zone``
        - ``timestamptz``
        - both date and time, with time zone. Allowed values should be of ISO8601 format. E.g. 2016-07-20T17:30:15Z, 2016-07-20T17:30:15+05:30, 2016-07-20T17:30:15.234890+05:30

      * - ``time with time zone``
        - ``timetz``
        - time of day only, with time zone. Allowed values should be of ISO8601 format. E.g. 17:30:15Z, 17:30:15+05:30, 17:30:15.234890+05:30

      * - ``date``
        -
        - date (no time of day). Allowed values are yyyy-mm-dd

4. Boolean type

   .. list-table::
      :widths: 8 6 20
      :header-rows: 1

      * - Type
        - Alias
        - Description

      * - ``boolean``
        -
        - state of true or false

5. JSON types

   .. list-table::
      :widths: 8 6 20
      :header-rows: 1

      * - Type
        - Alias
        - Description

      * - ``json``
        -
        - Stored as plain text

      * - ``jsonb``
        -
        - Stored in a binary format and can be indexed

.. _PGColumn:

PGColumn
^^^^^^^^

.. parsed-literal::

  String

.. _RelationshipName:

RelationshipName
^^^^^^^^^^^^^^^^

.. parsed-literal::

  String

.. _table_config:

Table Config
^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - custom_name
     - false
     - ``String``
     - Customise the ``<table-name>`` with the provided custom name value.
       The GraphQL nodes for the table will be generated according to the custom name.
   * - custom_root_fields
     - false
     - :ref:`Custom Root Fields <custom_root_fields>`
     - Customise the root fields
   * - column_config
     - false
     - :ref:`ColumnConfig`
     - Customise the columns
   * - custom_column_names (deprecated)
     - false
     - :ref:`CustomColumnNames`
     - Customise the column fields (deprecated in favour of custom_name on :ref:`ColumnConfig`)
   * - comment
     - false
     - ``String``
     - Customise the description shown in GraphQL introspection. If null or omitted then
       if a comment exists on the database table, it is used as the description
       (Postgres-only), and if not, an autogenerated description is used instead.

.. _custom_root_fields:

Custom Root Fields
^^^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - select
     - false
     - ``String`` | :ref:`CustomRootField`
     - Customise the ``<table-name>`` root field. Using a ``String`` customises the field name.
   * - select_by_pk
     - false
     - ``String`` | :ref:`CustomRootField`
     - Customise the ``<table-name>_by_pk`` root field. Using a ``String`` customises the field name.
   * - select_aggregate
     - false
     - ``String`` | :ref:`CustomRootField`
     - Customise the ``<table-name>_aggregate`` root field. Using a ``String`` customises the field name.
   * - insert
     - false
     - ``String`` | :ref:`CustomRootField`
     - Customise the ``insert_<table-name>`` root field. Using a ``String`` customises the field name.
   * - insert_one
     - false
     - ``String`` | :ref:`CustomRootField`
     - Customise the ``insert_<table-name>_one`` root field. Using a ``String`` customises the field name.
   * - update
     - false
     - ``String`` | :ref:`CustomRootField`
     - Customise the ``update_<table-name>`` root field. Using a ``String`` customises the field name.
   * - update_by_pk
     - false
     - ``String`` | :ref:`CustomRootField`
     - Customise the ``update_<table-name>_by_pk`` root field. Using a ``String`` customises the field name.
   * - delete
     - false
     - ``String`` | :ref:`CustomRootField`
     - Customise the ``delete_<table-name>`` root field. Using a ``String`` customises the field name.
   * - delete_by_pk
     - false
     - ``String`` | :ref:`CustomRootField`
     - Customise the ``delete_<table-name>_by_pk`` root field. Using a ``String`` customises the field name.

.. _CustomRootField:

CustomRootField
^^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - name
     - false
     - ``String``
     - The custom root field name
   * - comment
     - false
     - ``String``
     - Customise the description shown for the root field in GraphQL introspection. If null or
       omitted then an autogenerated description is used instead.

.. _ColumnConfig:

ColumnConfig
^^^^^^^^^^^^

A JSONObject_ of table column name to :ref:`ColumnConfigValue`.

.. parsed-literal::
   :class: haskell-pre

   {
      "column1" : ColumnConfigValue,
      "column2" : ColumnConfigValue,
      ...
   }

.. _ColumnConfigValue:

ColumnConfigValue
^^^^^^^^^^^^^^^^^

Configuration properties for particular column, as specified on :ref:`ColumnConfig`.

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - custom_name
     - false
     - ``String``
     - Customise the name of the field in the GraphQL schema
   * - comment
     - false
     - ``String``
     - Customise the description shown for the field in GraphQL introspection. If null or
       omitted then an autogenerated description is used instead.


.. _custom_function_root_fields:

Custom Function Root Fields
^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - function
     - false
     - ``String``
     - Customise the ``<function-name>`` root field
   * - function_aggregate
     - false
     - ``String``
     - Customise the ``<function-name>_aggregete`` root field

.. _InsertPermission:

InsertPermission
^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - check
     - true
     - :ref:`BoolExp`
     - This expression has to hold true for every new row that is inserted
   * - set
     - false
     - :ref:`ColumnPresetExp`
     - Preset values for columns that can be sourced from session variables or static values
   * - columns
     - false
     - :ref:`PGColumn` array (or) ``'*'``
     - Can insert into only these columns (or all when ``'*'`` is specified)
   * - backend_only
     - false
     - Boolean
     - When set to ``true`` the mutation is accessible only if ``x-hasura-use-backend-only-permissions``
       session variable exists and is set to ``true`` and request is made with ``x-hasura-admin-secret``
       set if any auth is configured

.. _SelectPermission:

SelectPermission
^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - columns
     - true
     - :ref:`PGColumn` array (or) ``'*'``
     - Only these columns are selectable (or all when ``'*'`` is specified)
   * - computed_fields
     - false
     - :ref:`ComputedFieldName` array
     - Only these computed fields are selectable
   * - filter
     - true
     - :ref:`BoolExp`
     - Only the rows where this expression holds true are selectable
   * - limit
     - false
     - ``Integer``
     - The maximum number of rows that can be returned
   * - allow_aggregations
     - false
     - ``Boolean``
     - Toggle allowing aggregate queries

.. _UpdatePermission:

UpdatePermission
^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - columns
     - true
     - :ref:`PGColumn` array (or) ``'*'``
     - Only these columns are selectable (or all when ``'*'`` is specified)
   * - filter
     - true
     - :ref:`BoolExp`
     - Only the rows where this precondition holds true are updatable
   * - check
     - false
     - :ref:`BoolExp`
     - Postcondition which must be satisfied by rows which have been updated
   * - set
     - false
     - :ref:`ColumnPresetExp`
     - Preset values for columns that can be sourced from session variables or static values.


.. _DeletePermission:

DeletePermission
^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - filter
     - true
     - :ref:`BoolExp`
     - Only the rows where this expression holds true are deletable

.. _ObjRelUsing:

ObjRelUsing
^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - foreign_key_constraint_on
     - false
     - :ref:`ObjRelUsingChoice <ObjRelUsingChoice>`
     - The column with foreign key constraint or the remote table and column
   * - manual_configuration
     - false
     - :ref:`ObjRelUsingManualMapping <ObjRelUsingManualMapping>`
     - Manual mapping of table and columns

.. note::

   There has to be at least one and only one of ``foreign_key_constraint_on``
   and ``manual_configuration``.

.. _ObjRelUsingChoice:

ObjRelUsingChoice
^^^^^^^^^^^^^^^^^

.. parsed-literal::
   :class: haskell-pre

   SameTable_ | RemoteTable_

SameTable
^^^^^^^^^

.. parsed-literal::

   PGColumn_

RemoteTable
^^^^^^^^^^^

.. parsed-literal::
   :class: haskell-pre

   {
       "table"  : TableName_,
       "column" : PGColumn_
   }

.. admonition:: Supported from

    Supported in ``v2.0.0-alpha.3`` and above.

.. _ObjRelUsingManualMapping:

ObjRelUsingManualMapping
^^^^^^^^^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - remote_table
     - true
     - :ref:`TableName`
     - The table to which the relationship has to be established
   * - column_mapping
     - true
     - Object (:ref:`PGColumn` : :ref:`PGColumn`)
     - Mapping of columns from current table to remote table
   * - insertion_order
     - false
     - :ref:`InsertOrder`
     - insertion order: before or after parent (default: before)


.. _InsertOrder:

InsertOrder
^^^^^^^^^^^

Describes when should the referenced table row be inserted in relation to the
current table row in case of a nested insert. Defaults to "before_parent".

.. parsed-literal::
   :class: haskell-pre

   "before_parent" | "after_parent"

.. admonition:: Supported from

    Supported in ``v2.0.0-alpha.3`` and above.

.. _ArrRelUsing:

ArrRelUsing
^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - foreign_key_constraint_on
     - false
     - ArrRelUsingFKeyOn_
     - The column with foreign key constraint
   * - manual_configuration
     - false
     - ArrRelUsingManualMapping_
     - Manual mapping of table and columns

ArrRelUsingFKeyOn
^^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - table
     - true
     - :ref:`TableName`
     - Name of the table
   * - column
     - true
     - :ref:`PGColumn`
     - Name of the column with foreign key constraint

ArrRelUsingManualMapping
^^^^^^^^^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - remote_table
     - true
     - :ref:`TableName`
     - The table to which the relationship has to be established
   * - column_mapping
     - true
     - Object (:ref:`PGColumn` : :ref:`PGColumn`)
     - Mapping of columns from current table to remote table

.. _BoolExp:

BoolExp
^^^^^^^

.. parsed-literal::
   :class: haskell-pre

   AndExp_ | OrExp_ | NotExp_ | ExistsExp_ | TrueExp_ | ColumnExp_

AndExp
^^^^^^

.. parsed-literal::
   :class: haskell-pre

   {
       "$and" : [BoolExp_],
   }

OrExp
^^^^^

.. parsed-literal::
   :class: haskell-pre

   {
       "$or"  : [BoolExp_],
   }

NotExp
^^^^^^

.. parsed-literal::
   :class: haskell-pre

   {
       "$not" : BoolExp_
   }

ExistsExp
^^^^^^^^^

.. parsed-literal::
   :class: haskell-pre

   {
       "$exists" : {
            "_table": TableName_,
            "_where": BoolExp_
       }
   }

TrueExp
^^^^^^^

.. parsed-literal::
   :class: haskell-pre

    {}

ColumnExp
^^^^^^^^^

.. parsed-literal::
   :class: haskell-pre

   {
       PGColumn_ | scalar ComputedFieldName_ : { Operator_ : Value }
   }

.. _MetadataOperator:

Operator
^^^^^^^^

**Generic operators (all column types except json, jsonb) :**

.. list-table::
   :header-rows: 1

   * - Operator
     - PostgreSQL equivalent
   * - ``"$eq"``
     - ``=``
   * - ``"$ne"``
     - ``<>``
   * - ``"$gt"``
     - ``>``
   * - ``"$lt"``
     - ``<``
   * - ``"$gte"``
     - ``>=``
   * - ``"$lte"``
     - ``<=``
   * - ``"$in"``
     - ``IN``
   * - ``"$nin"``
     - ``NOT IN``

(For more details, refer to the Postgres docs for `comparison operators <https://www.postgresql.org/docs/current/functions-comparison.html>`__ and `list based search operators <https://www.postgresql.org/docs/current/functions-comparisons.html>`__.)

**Text related operators :**

.. list-table::
   :header-rows: 1

   * - Operator
     - PostgreSQL equivalent
   * - ``"$like"``
     - ``LIKE``
   * - ``"$nlike"``
     - ``NOT LIKE``
   * - ``"$ilike"``
     - ``ILIKE``
   * - ``"$nilike"``
     - ``NOT ILIKE``
   * - ``"$similar"``
     - ``SIMILAR TO``
   * - ``"$nsimilar"``
     - ``NOT SIMILAR TO``
   * - ``$regex``
     - ``~``
   * - ``$iregex``
     - ``~*``
   * - ``$nregex``
     - ``!~``
   * - ``$niregex``
     - ``!~*``


(For more details on text related operators, refer to the `Postgres docs <https://www.postgresql.org/docs/current/functions-matching.html>`__.)

**Operators for comparing columns (all column types except json, jsonb):**

**Column Comparison Operator**

.. parsed-literal::
   :class: haskell-pre

   {
     PGColumn_: {
       Operator_: {
         PGColumn_ | ["$", PGColumn_]
       }
     }
   }

Column comparison operators can be used to compare columns of the same
table or a related table. To compare a column of a table with another column of :

1. The same table -

.. parsed-literal::
   :class: haskell-pre

   {
     PGColumn_: {
       Operator_: {
         PGColumn_
       }
     }
   }

2. The table on which the permission is being defined on -

.. parsed-literal::
   :class: haskell-pre

   {
     PGColumn_: {
       Operator_: {
         [$, PGColumn_]
       }
     }
   }

.. list-table::
   :header-rows: 1

   * - Operator
     - PostgreSQL equivalent
   * - ``"$ceq"``
     - ``=``
   * - ``"$cne"``
     - ``<>``
   * - ``"$cgt"``
     - ``>``
   * - ``"$clt"``
     - ``<``
   * - ``"$cgte"``
     - ``>=``
   * - ``"$clte"``
     - ``<=``

(For more details on comparison operators, refer to the `Postgres docs <https://www.postgresql.org/docs/current/functions-comparison.html>`__.)

**Checking for NULL values :**

.. list-table::
   :header-rows: 1

   * - Operator
     - PostgreSQL equivalent
   * - ``_is_null`` (takes true/false as values)
     - ``IS NULL``

(For more details on the ``IS NULL`` expression, refer to the `Postgres docs <https://www.postgresql.org/docs/current/functions-comparison.html>`__.)

**JSONB operators :**

.. list-table::
   :header-rows: 1

   * - Operator
     - PostgreSQL equivalent
   * - ``_contains``
     - ``@>``
   * - ``_contained_in``
     - ``<@``
   * - ``_has_key``
     - ``?``
   * - ``_has_keys_any``
     - ``?!``
   * - ``_has_keys_all``
     - ``?&``

(For more details on JSONB operators, refer to the `Postgres docs <https://www.postgresql.org/docs/current/static/functions-json.html#FUNCTIONS-JSONB-OP-TABLE>`__.)

**PostGIS related operators on GEOMETRY columns:**

.. list-table::
   :header-rows: 1

   * - Operator
     - PostGIS equivalent
   * - ``_st_contains``
     - ``ST_Contains(column, input)``
   * - ``_st_crosses``
     - ``ST_Crosses(column, input)``
   * - ``_st_equals``
     - ``ST_Equals(column, input)``
   * - ``_st_3d_intersects``
     - ``ST_3DIntersects(column, input)``
   * - ``_st_intersects``
     - ``ST_Intersects(column, input)``
   * - ``_st_overlaps``
     - ``ST_Overlaps(column, input)``
   * - ``_st_touches``
     - ``ST_Touches(column, input)``
   * - ``_st_within``
     - ``ST_Within(column, input)``
   * - ``_st_d_within``
     - ``ST_DWithin(column, input)``
   * - ``_st_3d_d_within``
     - ``ST_3DDWithin(column, input)``

(For more details on spatial relationship operators, refer to the `PostGIS docs <http://postgis.net/workshops/postgis-intro/spatial_relationships.html>`__.)

.. note::

   - All operators take a JSON representation of ``geometry/geography`` values as input value.
   - The input value for ``_st_d_within`` operator is an object:

     .. parsed-literal::

       {
         field-name : {_st_d_within: {distance: Float, from: Value} }
       }


.. _Object:

Object
^^^^^^

A JSONObject_

.. parsed-literal::
   :class: haskell-pre

   {
      "k1" : v1,
      "k2" : v2,
      ..
   }

.. _JSONObject: https://tools.ietf.org/html/rfc7159

.. _Empty Object:

Empty Object
^^^^^^^^^^^^

An empty JSONObject_

.. code-block:: json

   {}

.. _ColumnPresetExp:

ColumnPresetsExp
^^^^^^^^^^^^^^^^
A JSONObject_ of a Postgres column name to value mapping, where the value can be static or derived from a session variable.

.. parsed-literal::
   :class: haskell-pre

   {
      "column1" : colVal1,
      "column2" : colVal2,
      ..
   }

E.g. where ``id`` is derived from a session variable and ``city`` is a static value.

.. code-block:: json

   {
      "id" : "x-hasura-User-Id",
      "city" : "San Francisco"
   }

.. note::

   If the value of any key begins with "x-hasura-" (*case-insensitive*), the value of the column specified in the key will be derived from a session variable of the same name.

.. _RemoteSchemaName:

RemoteSchemaName
^^^^^^^^^^^^^^^^

.. parsed-literal::

  String

.. _RemoteSchemaDef:

RemoteSchemaDef
^^^^^^^^^^^^^^^

.. parsed-literal::
   :class: haskell-pre

   {
      "url" : url-string,
      "url_from_env" : env-var-string,
      "headers": [
           { "name": header-name-string,
             "value": header-value-string,
             "value_from_env": env-var-string
           }
      ],
      "forward_client_headers": boolean,
      "timeout_seconds": integer,
      "customization": RemoteSchemaCustomization_
   }

.. _RemoteSchemaCustomization:

RemoteSchemaCustomization
^^^^^^^^^^^^^^^^^^^^^^^^^

.. parsed-literal::
   :class: haskell-pre

   {
      "root_fields_namespace": String,
      "type_names": {
        "prefix": String,
        "suffix": String,
        "mapping": {
          String: String
        }
      },
      "field_names": [
        { "parent_type": String,
          "prefix": String,
          "suffix": String,
          "mapping": {
            String: String
          }
        }
      ]
   }

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - ``root_fields_namespace``
     - false
     - String
     - If provided, the fields of the remote schema will be nested under this top level field
   * - ``type_names``
     - false
     - RemoteTypeCustomization_
     - Customization of type names in the remote schema
   * - ``field_names``
     - false
     - [RemoteFieldCustomization_]
     - Customization of field names for types in the remote schema

.. _RemoteTypeCustomization:

RemoteTypeCustomization
^^^^^^^^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - ``prefix``
     - false
     - String
     - Prefix applied to type names in the remote schema
   * - ``suffix``
     - false
     - String
     - Suffix applied to type names in the remote schema
   * - ``mapping``
     - false
     - ``{String: String}``
     - Explicit mapping of type names in the remote schema
       Note: explicit mapping takes precedence over ``prefix`` and ``suffix``.

- Type name prefix and suffix will be applied to all types in the schema
  except the root types (for query, mutation and subscription),
  types starting with ``__``, standard scalar types (``Int``, ``Float``, ``String``, ``Boolean``, and ``ID``),
  and types with an explicit mapping.
- Root types, types starting with ``__``,  and standard scalar types may only be customized with an explicit mapping.


.. _RemoteFieldCustomization:

RemoteFieldCustomization
^^^^^^^^^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - ``parent_type``
     - true
     - String
     - Name of the parent type (in the original remote schema) for fields to be customized
   * - ``prefix``
     - false
     - String
     - Prefix applied to field names in parent type
   * - ``suffix``
     - false
     - String
     - Suffix applied to field names in the parent type
   * - ``mapping``
     - false
     - ``{String: String}``
     - Explicit mapping of field names in the parent type
       Note: explicit mapping takes precedence over ``prefix`` and ``suffix``.

- Fields that are part of an interface must be renamed consistently across all object types that implement that interface.


.. _SourceCustomization:

SourceCustomization
^^^^^^^^^^^^^^^^^^^

.. parsed-literal::
   :class: haskell-pre
  {
    "root_fields": {
      "namespace": String,
      "prefix": String,
      "suffix": String
    },
    "type_names": {
      "prefix": String,
      "suffix": String
    }
  }

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - ``root_fields``
     - false
     - RootFieldsCustomization_
     - Customization of root field names for a source
   * - ``type_names``
     - false
     - SourceTypeCustomization_
     - Customization of type names for a source


.. _RootFieldsCustomization:

RootFieldsCustomization
^^^^^^^^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - ``namespace``
     - false
     - String
     - Namespace root field under which fields for this source will be nested
   * - ``prefix``
     - false
     - String
     - Prefix to be prepended to all root fields in this source
   * - ``suffix``
     - false
     - String
     - Suffix to be appended to all root fields in this source


.. _SourceTypeCustomization:

SourceTypeCustomization
^^^^^^^^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - ``prefix``
     - false
     - String
     - Prefix to be prepended to all type names in this source
   * - ``suffix``
     - false
     - String
     - Suffix to be appended to all type names in this source


.. _CollectionName:

CollectionName
^^^^^^^^^^^^^^

.. parsed-literal::

  String

.. _QueryName:

QueryName
^^^^^^^^^

.. parsed-literal::

  String

.. _CollectionQuery:

CollectionQuery
^^^^^^^^^^^^^^^

.. parsed-literal::
   :class: haskell-pre

   {
       "name": String,
       "query": String
   }

.. _AllowlistScope:

AllowlistScope
^^^^^^^^^^^^^^

.. parsed-literal::

   {
       "global": Boolean,
       "roles" : [RoleName]
   }

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - global
     - false
     - Boolean
     - When set to ``false`` a non empty array of role names is expected in
       the ``roles`` key.
       When set to ``true``, the ``roles`` key must be omitted.
       (default: ``true``)
   * - roles
     - when ``global`` is set to ``false``
     - [ :ref:`RoleName` ]
     - Roles to which the a query collection's queries should be accessible.
       *(supported only in cloud/enterprise versions)*

.. _EndpointUrl:

EndpointUrl
^^^^^^^^^^^

.. parsed-literal::

  String

.. _EndpointMethods:

EndpointMethods
^^^^^^^^^^^^^^^

.. parsed-literal::

  [String]

.. _EndpointDef:

EndpointDefinition
^^^^^^^^^^^^^^^^^^

.. parsed-literal::
   :class: haskell-pre

   {
       "query": {
         "query_name : String,
         "collection_name" : CollectionName
       }
   }


.. _CustomColumnNames:

CustomColumnNames
^^^^^^^^^^^^^^^^^

.. admonition:: Deprecation

  CustomColumnNames is deprecated in favour of using the ``custom_name`` property on columns in :ref:`ColumnConfig`.
  If both CustomColumnNames and :ref:`ColumnConfig` is used, any ``custom_name`` properties used in
  :ref:`ColumnConfig` will take precedence and any overlapped values in `custom_column_names` will be discarded.

A JSONObject_ of Postgres column name to GraphQL name mapping

.. parsed-literal::
   :class: haskell-pre

   {
      "column1" : String,
      "column2" : String,
      ..
   }

.. _ActionName:

ActionName
^^^^^^^^^^

.. parsed-literal::
   :class: haskell-pre

   String


.. _WebhookURL:

WebhookURL
^^^^^^^^^^

A String value which supports templating environment variables enclosed in ``{{`` and ``}}``.

.. parsed-literal::
   :class: haskell-pre

   String

Template example: ``https://{{ACTION_API_DOMAIN}}/create-user``

.. _HeaderFromValue:

HeaderFromValue
^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - required
     - Schema
     - Description
   * - name
     - true
     - String
     - Name of the header
   * - value
     - true
     - String
     - Value of the header

.. _HeaderFromEnv:


HeaderFromEnv
^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - required
     - Schema
     - Description
   * - name
     - true
     - String
     - Name of the header
   * - value_from_env
     - true
     - String
     - Name of the environment variable which holds the value of the header

.. _GraphQLType:

GraphQLType
^^^^^^^^^^^

A GraphQL `Type Reference <https://spec.graphql.org/June2018/#sec-Type-References>`__ string.

.. parsed-literal::
   :class: haskell-pre

   String

Example: ``String!`` for non-nullable String type and ``[String]`` for array of String types

.. _GraphQLName:

GraphQLName
^^^^^^^^^^^

A string literal that conform to `GraphQL spec <https://spec.graphql.org/June2018/#Name>`__.

.. parsed-literal::
   :class: haskell-pre

   String

.. _ActionDefinition:

ActionDefinition
^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - arguments
     - false
     - Array of InputArgument_
     - Input arguments
   * - output_type
     - true
     - :ref:`GraphQLType <GraphQLType>`
     - The output type of the action. Only object and list of objects are allowed.
   * - kind
     - false
     - [ ``synchronous`` | ``asynchronous`` ]
     - The kind of the mutation action (default: ``synchronous``). If the type of
       the action is ``query`` then the ``kind`` field should be omitted.
   * - headers
     - false
     - [ :ref:`HeaderFromValue <HeaderFromValue>` | :ref:`HeaderFromEnv <HeaderFromEnv>` ]
     - List of defined headers to be sent to the handler
   * - forward_client_headers
     - false
     - boolean
     - If set to ``true`` the client headers are forwarded to the webhook handler (default: ``false``)
   * - handler
     - true
     - :ref:`WebhookURL <WebhookURL>`
     - The action's webhook URL
   * - type
     - false
     - [ ``mutation`` | ``query`` ]
     - The type of the action (default: ``mutation``)
   * - timeout
     - false
     - Integer
     - Number of seconds to wait for response before timing out. Default: 30
   * - request_transform
     - false
     - :ref:`RequestTransformation`
     - Request Transformation to be applied to this Action's request
   * - response_transform
     - false
     - :ref:`ResponseTransformation`
     - Response Transformation to be applied to this Action's response


.. _InputArgument:

InputArgument
^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - name
     - true
     - text
     - Name of the argument
   * - type
     - true
     - :ref:`GraphQLType <GraphQLType>`
     - Type of the argument

.. note::

   The ``GraphQL Types`` used in creating an action must be defined before via :ref:`Custom Types <metadata_api_custom_types>`

.. _ComputedFieldDefinition:

ComputedFieldDefinition
^^^^^^^^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - function
     - true
     - :ref:`FunctionName <FunctionName>`
     - The SQL function
   * - table_argument
     - false
     - String
     - Name of the argument which accepts a table row type. If omitted, the first
       argument is considered a table argument
   * - session_argument
     - false
     - String
     - Name of the argument which accepts the Hasura session object as
       a JSON/JSONB value. If omitted, the Hasura session object is
       not passed to the function

.. _function_configuration:

Function Configuration
^^^^^^^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - custom_name
     - false
     - ``String``
     - Customise the ``<function-name>`` with the provided custom name value.
       The GraphQL nodes for the function will be generated according to the custom name.
   * - custom_root_fields
     - false
     - :ref:`Custom Function Root Fields <custom_function_root_fields>`
     - Customise the root fields

   * - session_argument
     - false
     - `String`
     - Function argument which accepts session info JSON
   * - exposed_as
     - false
     - `String`
     - In which part of the schema should we expose this function? Either "mutation" or "query".

.. _function_req_note:

.. note::

   Currently, only functions which satisfy the following constraints can be exposed over the GraphQL API
   (*terminology from* `Postgres docs <https://www.postgresql.org/docs/current/sql-createfunction.html>`__):

   - **Function behaviour**: ``STABLE`` or ``IMMUTABLE`` functions may *only* be exposed as queries (i.e. with ``exposed_as: query``)
     ``VOLATILE`` functions may be exposed as mutations or queries.
   - **Return type**: MUST be ``SETOF <table-name>`` OR ``<table_name>`` where ``<table-name>`` is already tracked
   - **Argument modes**: ONLY ``IN``


.. _InputObjectType:

InputObjectType
^^^^^^^^^^^^^^^

A simple JSON object to define `GraphQL Input Object <https://spec.graphql.org/June2018/#sec-Input-Objects>`__

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - name
     - true
     - :ref:`GraphQLName`
     - Name of the Input object type
   * - description
     - false
     - String
     - Description of the Input object type
   * - fields
     - true
     - Array of InputObjectField_
     - Fields of the Input object type

.. _InputObjectField:

InputObjectField
****************

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - name
     - true
     - :ref:`GraphQLName`
     - Name of the Input object field
   * - description
     - false
     - String
     - Description of the Input object field
   * - type
     - true
     - :ref:`GraphQLType <GraphQLType>`
     - GraphQL ype of the input object field


.. _ObjectType:

ObjectType
^^^^^^^^^^

A simple JSON object to define `GraphQL Object <https://spec.graphql.org/June2018/#sec-Objects>`__

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - name
     - true
     - :ref:`GraphQLName`
     - Name of the Object type
   * - description
     - false
     - String
     - Description of the Object type
   * - fields
     - true
     - Array of ObjectField_
     - Fields of the Object type
   * - relationships
     - false
     - Array of ObjectRelationship_
     - Relationships of the Object type to tables

.. _ObjectField:

ObjectField
***********

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - name
     - true
     - :ref:`GraphQLName`
     - Name of the Input object field
   * - description
     - false
     - String
     - Description of the Input object field
   * - type
     - true
     - :ref:`GraphQLType <GraphQLType>`
     - GraphQL type of the input object field

.. _ObjectRelationship:

ObjectRelationship
******************

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - name
     - true
     - :ref:`RelationshipName`
     - Name of the relationship, shouldn't conflict with existing field names
   * - type
     - true
     - [ ``object`` | ``array`` ]
     - Type of the relationship
   * - remote_table
     - true
     - :ref:`TableName`
     - The table to which relationship is defined
   * - field_mapping
     - true
     - Object (ObjectField_ name : Remote table's :ref:`PGColumn`)
     - Mapping of fields of object type to columns of remote table

.. _ScalarType:

ScalarType
^^^^^^^^^^

A simple JSON object to define `GraphQL Scalar <https://spec.graphql.org/June2018/#sec-Scalars>`__

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - name
     - true
     - :ref:`GraphQLName`
     - Name of the Scalar type
   * - description
     - false
     - String
     - Description of the Scalar type

.. _EnumType:

EnumType
^^^^^^^^

A simple JSON object to define `GraphQL Enum <https://spec.graphql.org/June2018/#sec-Enums>`__

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - name
     - true
     - :ref:`GraphQLName`
     - Name of the Enum type
   * - description
     - false
     - String
     - Description of the Enum type
   * - values
     - true
     - Array of EnumValue_
     - Values of the Enum type

.. _EnumValue:

EnumValue
*********

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - value
     - true
     - :ref:`GraphQLName`
     - Value of the Enum type
   * - description
     - false
     - String
     - Description of the value
   * - is_deprecated
     - false
     - Boolean
     - If set to ``true``, the enum value is marked as deprecated


.. _TriggerName:

TriggerName
^^^^^^^^^^^

.. parsed-literal::

  String

.. _OperationSpec:

OperationSpec
^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - columns
     - true
     - EventTriggerColumns_
     - List of columns or "*" to listen to changes
   * - payload
     - false
     - EventTriggerColumns_
     - List of columns or "*" to send as part of webhook payload

.. _EventTriggerColumns:

EventTriggerColumns
^^^^^^^^^^^^^^^^^^^

.. parsed-literal::
   :class: haskell-pre

   "*" | [:ref:`PGColumn`]

.. _RequestTransformation:

RequestTransformation
^^^^^^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - required
     - Schema
     - Description
   * - version
     - false
     - "1" | "2"
     - Sets the ``RequestTransformation`` schema version. Version ``1`` uses a ``String`` for the ``body`` field and Version ``2`` takes a :ref:`BodyTransform`. Defaults to version ``1``. Warning: You must remove any  "version 2" schemas from your metadata prior to downgrading to ``v2.4.0`` or earlier.
   * - method
     - false
     - String
     - Change the request method to this value.
   * - url
     - false
     - String
     - Change the request URL to this value.
   * - body
     - false
     - :ref:`BodyTransform` | String
     - A template script for transforming the request body.
   * - content_type
     - false
     - String
     - Replace the Content-Type with this value. Only "application/json" and "application/x-www-form-urlencoded" are allowed. Default: "application/json"
   * - query_params
     - false
     - Object (String : String)
     - Replace the query params on the URL with this value.
   * - request_headers
     - false
     - :ref:`TransformHeaders`
     - Request Header Transformation
   * - template_engine
     - false
     - :ref:`TemplateEngine`
     - Template language to be used for this transformation. Default: "Kriti"

.. note::

   HGE provides the following functions that can be used in the template:

   - ``not``: This function takes a boolean and returns its negation.

     eg::

        > {{not true}}
        false

   - ``escapeUri``: This function takes a string and escapes it as per URI specification.

     eg::

        > {{ escapeUri "?foo=bar/baz" }}
        "%3Ffoo%3Dbar%2Fbaz"


   - ``getSessionVariable``: This function takes a string and returns the session variable of the given name. This function can throw the following errors:

     - Session variable {variable name} not found
     - Session variable name should be a string

     eg::

        > {{getSessionVariable "myVariableName"}}
        "myVariableValue"

.. _TransformHeaders:

TransformHeaders
^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - required
     - Schema
     - Description
   * - add_headers
     - false
     - Object (:ref:`HeaderKey` : :ref:`HeaderValue`)
     - A map of Header Key Value pairs to be added to the request.
   * - remove_headers
     - false
     - Array of (:ref:`HeaderKey`)
     - Headers to be removed from the request.

.. _HeaderKey:

HeaderKey
^^^^^^^^^

.. parsed-literal::
   :class: haskell-pre

   String

.. _HeaderValue:

HeaderValue
^^^^^^^^^^^

.. parsed-literal::
   :class: haskell-pre

   String


.. _BodyTransform:

BodyTransform
^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - required
     - Schema
     - Description
   * - action
     - true
     - remove | transform
     - The action to perform on the request body.
   * - template
     - false
     - String
     - The transformation template to be applied to the body. This is
       required if the action is `transform`.
   * - form_template
     - false
     - Object (:ref:`String` : :ref:`String`)
     - The key/value pairs to be used in a `x-www-url-formencoded` body. The values can be transfomation templates.

.. _TemplateEngine:

TemplateEngine
^^^^^^^^^^^^^^

The JSON templating language to be used for this JSON transformation.

.. parsed-literal::
   :class: haskell-pre

   "Kriti"

.. _ResponseTransformation:

ResponseTransformation
^^^^^^^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - required
     - Schema
     - Description
   * - version
     - false
     - "1" | "2"
     - Sets the `RequestTransformation` schema version. Version `1` uses a `String` for the `body` field and Version `2` takes a :ref:`BodyTransform`. `Defaults to version `1`.
   * - body
     - false
     - :ref:`BodyTransform` | String
     - A template script for transforming the response body.
   * - template_engine
     - false
     - :ref:`TemplateEngine`
     - Template language to be used for this transformation. Default: "Kriti"

.. _RetryConf:

RetryConf
^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - required
     - Schema
     - Description
   * - num_retries
     - false
     - Integer
     - Number of times to retry delivery. Default: 0
   * - interval_sec
     - false
     - Integer
     - Number of seconds to wait between each retry. Default: 10
   * - timeout_sec
     - false
     - Integer
     - Number of seconds to wait for response before timing out. Default: 60

.. _RemoteRelationshipName:

RemoteRelationshipName
^^^^^^^^^^^^^^^^^^^^^^

.. parsed-literal::

  String

.. _RemoteField:

RemoteField
^^^^^^^^^^^

.. parsed-literal::
   :class: haskell-pre

   {
      FieldName: {
        "arguments": InputArguments
        "field": RemoteField  # optional
      }
   }


``RemoteField`` is a recursive tree structure that points to the field in the remote schema that needs to be joined with. It is recursive because the remote field maybe nested deeply in the remote schema.

Examples:

.. parsed-literal::
   :class: haskell-pre

   {
      "message": {
         "arguments":{
            "message_id":"$id"
          }
      }
   }

.. parsed-literal::
   :class: haskell-pre

   {
      "messages": {
         "arguments": {
            "limit": 100
         },
         "field": {
           "private": {
             "arguments": {
                "id" : "$id"
             }
           }
         }
      }
   }

InputArguments
^^^^^^^^^^^^^^

.. parsed-literal::
   :class: haskell-pre

   {
     InputField : $PGColumn | Scalar
   }

Table columns can be referred by prefixing ``$`` e.g ``$id``.

.. _RemoteSchemaPermission:

RemoteSchemaPermission
^^^^^^^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - schema
     - true
     - GraphQL SDL
     - GraphQL SDL defining the role based schema

UrlFromEnv
^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - required
     - Schema
     - Description
   * - from_env
     - true
     - String
     - Name of the environment variable which has the URL

.. _RetryConfST:

RetryConfST
^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - required
     - Schema
     - Description
   * - num_retries
     - false
     - Integer
     - Number of times to retry delivery. Default: 0
   * - retry_interval_seconds
     - false
     - Integer
     - Number of seconds to wait between each retry. Default: 10
   * - timeout_seconds
     - false
     - Integer
     - Number of seconds to wait for response before timing out. Default: 60
   * - tolerance_seconds
     - false
     - Integer
     - Number of seconds between scheduled time and actual delivery time that is acceptable. If the time difference is more than this, then the event is dropped. Default: 21600 (6 hours)
