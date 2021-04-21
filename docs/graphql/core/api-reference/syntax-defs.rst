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
     - ``String`` | FromEnv_
     - The database connection URL string, or as an environment variable
   * - pool_settings
     - true
     - PGPoolSettings_
     - Connection pool settings
   * - use_prepared_statements
     - false
     - Boolean
     - If set to ``true`` the server prepares statement before executing on the source database (default: ``false``).
       For more details, refer to the `Postgres docs <https://www.postgresql.org/docs/current/sql-prepare.html>`__


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
   * - custom_column_names
     - false
     - :ref:`CustomColumnNames`
     - Customise the column fields

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
     - ``String``
     - Customise the ``<table-name>`` root field
   * - select_by_pk
     - false
     - ``String``
     - Customise the ``<table-name>_by_pk`` root field
   * - select_aggregate
     - false
     - ``String``
     - Customise the ``<table-name>_aggregete`` root field
   * - insert
     - false
     - ``String``
     - Customise the ``insert_<table-name>`` root field
   * - insert_one
     - false
     - ``String``
     - Customise the ``insert_<table-name>_one`` root field
   * - update
     - false
     - ``String``
     - Customise the ``update_<table-name>`` root field
   * - update_by_pk
     - false
     - ``String``
     - Customise the ``update_<table-name>_by_pk`` root field
   * - delete
     - false
     - ``String``
     - Customise the ``delete_<table-name>`` root field
   * - delete_by_pk
     - false
     - ``String``
     - Customise the ``delete_<table-name>_by_pk`` root field

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
       PGColumn_ : { Operator_ : Value }
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
      "timeout_seconds": integer
   }

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

   The ``GraphQL Types`` used in creating an action must be defined before via :ref:`Custom Types <api_custom_types>`

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

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
      "message": {
         "arguments":{
            "message_id":"$id"
          }
      }
   }

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

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
