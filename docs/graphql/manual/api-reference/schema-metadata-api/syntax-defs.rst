Schema/Metadata API reference: Syntax definitions
=================================================

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

QualifiedTable
^^^^^^^^^^^^^^

.. parsed-literal::
   :class: haskell-pre

   {
           "name": String,
           "schema": String
   }

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

.. _BoolExp:

BoolExp
^^^^^^^

.. parsed-literal::
   :class: haskell-pre

   AndExp_ | OrExp_ | NotExp_ | TrueExp_ | ColumnExp_

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

Operator
^^^^^^^^

Generic operators (all column types except json, jsonb) :

- ``"$eq"``
- ``"$ne"``
- ``"$in"``
- ``"$nin"``
- ``"$gt"``
- ``"$lt"``
- ``"$gte"``
- ``"$lte"``

Text related operators :

- ``"$like"``
- ``"$nlike"``
- ``"$ilike"``
- ``"$nilike"``
- ``"$similar"``
- ``"$nsimilar"``

Operators for comparing columns (all column types except json, jsonb):

- ``"$ceq"``
- ``"$cne"``
- ``"$cgt"``
- ``"$clt"``
- ``"$cgte"``
- ``"$clte"``

Checking for NULL values :

- ``_is_null`` (takes true/false as values)

JSONB operators :

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

PostGIS related operators on GEOMETRY columns:

.. list-table::
   :header-rows: 1

   * - Operator
     - PostGIS equivalent
   * - ``_st_contains``
     - ``ST_Contains``
   * - ``_st_crosses``
     - ``ST_Crosses``
   * - ``_st_equals``
     - ``ST_Equals``
   * - ``_st_intersects``
     - ``ST_Intersects``
   * - ``_st_overlaps``
     - ``ST_Overlaps``
   * - ``_st_touches``
     - ``ST_Touches``
   * - ``_st_within``
     - ``ST_Within``
   * - ``_st_d_within``
     - ``ST_DWithin``

(For more details on what these operators do, refer to `PostGIS docs <http://postgis.net/workshops/postgis-intro/spatial_relationships.html>`__.)

.. note::

   - All operators take a JSON representation of ``geometry/geography`` values as input value.
   - Input value for ``_st_d_within`` operator is an object:

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
A JSONObject_ of Postgres column name to value mapping, where value can be static or derived from a session variable.

.. parsed-literal::
   :class: haskell-pre

   {
      "column1" : colVal1,
      "column2" : colVal2,
      ..
   }

E.g. where ``id`` is derived from session variable and ``city`` is a static value.

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
      "forward_client_headers": boolean
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
