Schema/Metadata API reference: Syntax definitions
=================================================


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

   AndExp_ | OrExp_ | NotExp_ | ColumnExp_

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
