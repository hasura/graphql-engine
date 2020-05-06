.. meta::
   :description: Supported PostgreSQL types in the Hasura API
   :keywords: hasura, docs, PostgreSQL types, API reference

.. _api_postgres_types:

API Reference - Supported PostgreSQL Types
==========================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

.. _types_table:

List of PostgreSQL types supported by the Hasura GraphQL engine with their equivalent Hasura types:

.. csv-table::
   :file: pgtypes.csv
   :widths: 13, 11, 25, 6
   :header-rows: 1

.. _Int:

Int
---
GraphQL default scalar with name **Int**.

E.g.

.. code-block:: graphql

   objects: [
     {
       id: 1,
       int_col: 27
     }
   ]

.. _Float:

Float
-----
GraphQL custom scalar type with name **float8**.

E.g.

.. code-block:: graphql

   objects: [
     {
       id: 1,
       float_col: 0.8
     }
   ]

.. _Numeric:

Numeric
-------
GraphQL custom scalar type with name **numeric**.

E.g.

.. code-block:: graphql

   objects: [
     {
       id: 1,
       numeric_col: 0.00000008
     }
   ]

.. _Bool:

Bool
----
GraphQL default Scalar with name **Boolean**. The **Boolean** scalar type represents ``true`` or ``false``.

E.g.

.. code-block:: graphql

   objects: [
     {
       id: 1,
       is_published: true
     }
   ]

.. _Char:

Char
----
GraphQL custom scalar with name **character**. It is a ``String`` with single character.

E.g.

.. code-block:: graphql

   objects: [
     {
       id: 1,
       char_column: "a"
     }
   ]


.. _String:

String
------
GraphQL default scalar with name **String**. The **String** scalar type represents textual data, represented as UTF-8 character sequences.
The String type is most often used by GraphQL to represent free-form human-readable text.

E.g.

.. code-block:: graphql

   objects: [
     {
       id: 1,
       name: "Raven"
     }
   ]


.. _Date:

Date
----
GraphQL custom scalar with name **date**. Date (no time of day). Allowed values are yyyy-mm-dd.

E.g.

.. code-block:: graphql

   objects: [
     {
       id: 1,
       date: "1996-03-15"
     }
   ]

.. _Timetz:

Time with time zone
-------------------
GraphQL custom scalar type with name **timetz**. Time of day only, with time zone. Allowed values should be of ISO8601 format
(e.g. 17:30:15Z, 17:30:15+05:30, 17:30:15.234890+05:30).

E.g.

.. code-block:: graphql

   objects: [
     {
       id: 1,
       time: "17:30:15+05:30"
     }
   ]

.. _Timestamptz:

Timestamp with time zone
------------------------
GraphQL custom scalar type with name **timestamptz**. Both date and time, with time zone. Allowed values should be of ISO8601 format
(e.g. 2016-07-20T17:30:15Z, 2016-07-20T17:30:15+05:30, 2016-07-20T17:30:15.234890+05:30).

E.g.

.. code-block:: graphql

   objects: [
     {
       id: 1,
       timestamptz_col: "2016-07-20T17:30:15+05:30"
     }
   ]

.. _JSON:

JSON
----
GraphQL custom scalar type with name **json**. It is a stringified json value.

E.g.

.. code-block:: graphql

   objects: [
     {
       id: 1,
       json_col: "{ \"name\": \"raven\" }"
     }
   ]

.. _JSONB:

JSONB
-----
GraphQL custom scalar type with name **jsonb**. Value should be given through a variable of type **jsonb**.

E.g.

.. code-block:: graphql

   mutation insert_test($value : jsonb) {
     insert_test(
       objects: [
         {
           jsonb_col: $value
         }
       ]
     ) {
        affected_rows
        returning{
          id
          details
        }
     }
   }

variables:

.. code-block:: json

   {
     "value": {
       "name": "raven"
     }
   }

.. _Geometry:

Geometry
--------

GraphQL custom scalar type ``geometry`` is generated for a ``GEOMETRY`` column
on a PostGIS enabled Postgres instance. Value should be given as GeoJSON.

E.g.

.. code-block:: graphql

   mutation insertGeometry($point: geometry!) {
     insert_test(
       objects: [{
         geometry_col: $point
       }]
     ) {
       affected_rows
       returning {
         geometry_col
       }
     }
   }

variables:

.. code-block:: json

   {
     "point": {
       "type": "Point",
       "coordinates": [0, 0]
     }
   }


.. _Geography:

Geography
---------

GraphQL custom scalar type ``geography`` is generated for a ``GEOGRAPHY`` column
on a PostGIS enabled Postgres instance. Value should be given as GeoJSON.

E.g.

.. code-block:: graphql

   mutation insertGeography($point: geography!) {
     insert_test(
       objects: [{
         geography_col: $point
       }]
     ) {
       affected_rows
       returning {
         geography_col
       }
     }
   }

variables:

.. code-block:: json

   {
     "point": {
       "type": "Point",
       "coordinates": [0, 0]
     }
   }

.. _Implicit:

Implicitly Supported types
--------------------------
All ``Implicit`` types in the :ref:`above table <types_table>` are implicitly supported by the GraphQL engine. You have to
provide the value as a **String**.


E.g. For time without time zone type

In ISO 8601 format

.. code-block:: graphql

   objects: [
     {
       id: 1,
       time_col: "04:05:06.789"
     }
   ]

E.g. For macaddr type

.. code-block:: graphql

   objects: [
     {
       id: 1,
       macaddr_col: "08:00:2b:01:02:03"
     }
   ]

.. Note::

   You can learn more about PostgreSQL data types `here <https://www.postgresql.org/docs/current/static/datatype.html>`__.


