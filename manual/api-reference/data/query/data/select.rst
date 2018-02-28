.. .. meta::
   :description: Hasura's Data microservice's select query - JSON body's syntax, description, response params and examples.
   :keywords: hasura, docs, data, query reference, select query

.. _data_select:

select
------

Syntax
^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - table
     - true
     - :ref:`TableName <TableName>`
     - Name of the table
   * - columns
     - true
     - SelectColumn_ array
     - Return these columns of the selected rows
   * - where
     - false
     - :ref:`BoolExp <BoolExp>`
     - Select rows where this condition holds true
   * - order_by
     - false
     - :ref:`OrderByExp <OrderByExp>`
     - Define the ordering of the returned rows
   * - limit
     - false
     - Integer
     - Limit the result to these number of rows
   * - offset
     - false
     - Integer
     - The offset from which the rows have to be returned

.. note:: ``limit`` and ``offset`` only make sense when used with ``order_by``. Otherwise, the result may differ every time the query is run

.. _SelectColumn:

``SelectColumn``
&&&&&&&&&&&&&&&&

   Any of :ref:`PGColumn <PGColumn>` / ObjRelationship_ / ArrRelationship_

.. _ObjRelationship:

``ObjRelationship``
&&&&&&&&&&&&&&&&&&&

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - name
     - true
     - :ref:`RelationshipName <RelationshipName>`
     - Name of the relationship
   * - columns
     - true
     - SelectColumn_ array
     - Return these columns of the relationship

.. _ArrRelationship:

``ArrRelationship``
&&&&&&&&&&&&&&&&&&&

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - name
     - true
     - :ref:`RelationshipName <RelationshipName>`
     - Name of the relationship
   * - columns
     - true
     - SelectColumn_ array
     - Return these columns of the relationship
   * - where
     - false
     - :ref:`BoolExp <BoolExp>`
     - Select objects of the relationship where this condition holds true
   * - order_by
     - false
     - :ref:`OrderByExp <OrderByExp>`
     - Define the ordering of the returned objects
   * - limit
     - false
     - Integer
     - Limit the returned objects to this number
   * - offset
     - false
     - Integer
     - The offset from which the objects have to be returned

.. _OrderByExp:

``OrderByExp``
&&&&&&&&&&&&&&

   OrderByItem_ or OrderByItem_ array

``OrderByItem``
&&&&&&&&&&&&&&&

.. parsed-literal::
   :class: haskell-pre

   "+/-" ++ :ref:`PGColumn <PGColumn>`

   or

   {
       "column" : :ref:`PGColumn <PGColumn>`,
       "order"  : "asc" | "desc",
       "nulls"  : "first" | "last"
   }

Response
^^^^^^^^
   An array of `Object <Object>`. The structure of each object is defined by the ``columns`` of the query.
