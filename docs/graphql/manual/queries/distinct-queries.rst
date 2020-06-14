.. meta::
   :description: Make distinct queries with Hasura
   :keywords: hasura, docs, query, distinct query

.. _distinct_queries:

Distinct query results
======================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

The **distinct_on** argument
----------------------------

You can fetch rows with only distinct values of a column using the ``distinct_on`` argument.

It is typically recommended to use ``order_by`` along with ``distinct_on`` to ensure we get predictable results
*(otherwise any arbitrary row with a distinct value of the column may be returned)*.
Note that the ``distinct_on`` column needs to be the first column in the ``order_by`` expression.
See :ref:`sort queries <sort_query_results>` for more info on using ``order_by``.

.. code-block:: graphql

   employee (
     distinct_on: [employee_select_column]
     order_by: [employee_order_by]
   ): [employee]!

   # select column enum type for "employee" table
   enum employee_select_column {
     id
     name
     department
     salary
   }

You can see the complete specification of the ``distinct_on`` argument in the :ref:`API reference <DistinctOnExp>`.

Fetch results with distinct values of a particular field
--------------------------------------------------------

**For example**, fetch the employee with the highest salary from each department:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    .. graphiql::
      :view_only:
      :query:
          query {
            employee (
              distinct_on: [department]
              order_by: [
                {department: asc},
                {salary: desc}
              ]
            ) {
              id
              name
              department
              salary
            }
          }
      :response:
        {
          "data": {
            "employee": [
              {
                "id": 5,
                "name": "Kamila",
                "department": "Engineering",
                "salary": 4325
              },
              {
                "id": 4,
                "name": "Damien",
                "department": "Product",
                "salary": 3124
              },
              {
                "id": 7,
                "name": "Rickard",
                "department": "Services",
                "salary": 2223
              }
            ]
          }
        }
        
  .. tab:: Via API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "query { employee (distinct_on: [department] order_by: [{ department: asc }, { salary: desc }]) { id name department salary }}"
      }
