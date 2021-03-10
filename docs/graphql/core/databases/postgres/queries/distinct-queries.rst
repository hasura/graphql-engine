.. meta::
   :description: Make distinct queries on Postgres with Hasura
   :keywords: hasura, docs, postgres, query, distinct query

.. _distinct_queries:

Postgres: Distinct query results
================================

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

   employees (
     distinct_on: [employees_select_column]
     order_by: [employees_order_by]
   ): [employees]!

   # select column enum type for "employees" table
   enum employees_select_column {
     id
     name
     department
     salary
   }

You can see the complete specification of the ``distinct_on`` argument in the :ref:`API reference <DistinctOnExp>`.

Fetch results with distinct values of a particular field
--------------------------------------------------------

**For example**, fetch the employee with the highest salary from each department:

.. graphiql::
   :view_only:
   :query:
      query {
        employees (
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
         "employees": [
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
     
