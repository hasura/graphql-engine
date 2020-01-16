.. meta::
   :description: Make distinct queries with Hasura
   :keywords: hasura, docs, query, distinct query

Distinct query results
======================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

The **distinct_on** argument
----------------------------

You can fetch rows with only distinct values of a column using the ``distinct_on`` argument.

This requires the data to be first sorted by the column i.e. the ``distinct_on`` column should also be the first
``order_by`` column. See :doc:`sort queries <sorting>` for more info on using ``order_by``.

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
     
