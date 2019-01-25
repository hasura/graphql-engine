Distinct query results
======================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

You can fetch rows with only distinct values of a column using the ``distinct_on`` argument. The first ``order_by``
columns must match the ``distinct_on`` column. See :doc:`sort queries <sorting>` for more info on ``order_by``.

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

Fetch results with distinct values of a particular field
--------------------------------------------------------

**For example**, fetch highest salaried employee from each department:

.. graphiql::
   :view_only:
   :query:
      query {
        employee(
          distinct_on: [department]
          order_by: [{department: asc}, {salary: desc}]
        ){
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
     
