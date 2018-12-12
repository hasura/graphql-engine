Distinct queries
================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

You can fetch distinct columns using ``distinct_on`` argument. Initial ``order_by`` columns must
match ``distinct_on`` columns. Learn more about ``order_by`` :doc:`here <sorting>`.

.. code-block:: graphql

   employee (
     distinct_on: [employee_select_column]
     order_by: [employee_order_by]
   ): [employee]!

   #select column enum type for "employee" table
   enum employee_select_column {
     id
     name
     department
     salary
   }

For example, fetch highest salaried employee from each department:

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
     
