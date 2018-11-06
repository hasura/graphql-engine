Using Aggregations
==================

You can fetch aggregations on columns along with nodes using an aggregation query.
Available aggregation queries are ``count``, ``sum``, ``avg``, ``max`` and ``min``.

For example, fetch a list of authors with aggregations:

.. graphiql::
  :view_only:
  :query:
    query {
      author_aggregate {
        aggregate {
          count
          sum {
            id
          }
          avg {
            id
          }
          max {
            id
            name
          }
        }
        nodes {
          id
          name
        }
      }
    }
  :response:
    {
      "data": {
        "author": {
           "aggregate": {
              "count": 4,
              "sum": {
                "id": 10
              },
              "avg": {
                "id": 2.5
              },
              "max": {
                "id": 4,
                "name": "Sidney"
              }
           },
           "nodes": [
             {
               "id": 1,
               "name": "Justin"
             },
             {
               "id": 2,
               "name": "Beltran"
             },
             {
               "id": 3,
               "name": "Sidney"
             },
             {
               "id": 4,
               "name": "Anjela"
             }
          ]
        }
      }
    }
