Queries
=======

Sample query
------------

.. graphiql::
   :query:
      query fetch_article {
        article {
          title
          comments {
            comment
          }
        }
      }
   :response:
      {
         "data": {
           "article": [
             {
               "title": "primis in faucibus orci luctus",
               "comments": []
             },
             {
               "title": "erat eget ipsum. Suspendisse sagittis.",
               "comments": []
             },
             {
               "title": "arcu. Sed et libero. Proin",
               "comments": []
             }
           ]
         }
       }


Bulk queries
------------

Multiple queries/mutations in single call

Nested object queries
---------------------

Fetch across tables using relationships

- array relations (multiple nested objects)
- object relations (single nested object)

Filter / Search
---------------

Possible to filter using own fields as well as nested objects' fields

- $eq/$ne - match exactly
- $gt/$lt - compare
- $like/$similar - pattern match strings for search
- $in/$nin - check presence in list
- $and/$or/$not - create boolean expressions of filters

Sort
----

Possible to sort using own fields as well as nested objects’ fields

- ascending / descending
- stable sort

Paginate
--------

Pagination possible for self as well as nested array objects

- limit - fetch fixed number of results
- offset - fetch results after given offset


Aggregations
------------

Possible to make aggregation queries using ``views``

- count
- min/max
- avg

Control access
--------------

Limit access to all fields using ``views``

