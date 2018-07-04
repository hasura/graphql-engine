Nested object queries
=====================

You can use the 1:1 (object)  or 1:m (array) relationships defined in your schema (in the API-console) to make a nested query i.e. fetch data for a type along with data from a nested or related type.

Query using a 1:many relationship
---------------------------------
The following is an example of nested object query using the 1:many or an array relationship between an author and articles.

Example: Nested object query over a 1:many relationship
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Fetch a list of authors and a nested list of the each author’s articles:

.. graphiql::
  :query:
    query {
      author {
        id
        name
        articles {
          id
          title
        }
      }
    }
  :response:
    {
      "data": {
        "author": [
          {
            "id": 1,
            "name": "Chrissie",
            "articles": [
              {
                "id": 98,
                "title": "some title"
              },
              {
                "id": 73,
                "title": "some title"
              },
              {
                "id": 87,
                "title": "some title"
              }
            ]
          },
          {
            "id": 2,
            "name": "Aubrey",
            "articles": [
              {
                "id": 51,
                "title": "some title"
              },
              {
                "id": 41,
                "title": "some title"
              },
              {
                "id": 19,
                "title": "some title"
              }
            ]
          },
          {
            "id": 29,
            "name": "Carmella",
            "articles": [
              {
                "id": 78,
                "title": "some title"
              },
              {
                "id": 64,
                "title": "some title"
              }
            ]
          }
        ]
      }
    }

Query using a 1:1 relationship
------------------------------
The following is an example of nested object query using the 1:1 or an object relationship between an article and an author.

Example: Nested object query over a 1:1 relationship
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Fetch a list of articles and the name of each article’s author:

.. graphiql::
  :query:
    query {
      article {
        id
        title
        author {
          name
        }
      }
    }
  :response:
    {
      "data": {
        "article": [
          {
            "id": 3,
            "title": "some title",
            "author": {
              "name": "Derril"
            }
          },
          {
            "id": 4,
            "title": "some title",
            "author": {
              "name": "Dreddy"
            }
          },
          {
            "id": 5,
            "title": "some title",
            "author": {
              "name": "Mallorie"
            }
          },
          {
            "id": 6,
            "title": "some title",
            "author": {
              "name": "Saunderson"
            }
          }
        ]
      }
    }

.. note::
    
    The name of the nested object is the same as the name of the `1:many` or `1:1` relationship configured in the API-Console
