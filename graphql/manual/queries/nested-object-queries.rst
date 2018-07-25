Nested object queries
=====================

You can use the 1:1 (object)  or 1:m (array) relationships defined in your schema (using the console) to make a
nested query i.e. fetch data for a type along with data from a nested or related type.

Query using a 1:many relationship
---------------------------------
The following is an example of nested object query using the 1:many or an ``array relationship`` between an author and
articles.

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
            "name": "Justin",
            "articles": [
              {
                "id": 15,
                "title": "vel dapibus at"
              },
              {
                "id": 16,
                "title": "sem duis aliquam"
              }
            ]
          },
          {
            "id": 2,
            "name": "Beltran",
            "articles": [
              {
                "id": 2,
                "title": "a nibh"
              },
              {
                "id": 9,
                "title": "sit amet"
              }
            ]
          },
          {
            "id": 3,
            "name": "Sidney",
            "articles": [
              {
                "id": 6,
                "title": "sapien ut"
              },
              {
                "id": 11,
                "title": "turpis eget"
              },
              {
                "id": 14,
                "title": "congue etiam justo"
              }
            ]
          }
        ]
      }
    }

Query using a 1:1 relationship
------------------------------
The following is an example of nested object query using the 1:1 or an ``object relationship`` between an article and an
author.

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
            "id": 1,
            "title": "sit amet",
            "author": {
              "name": "Anjela"
            }
          },
          {
            "id": 2,
            "title": "a nibh",
            "author": {
              "name": "Beltran"
            }
          },
          {
            "id": 3,
            "title": "amet justo morbi",
            "author": {
              "name": "Anjela"
            }
          }
        ]
      }
    }

.. note::
    
    The name of the nested object is the same as the name of the object or array relationship configured in the
    console
