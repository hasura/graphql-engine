Nested object queries
=====================

You can use the object (one-to-one) or array (one-to-many) :doc:`relationships <../schema/relationships/index>` defined
in your schema to make a nested query i.e. fetch data for a type along with data from a nested or related type.

Query using an array relationship
---------------------------------
The following is an example of a nested object query using the ``array relationship`` between an author and
articles.

Fetch a list of authors and a nested list of each author’s articles:

.. graphiql::
  :view_only:
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

Query using an object relationship
----------------------------------
The following is an example of a nested object query using the ``object relationship`` between an article and an
author.

Fetch a list of articles and the name of each article’s author:

.. graphiql::
  :view_only:
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
    console.
