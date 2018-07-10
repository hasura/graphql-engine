Relationships or connections between tables/views
=================================================
To make nested object queries, the tables/data in your database need to be connected using foreign-key constraints
and relationships. So far, we've only created the two tables, ``author`` and ``article``.


Add foreign-key constraint
--------------------------
In the console, navigate to the **Modify** tab of the ``article`` table. Edit the ``author_id`` column and configure
it as a foreign-key for the ``id`` column in the ``author`` table.

.. image:: ../../../img/graphql/manual/schema/add-foreign-key.png

Create 1:1 relationship
-----------------------
Each article has one author. This is an example of a 1:1 relationship a.k.a an *object* relationship. The console
infers this and recommends potential relationships in the **Relationships** tab. Let's add a 1:1 relationship named
*author* as shown here:

.. image:: ../../../img/graphql/manual/schema/add-1-1-relationship.png

Please note that the name of the relationship will also be used to reference the nested object. For e.g. we can now
run a nested object query that is based on just this one 1:1 relationship i.e. fetch a list of articles and each
article's author:

.. graphiql::
  :query:
    query {
      article {
        id
        title
        author {
          id
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
              "name": "Anjela",
              "id": 4
            }
          },
          {
            "id": 2,
            "title": "a nibh",
            "author": {
              "name": "Beltran",
              "id": 2
            }
          },
          {
            "id": 3,
            "title": "amet justo morbi",
            "author": {
              "name": "Anjela",
              "id": 4
            }
          }
        ]
      }
    }

Create 1:many relationship
--------------------------
An author can write multiple articles. This is an example of a 1:many relationship a.ka. an *array* relationship. You
add a 1:many relationship exactly how you add a 1:1 relationship in the console.

We can now run a nested object query that is based on this 1:many relationship i.e. fetch a list of authors and a
nested list of each author's articles:

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

For a list of all the different types of queries you can make, please see :doc:`this <../queries/index>`.