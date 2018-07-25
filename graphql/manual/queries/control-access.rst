Control access to certain data
==============================

If you want to control access to sensitive fields in a table, use views to expose only the safe fields.

Here’s an example on how you can do this. Our aim here is to mask access to the ``article`` table and only expose the
``id``, ``title`` and ``rating`` columns from this table.

Create a view
-------------
Create a view with data from only the required (or safe) columns:

.. code-block:: SQL

    CREATE VIEW article_safe AS
    SELECT id, title, rating 
    FROM article;

Modify permissions
------------------
You will need to revoke permission (if already granted) from the source table and grant access to the newly created view. So, in our example, we do the following:

#. Remove access permissions from the ``article`` table

#. Grant access permissions to the ``article_safe`` view

Query the view
--------------
You can now query the newly created view like you would a regular table. For e.g. the following query will access only the *safe* fields:

.. graphiql::
  :query:
    query {
      article_safe {
        id
        title
        rating
      }
    }
  :response:
    {
      "data": {
        "article_safe": [
          {
            "id": 1,
            "title": "sit amet",
            "rating": 1
          },
          {
            "id": 2,
            "title": "a nibh",
            "rating": 3
          },
          {
            "id": 3,
            "title": "amet justo morbi",
            "rating": 4
          },
          {
            "id": 4,
            "title": "vestibulum ac est",
            "rating": 2
          }
        ]
      }
    }
