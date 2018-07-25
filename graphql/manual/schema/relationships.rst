Relationships or connections between tables/views
=================================================
To make nested object queries, the tables/views in your database need to be connected using relationships.

Typically relationships are defined using foreign-key constraints. But in some cases, it might not be possible to
create foreign key constraints to create the link. For e.g. while trying to create a relationship involving a view
as foreign keys can't be created on views.

The following are examples to create relationships:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Using Foreign Keys

    In the previous section, we created two tables, ``author`` and ``article``. Let us now connect these tables to
    enable nested queries:

    **Add foreign-key constraint**

    In the console, navigate to the ``Modify`` tab of the ``article`` table. Edit the ``author_id`` column and configure
    it as a foreign-key for the ``id`` column in the ``author`` table.

    .. image:: ../../../img/graphql/manual/schema/add-foreign-key.png

    **Create 1:1 relationship**

    Each article has one author. This is an example of a 1:1 relationship a.k.a an ``object relationship``. The console
    infers this and recommends potential relationships in the ``Relationships`` tab. Let's add an ``object relationship``
    named ``author`` as shown here:

    .. image:: ../../../img/graphql/manual/schema/add-1-1-relationship.png

    Please note that the name of the relationship will also be used to reference the nested object. For e.g. we can now
    run a nested object query that is based on this ``object relationship`` i.e. fetch a list of articles and each
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

    **Create 1:many relationship**

    An author can write multiple articles. This is an example of a 1:many relationship a.ka. an ``array relationship``.
    You add an ``array relationship`` exactly how you add an ``object relationship`` in the console.

    .. image:: ../../../img/graphql/manual/schema/add-1-many-relationship.png

    We can now run a nested object query that is based on this ``array relationship`` i.e. fetch a list of authors and a
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

  .. tab:: Without Foreign Keys

      Let's say you have an ``author`` table and an ``author_avg_rating`` view with fields ``(id, avg)`` which has the
      average rating of articles for each author. To create an ``object relationship`` for ``author::id -> author_avg_rating::id``,
      navigate to the ``Relationships`` tab of the ``author`` table in the console:

      .. image:: ../../../img/graphql/manual/schema/manual-relationship-btn.png

      Click on *+ Add a manual relationship* button. This will open up a section as shown below:

      .. image:: ../../../img/graphql/manual/schema/manual-relationship-create.png

      For our case:

      - Relationship Type will be: ``Object Relationship``
      - Relationship Name can be: ``avg_rating``
      - Configuration: ``id :: author_avg_rating -> id``

      Now click on the *Add* button to create the relationship.

      We can now run a nested object query that is based on this ``object relationship``
      i.e. fetch a list of authors with the average rating of their articles:

      .. graphiql::
        :query:
          query {
            author {
              id
              name
              avg_rating {
                avg
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
                  "avg_rating": {
                    "avg": 2.5
                  }
                },
                {
                  "id": 2,
                  "name": "Beltran",
                  "avg_rating": {
                    "avg": 3
                  }
                },
                {
                  "id": 3,
                  "name": "Sidney",
                  "avg_rating": {
                    "avg": 2.6666666666666665
                  }
                }
              ]
            }
          }


For a list of all the different types of queries you can make, please see :doc:`this <../queries/index>`.