.. meta::
   :description: Insert an object into the database using a mutation
   :keywords: hasura, docs, mutation, insert

.. _insert:

Insert mutation
===============

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Auto-generated insert mutation schema
-------------------------------------

**For example**, the auto-generated schema for the insert mutation field for a table ``article`` looks like this:

.. code-block:: graphql

  insert_article (
    objects: [article_insert_input!]!
    on_conflict: article_on_conflict
  ): article_mutation_response

  # response of any mutation on the table "article"
  type article_mutation_response {
    # number of affected rows by the mutation
    affected_rows: Int!
    # data of the affected rows by the mutation
    returning: [article!]!
  }

  # single object insert (supported from v1.2.0)
  insert_article_one (
    object: article_insert_input!
    on_conflict: article_on_conflict
  ): article

As you can see from the schema:

- ``objects`` argument is necessary and you can pass multiple ``objects`` to the mutation.
- You can pass an ``on_conflict`` argument to convert the mutation to an :ref:`upsert mutation <upsert>`.
- You can return the number of affected rows and the affected objects (with nested objects) in the response.
- You can use the single object insert to get the inserted object directly as the mutation response.

See the :ref:`insert mutation API reference <insert_upsert_syntax>` for the full specifications.

.. note::

  If a table is not in the ``public`` Postgres schema, the insert mutation field will be of the format
  ``insert_<schema_name>_<table_name>``.

Insert a single object
----------------------
**Example:** Insert a new ``article`` object and return the inserted article object in the response:

.. graphiql::
  :view_only:
  :query:
    mutation insert_single_article {
      insert_article_one(
        object: {
          title: "Article 1",
          content: "Sample article content",
          author_id: 3
        }
      ) {
        id
        title
      }
    }
  :response:
    {
      "data": {
        "insert_article_one": {
          "id": 21,
          "title": "Article 1"
        }
      }
    }

Using variables:

.. graphiql::
  :view_only:
  :query:
    mutation insert_single_article($object: article_insert_input! ) {
      insert_article_one(object: $object) {
        id
        title
      }
    }
  :response:
    {
      "data": {
        "insert_article_one": {
          "id": 21,
          "title": "Article 1"
        }
      }
    }
  :variables:
    {
      "object": {
        "title": "Article 1",
        "content": "Sample article content",
        "author_id": 3
      }
    }

.. note:: 

   ``insert_<object>_one`` will **only** be available if you have select permissions on the table, as it returns the inserted row.

.. admonition:: Supported from

   The ``insert_<object>_one`` mutation is supported in versions ``v1.2.0``
   and above.

Insert multiple objects of the same type in the same mutation
-------------------------------------------------------------
**Example:** Insert 2 new ``article`` objects and return both the article objects in the response:

.. graphiql::
  :view_only:
  :query:
    mutation insert_multiple_articles {
      insert_article(
        objects: [
          {
            title: "Article 2",
            content: "Sample article content",
            author_id: 4
          },
          {
            title: "Article 3",
            content: "Sample article content",
            author_id: 5
          }
        ]
      ) {
        returning {
          id
          title
        }
      }
    }
  :response:
    {
      "data": {
        "insert_article": {
          "affected_rows": 2,
          "returning": [
            {
              "id": 22,
              "title": "Article 2"
            },
            {
              "id": 23,
              "title": "Article 3"
            }
          ]
        }
      }
    }

Using variables:

.. graphiql::
  :view_only:
  :query:
    mutation insert_multiple_articles($objects: [article_insert_input!]! ) {
      insert_article(objects: $objects) {
        returning {
          id
          title
        }
      }
    }
  :response:
    {
        "data": {
          "insert_article": {
            "affected_rows": 2,
            "returning": [
              {
                "id": 22,
                "title": "Article 2"
              },
              {
                "id": 23,
                "title": "Article 3"
              }
            ]
          }
        }
      }
  :variables:
    {
      "objects": [
        {
          "title": "Article 2",
          "content": "Sample article content",
          "author_id": 4
        },
        {
          "title": "Article 3",
          "content": "Sample article content",
          "author_id": 5
        }
      ]
    }


Insert an object and get a nested object in response
----------------------------------------------------
**Example:** Insert a new ``article`` object and return the inserted article object with its author in the response:

.. graphiql::
  :view_only:
  :query:
    mutation insert_article {
      insert_article(
        objects: [
          {
            title: "Article 1",
            content: "Sample article content",
            author_id: 3
          }
        ]
      ) {
        returning {
          id
          title
          author {
            id
            name
          }
        }
      }
    }
  :response:
    {
      "data": {
        "insert_article": {
          "affected_rows": 1,
          "returning": [
            {
              "id": 21,
              "title": "Article 1",
              "author": {
                "id": 3,
                "name": "Sidney"
              }
            }
          ]
        }
      }
    }

.. _nested_inserts:

Insert an object along with its related objects through relationships
---------------------------------------------------------------------

One-to-one / One-to-many relationships
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Let's say an ``author`` has an ``object relationship`` called ``address`` to the ``addresses`` table and an
``array relationship`` called ``articles`` to the ``articles`` table.

**Example:** Insert an ``author`` along with their ``address`` and a few ``articles``.

.. graphiql::
  :view_only:
  :query:
    mutation insertData {
      insert_authors
        (objects: [
          {
            name: "John",
            address: {
              data: {
                location: "San Francisco"
              }
            },
            articles: {
              data: [
                {
                  title: "GraphQL Guide",
                  content: "Let's see what we can do with GraphQL"
                },
                {
                  title: "Authentication Guide",
                  content: "Let's look at best practices for authentication"
                }
              ]
            }
          }
        ]
      ) {
        affected_rows
        returning {
          id
          name
          address_id
          address {
            id
            location
          }
          articles {
            id
            title
            author_id
          }
        }
      }
    }
  :response:
    {
      "data": {
        "insert_authors": {
          "affected_rows": 4,
          "returning": [
            {
              "id": 26,
              "name": "John",
              "address_id": 27,
              "address": {
                "id": 27,
                "location": "San Francisco"
              },            
              "articles": [
                {
                  "id": 28,
                  "title": "GraphQL Guide",
                  "author_id": 26
                },
                {
                  "id": 29,
                  "title": "Authentication Guide",
                  "author_id": 26,
                }
              ]
            }
          ]
        }
      }
    }

**How it works**

A nested insert mutation is processed as follows:

1. The object relationship objects are inserted first, i.e. in this case, the ``address`` is inserted and its ``id`` is
   collected in this step.

2. The parent object is inserted next. i.e. in this case, the ``author`` is now inserted with the ``address_id`` being set
   to the ``id`` of the address that was inserted. Because of this, it is not allowed to pass ``address_id`` in the
   author object if you are also providing data for the address relationship.

   The ``id`` of the author is collected in this step.

3. The array relationship objects are inserted at the end. i.e. in this case, the ``articles`` are now inserted with their
   ``author_id`` set to the author's ``id`` collected in the step 2. Hence, it's not possible to specify ``author_id``
   in the data for the articles relationship.

Many-to-many relationships
^^^^^^^^^^^^^^^^^^^^^^^^^^
Let's say the ``articles`` has a :ref:`many-to-many relationship <many_to_many_modelling>` with the ``tags`` table via
a bridge table ``article_tags``.

**Example:** Insert an ``article`` along with a few ``tags``.

.. graphiql::
  :view_only:
  :query:
    mutation insertArticle {
      insert_articles(objects: [
        {
          title: "How to make fajitas",
          content: "Guide on making the best fajitas in the world",
          author_id: 3,
          article_tags: {
            data: [
              {
                tag: {
                  data: {
                    label: "Recipes"
                  },
                  on_conflict: {
                    constraint: tags_label_key,
                    update_columns: [label]
                  }
                }
              },
              {
                tag: {
                  data: {
                    label: "Cooking"
                  },
                  on_conflict: {
                    constraint: tags_label_key,
                    update_columns: [label]
                  }
                }
              }  
            ]
          }
        }
      ]) {
        affected_rows
        returning {
          id
          title
          content
          author_id
          article_tags {
            tag {
              label
            }
          }
        }
      }
    }
  :response:
    {
      "data": {
        "insert_articles": {
          "affected_rows": 5,
          "returning": [
            {
              "author_id": 3,
              "article_tags": [
                {
                  "tag": {
                    "label": "Recipes"
                  }
                },
                {
                  "tag": {
                    "label": "Cooking"
                  }
                }
              ],
              "content": "Guide on making the best fajitas in the world",
              "id": 34,
              "title": "How to make fajitas"
            }
          ]
        }
      }
    }

**How it works**

1. The parent object (from the perspective of ``article``) is inserted first i.e. the ``article`` is inserted.

   The ``id`` of the article is collected in this step.

2. The array relationship objects (from the perspective of ``article``) are inserted next i.e. the
   ``article_tags`` are inserted.

   1. The object relationship objects (from the perspective of ``article_tags``) are inserted now i.e.
      the ``tags`` are now inserted.

      The ``ids`` of the tags are collected in this step.

   2. The parent object (from the perspective of ``article_tags``) is inserted at the end i.e. the
      ``article_tags`` are now inserted with their ``article_id`` set to the article's ``id`` collected in step 1.
      The ``tag_id`` is set to the tag's ``id`` collected in step 2.1. Hence, it’s not possible to specify
      ``article_id`` and ``tag_id`` in the data for the `article_tags` relationship.

**on_conflict**

``on_conflict`` can be passed as an argument in a nested insert statement. In our example, we say that if the unique key (``label``) already
exists for a tag, we update the ``label`` of this respective tag (see :ref:`nested upsert caveats <nested-upsert-caveats>`).

Insert an object with a JSONB field
-----------------------------------
**Example:** Insert a new ``author`` object with a JSONB ``address`` field:

.. graphiql::
  :view_only:
  :query:
    mutation insert_author($address: jsonb) {
      insert_author (
        objects: [
          {
            name: "Ash",
            address: $address
          }
        ]
      ) {
        affected_rows
        returning {
          id
          name
          address
        }
      }
    }
  :response:
    {
      "data": {
        "insert_author": {
          "affected_rows": 1,
          "returning": [
            {
              "id": 1,
              "name": "Ash",
              "address": {
                "city": "Bengaluru",
                "phone": "9090909090",
                "state": "Karnataka",
                "pincode": 560095,
                "street_address": "161, 19th Main Road, Koramangala 6th Block"
              }
            }
          ]
        }
      }
    }
  :variables:
    {
      "address": {
        "street_address": "161, 19th Main Road, Koramangala 6th Block",
        "city": "Bengaluru",
        "phone": "9090909090",
        "state": "Karnataka",
        "pincode": 560095
      }
    }

Insert an object with an ARRAY field
------------------------------------

To insert fields of array types, you currently have to pass them as a `Postgres array literal <https://www.postgresql.org/docs/current/arrays.html#ARRAYS-INPUT>`_.

**Example:** Insert a new ``author`` with a text array ``emails`` field:

.. graphiql::
  :view_only:
  :query:
    mutation insert_author {
      insert_author (
        objects: [
          {
            name: "Ash",
            emails: "{ash@ash.com, ash123@ash.com}"
          }
        ]
      ) {
        affected_rows
        returning {
          id
          name
          emails
        }
      }
    }
  :response:
    {
      "data": {
        "insert_author": {
          "affected_rows": 1,
          "returning": [
            {
              "id": 1,
              "name": "Ash",
              "emails": ["ash@ash.com", "ash123@ash.com"]
            }
          ]
        }
      }
    }


Using variables:

.. graphiql::
  :view_only:
  :query:
    mutation insert_author($emails: _text) {
      insert_author (
        objects: [
          {
            name: "Ash",
            emails: $emails
          }
        ]
      ) {
        affected_rows
        returning {
          id
          name
          emails
        }
      }
    }
  :response:
    {
      "data": {
        "insert_author": {
          "affected_rows": 1,
          "returning": [
            {
              "id": 1,
              "name": "Ash",
              "emails": ["ash@ash.com", "ash123@ash.com"]
            }
          ]
        }
      }
    }
  :variables:
    {
      "emails": "{ash@ash.com, ash123@ash.com}"
    }

Set a field to its default value during insert
----------------------------------------------

To set a field to its ``default`` value, just omit it from the input object, irrespective of the
:ref:`default value configuration <postgres_defaults>` i.e. via Postgres defaults or using column presets.

**Example:** If the default value of ``id`` is set to auto-incrementing integer, there's no need to pass the ``id`` field to the input object:

.. graphiql::
  :view_only:
  :query:
    mutation insert_article_with_def_id {
      insert_article(
        objects: [
          {
            title: "Article 1",
            content: "Sample article content",
            author_id: 3
          }
        ]
      ) {
        returning {
          id
          title
        }
      }
    }
  :response:
    {
      "data": {
        "insert_article": {
          "affected_rows": 1,
          "returning": [
            {
              "id": 21,
              "title": "Article 1"
            }
          ]
        }
      }
    }

Set a field to NULL during insert
---------------------------------

If a field is ``nullable`` in the database, to set its value to ``null``, either pass its value as ``null`` or
just omit it from the input object.

**Example:** If ``age`` is a nullable field, to set it to ``null``, either don't pass the age field to the input object
or pass it as ``null``:

.. graphiql::
  :view_only:
  :query:
    mutation insert_author_with_null_age {
      insert_author(
        objects: [
          {
            name: "Jeff",
          }
        ]
      ) {
        returning {
          id
          name
          age
        }
      }
    }
  :response:
    {
      "data": {
        "insert_author": {
          "returning": [
            {
                "id": 11,
                "name": "Jeff",
                "age": null
            }
          ]
        }
      }
    }

OR

.. graphiql::
  :view_only:
  :query:
    mutation insert_author_with_null_age {
      insert_author(
        objects: [
          {
            name: "Jeff",
            age: null
          }
        ]
      ) {
        returning {
          id
          name
          age
        }
      }
    }
  :response:
    {
      "data": {
        "insert_author": {
          "returning": [
            {
                "id": 11,
                "name": "Jeff",
                "age": null
            }
          ]
        }
      }
    }
