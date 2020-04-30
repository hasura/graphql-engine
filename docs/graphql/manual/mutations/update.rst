.. meta::
   :description: Update an object in the database using a mutation
   :keywords: hasura, docs, mutation, update

.. _update:

Update mutation
===============

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Auto-generated update mutation schema
-------------------------------------

**For example**, the auto-generated schema for the update mutation field for a table ``article`` looks like this:

.. code-block:: graphql

  update_article (
    _inc: article_inc_input
    _set: article_set_input
    where: article_bool_exp!
  ): article_mutation_response

  # response of any mutation on the table "article"
  type article_mutation_response {
    # number of affected rows by the mutation
    affected_rows: Int!
    #data of the affected rows by the mutation
    returning: [article!]!
  }

  # single object update (supported from v1.2.0)
  update_article_by_pk (
    _inc: article_inc_input
    _set: article_set_input
    # primary key columns input value
    pk_columns: article_pk_columns_input!
  ): article # the article table object

As you can see from the schema:

- The ``where`` argument is compulsory to filter rows to be updated. See :ref:`Filter queries <filter_queries>`
  for filtering options. Objects can be updated based on filters on their own fields or those in their nested objects.
  The ``{}`` expression can be used to update all rows.
- You can return the number of affected rows and the affected objects (with nested objects) in the response.

See the :ref:`update mutation API reference <update_syntax>` for the full specifications.

.. note::

  - At least any one of ``_set``, ``_inc`` operators or the jsonb operators ``_append``, ``_prepend``, ``_delete_key``,
    ``_delete_elem``, ``_delete_at_path`` is required.

  - If a table is not in the ``public`` Postgres schema, the update mutation field will be of the format
    ``update_<schema_name>_<table_name>``.

Update based on an object's fields
----------------------------------
**Example:** Update the ``title``, ``content`` and ``rating`` of the article with a given ``id``:

.. graphiql::
  :view_only:
  :query:
    mutation update_article {
      update_article(
        where: {id: {_eq: 3}},
        _set: {
          title: "lorem ipsum",
          content: "dolor sit amet",
          rating: 2
        }
      ) {
        affected_rows
        returning {
          id
          title
          content
          rating
        }
      }
    }
  :response:
    {
      "data": {
        "update_article": {
          "affected_rows": 1,
          "returning": [
            {
              "id": 3,
              "title": "lorem ipsum",
              "content": "dolor sit amet",
              "rating": 2
            }
          ]
        }
      }
    }

Using variables:

.. graphiql::
  :view_only:
  :query:
    mutation update_article($id: Int, $changes: article_set_input) {
      update_article(
        where: {id: {_eq: $id}},
        _set: $changes
      ) {
        affected_rows
        returning {
          id
          title
          content
          rating
        }
      }
    }
  :response:
    {
      "data": {
        "update_article": {
          "affected_rows": 1,
          "returning": [
            {
              "id": 3,
              "title": "lorem ipsum",
              "content": "dolor sit amet",
              "rating": 2
            }
          ]
        }
      }
    }
  :variables:
    {
      "id": 3,
      "changes": {
        "title": "lorem ipsum",
        "content": "dolor sit amet",
        "rating": 2
      }
    }

OR

.. graphiql::
  :view_only:
  :query:
    mutation update_article($id: Int, $title: String, $content: String, $rating: Int) {
      update_article(
        where: {id: {_eq: $id}},
        _set: {
          title: $title,
          content: $content,
          rating: $rating
        }
      ) {
        affected_rows
        returning {
          id
          title
          content
          rating
        }
      }
    }
  :response:
    {
      "data": {
        "update_article": {
          "affected_rows": 1,
          "returning": [
            {
              "id": 3,
              "title": "lorem ipsum",
              "content": "dolor sit amet",
              "rating": 2
            }
          ]
        }
      }
    }
  :variables:
    {
      "id": 3,
      "title": "lorem ipsum",
      "content": "dolor sit amet",
      "rating": 2
    }

Update based on a nested object's fields
----------------------------------------
**Example:** Reset the ``rating`` of all articles authored by "Sidney":

.. graphiql::
  :view_only:
  :query:
    mutation update_ratings {
      update_article(
        where: {author: {name: {_eq: "Sidney"}}},
        _set: {rating: null}
      ) {
        affected_rows
      }
    }
  :response:
    {
      "data": {
        "update_article": {
          "affected_rows": 3
        }
      }
    }

Update all objects
------------------

You can update all objects in a table using the ``{}`` expression as the ``where`` argument. ``{}`` basically
evaluates to ``true`` for all objects.

**Example:** Reset rating of all articles:

.. graphiql::
  :view_only:
  :query:
    mutation reset_rating {
      update_article (
        where: {}
        _set: { rating: null }
      ) {
        affected_rows
      }
    }
  :response:
    {
      "data": {
        "update_article": {
          "affected_rows": 20
        }
      }
    }

Update a single object
----------------------

You can update a single object in a table using the primary key columns value.
The output type is the nullable table object. The mutation returns the updated
row object or ``null`` if the row does not exist.

**Examples:**

1. Update an article whose ``id`` is ``1``:

.. graphiql::
  :view_only:
  :query:
    mutation update_an_article {
      update_article_by_pk (
        pk_columns: {id: 1}
        _set: { is_published: true }
      ) {
        id
        is_published
      }
    }
  :response:
    {
      "data": {
        "update_article_by_pk": {
          "id": 1,
          "is_published": true
        }
      }
    }

2. Update a non-existent article:

.. graphiql::
  :view_only:
  :query:
    mutation update_an_article {
      update_article_by_pk (
        pk_columns: {id: 1000000}
        _set: { is_published: true }
      ) {
        id
        is_published
      }
    }
  :response:
    {
      "data": {
        "update_article_by_pk": null
      }
    }

.. admonition:: Supported from

   The ``update_<table>_by_pk`` mutation is supported in versions ``v1.2.0``
   and above.

Increment **int** columns
-------------------------
You can increment an ``int`` column with a given value using the ``_inc`` operator.

**Example:** Increment the ``likes`` of an article by 2:

.. graphiql::
  :view_only:
  :query:
    mutation update_likes {
      update_article(
        where: {id: {_eq: 1}},
        _inc: {likes: 2}  # initial value: 1
      ) {
        affected_rows
        returning {
          id
          likes
        }
      }
    }
  :response:
    {
      "data": {
        "update_article": {
          "affected_rows": 1,
          "returning": {
            "id": 1,
            "likes": 3
          }
        }
      }
    }

Update **jsonb** columns
------------------------

The currently available ``jsonb`` operators are:

+----------------------+------------------------+--------------------------------------------------+
| Operator             | Postgres equivalent    | Function                                         |
+======================+========================+==================================================+
| ``_append``          | ``||``                 | append json value to a ``jsonb`` column          |
+----------------------+------------------------+--------------------------------------------------+
| ``_prepend``         | ``||``                 | prepend json value to a ``jsonb`` column         |
+----------------------+------------------------+--------------------------------------------------+
| ``_delete_key``      | ``-``                  | delete top-level key from ``jsonb`` column       |
+----------------------+------------------------+--------------------------------------------------+
| ``_delete_elem``     | ``-``                  | delete array element from ``jsonb`` column       |
+----------------------+------------------------+--------------------------------------------------+
| ``_delete_at_path``  | ``#-``                 | delete element at a path from ``jsonb`` column   |
+----------------------+------------------------+--------------------------------------------------+

.. note::

  You can learn more about Postgres jsonb operators `here <https://www.postgresql.org/docs/current/static/functions-json.html#FUNCTIONS-JSONB-OP-TABLE>`__.

.. contents:: Examples
  :backlinks: none
  :depth: 1
  :local:

Append a json to a jsonb column
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
You can append any ``jsonb`` column with another json value by using the ``_append`` operator.

Since the input is a json value, it should be provided through a variable.

**Example:** Append the json ``{"key1": "value1"}`` to the ``jsonb`` column ``extra_info`` of the ``article`` table:

.. graphiql::
  :view_only:
  :query:
    mutation update_extra_info($value: jsonb) {
      update_article(
        where: {id: {_eq: 1}},
        _append: {extra_info: $value}  # initial value: {"key": "value"}
      ) {
        affected_rows
        returning {
          id
          extra_info
        }
      }
    }
  :response:
    {
      "data": {
        "update_article": {
          "affected_rows": 1,
          "returning": {
            "id": 1,
            "extra_info": {
              "key": "value",
              "key1": "value1"
            }
          }
        }
      }
    }
  :variables:
    {
      "value": { "key1": "value1" }
    }

Prepend a json to a jsonb column
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
You can prepend any ``jsonb`` column with another json value by using the ``_prepend`` operator.

Since the input is a json value, it should be provided through a variable.

**Example:** Prepend the json ``{"key0": "value0"}`` to the ``jsonb`` column ``extra_info`` of the ``article`` table:

.. graphiql::
  :view_only:
  :query:
    mutation update_extra_info($value: jsonb) {
      update_article(
        where: {id: {_eq: 1}},
        _prepend: {extra_info: $value}  # initial value "{"key": "value", "key1": "value1"}"
      ) {
        affected_rows
        returning {
          id
          extra_info
        }
      }
    }
  :response:
    {
      "data": {
        "update_article": {
          "affected_rows": 1,
          "returning": {
            "id": 1,
            "extra_info": {
              "key0": "value0",
              "key": "value",
              "key1": "value1"
            }
          }
        }
      }
    }
  :variables:
    {
      "value": { "key0": "value0" }
    }

Delete a top-level key from a jsonb column
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
You can delete a top-level key of a ``jsonb`` column by using the ``_delete_key`` operator.

The input value should be a ``String``.

**Example:** Delete the key ``key`` in the ``jsonb`` column ``extra_info`` of the ``article`` table:

.. graphiql::
  :view_only:
  :query:
    mutation update_extra_info {
      update_article(
        where: {id: {_eq: 1}},
        _delete_key: {extra_info: "key"}  # initial value "{"key0": "value0, "key": "value", "key1": "value1"}"
      ) {
        affected_rows
        returning {
          id
          extra_info
        }
      }
    }
  :response:
    {
      "data": {
        "update_article": {
          "affected_rows": 1,
          "returning": {
            "id": 1,
            "extra_info": {
              "key0": "value0",
              "key1": "value1"
            }
          }
        }
      }
    }

Delete an element from a jsonb column storing a json array
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
If a ``jsonb`` column is storing a json array, you can delete an element from the array using the ``_delete_elem``
operator.

The input value should be an ``Int``.

**Example:** Delete the element at position 2 in the array value of the ``jsonb`` column ``extra_info``
of the ``article`` table:

.. graphiql::
  :view_only:
  :query:
    mutation update_extra_info {
      update_article(
        where: {id: {_eq: 1}},
        _delete_elem: {extra_info: 2}  # initial value "["a", "b", "c"]"
      ) {
        affected_rows
        returning {
          id
          extra_info
        }
      }
    }
  :response:
    {
      "data": {
        "update_article": {
          "affected_rows": 1,
          "returning": {
            "id": 1,
            "extra_info": ["a", "b"]
          }
        }
      }
    }

Delete an element at a specific path in a jsonb column
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
You can delete a field or element of a ``jsonb`` column at a specified path by using the ``_delete_at_path`` operator.

The input value should be a ``String Array``.

**Example:** Delete element at json path ``name.last`` in the ``jsonb`` column ``extra_info`` of the author table:

.. graphiql::
  :view_only:
  :query:
    mutation update_extra_info {
      update_author(
        where: {id: {_eq: 1}},
        _delete_at_path: {extra_info: ["name", "first"]}  # initial value "{"name": {"first": "first_name", "last": "last_name"}}"
      ) {
        affected_rows
        returning {
          id
          extra_info
        }
      }
    }
  :response:
    {
      "data": {
        "update_author": {
          "affected_rows": 1,
          "returning": {
            "id": 1,
            "extra_info": {
              "name": {
                "last": "last_name"
              }
            }
          }
        }
      }
    }

Replace all nested array objects of an object
---------------------------------------------

In order to replace all existing nested array objects of an object, currently it's required to use two mutations:
one to delete all the existing objects and one to add a list of new nested objects.

**Example:** Replace all articles of an author with a new list:

.. graphiql::
  :view_only:
  :query:
    mutation updateAuthorArticles($author_id: Int!) {
      delete_articles(
        where: {author_id: {_eq: $author_id}}
      ) {
        affected_rows
      }
      insert_articles(
        objects: [
          {
            author_id: $author_id,
            title: "title",
            content: "some content"
          },
          {
            author_id: $author_id,
            title: "another title",
            content: "some other content"
          }
        ]
      ) {
        affected_rows
      }
    }
  :response:
    {
      "data": {
        "delete_article_tags": {
          "affected_rows": 3
        },
        "insert_article_tags": {
          "affected_rows": 2
        }
      }
    }
  :variables:
    {
      "author_id": 21
    }
