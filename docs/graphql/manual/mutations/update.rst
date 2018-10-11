Update mutation
===============

Objects can be updated based on filters on their own fields or those in their nested objects. 

.. note::

   ``where`` argument is compulsory to filter rows.
   At least any one of ``_set``, ``_inc``,  ``_append``, ``_prepend``, ``_delete_key``, ``_delete_elem`` and
   ``_delete_at_path`` is expected.

Update based on a filter on an object's fields
----------------------------------------------
Update the ``name`` of the author with a given ``id``:

.. graphiql::
  :view_only:
  :query:
    mutation update_author {
      update_author(
        where: {id: {_eq: 3}},
        _set: {name: "Jane"}
      ) {
        affected_rows
      }
    }
  :response:
    {
      "data": {
        "update_author": {
          "affected_rows": 1
        }
      }
    }

Update based on a filter on a nested object's fields
----------------------------------------------------
Update the ``rating`` of all articles that belong to an author:

.. graphiql::
  :view_only:
  :query:
    mutation update_ratings {
      update_article(
        where: {author: {name: {_eq: "Sidney"}}},
        _set: {rating: 1}
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

Update using **_inc** operator
------------------------------
Update any ``int`` column by incrementing it with given value.

Increment the ``likes`` of an article:

.. graphiql::
  :view_only:
  :query:
    mutation update_likes {
      update_article(
        where: {id: {_eq: 1}},
        _inc: {likes: 1}
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
            "likes": 2
          }
        }
      }
    }

Using jsonb operators
---------------------

.. note::

   Available jsonb operators are ``_append`` (``||``), ``_prepend`` (``||``), ``_delete_key`` (``-``), ``_delete_elem`` (``-``) and ``_delete_at_path`` (``#-``).
   You can learn more about jsonb operators `here <https://www.postgresql.org/docs/current/static/functions-json.html#FUNCTIONS-JSONB-OP-TABLE>`__


Update using **_append** operator
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Update any ``jsonb`` column by appending it with given value. Since it is a json value, it should
be provided through a variable.

Append the ``extra_info`` of an article:

.. graphiql::
  :view_only:
  :query:
    mutation update_extra_info($value: jsonb) {
      update_article(
        where: {id: {_eq: 1}},
        _append: {extra_info: $value}
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
              "key": "value"
            }
          }
        }
      }
    }

variables for above query:

.. code-block:: json

   {
     "value": { "key": "value" }
   }

Update using **_prepend** operator
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Update any ``jsonb`` column by prepending it with given value. Since it is a json value, it should
provided through a variable.

Prepend the ``extra_info`` of an article:

.. graphiql::
  :view_only:
  :query:
    mutation update_extra_info($value: jsonb) {
      update_article(
        where: {id: {_eq: 1}},
        _prepend: {extra_info: $value}
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
              "key": "value"
            }
          }
        }
      }
    }

variables for above query:

.. code-block:: json

   {
     "value": { "key0": "value0" }
   }

Update using **_delete_key** operator
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Update any ``jsonb`` column by deleting a top level key. Input value should be a ``String``.

Delete the key ``key3`` in the ``extra_info`` of an article:

.. graphiql::
  :view_only:
  :query:
    mutation update_extra_info {
      update_article(
        where: {id: {_eq: 1}},
        _delete_key: {extra_info: "key3"}
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
              "key1": "value1",
              "key2": "value2"
            }
          }
        }
      }
    }

Update using **_delete_elem** operator
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Update any ``jsonb`` column by deleting an array element with given index value. Input value should be a ``Int``.

Delete the element at ``2`` in ``jsonb`` array ``["a", "b", "c"]`` of column ``extra_info`` of an article:

.. graphiql::
  :view_only:
  :query:
    mutation update_extra_info {
      update_article(
        where: {id: {_eq: 1}},
        _delete_elem: {extra_info: 2}
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

Update using **_delete_at_path** operator
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Update any ``jsonb`` column by deleting field or element with specified path. Input value should be a ``String Array``.

Delete element at json path ``name.last`` in ``extra_info`` column of author table:

.. graphiql::
  :view_only:
  :query:
    mutation update_extra_info {
      update_author(
        where: {id: {_eq: 1}},
        _delete_at_path: {extra_info: ["name", "first"]}
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

