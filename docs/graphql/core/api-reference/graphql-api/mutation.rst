.. meta::
   :description: Hasura GraphQL API mutations API reference
   :keywords: hasura, docs, GraphQL API, API reference, mutation

.. _graphql_api_mutation:

API Reference - Mutation
========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:
 
.. _insert_upsert_syntax:

**insert** (upsert) syntax
--------------------------

.. code-block:: none

    mutation [<mutation-name>] {
      <mutation-field-name> (
        [<input-object>!]
        [conflict-clause]
      )
      [mutation-response!]
    }

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - mutation-name
     - false
     - Value
     - Name of mutation for observability
   * - mutation-field-name
     - true
     - Value
     - Name of the auto-generated mutation field, e.g. *insert_author*
   * - input-object
     - true
     - InputObjects_
     - Name of the auto-generated mutation field
   * - mutation-response
     - true
     - MutationResponse_
     - Object to be returned after mutation succeeds
   * - on-conflict
     - false
     - ConflictClause_
     - Converts *insert* to *upsert* by handling conflict

**Example: Insert**

.. code-block:: graphql

    mutation insert_article {
      insert_article(
        objects: [
          {
            title: "Software is gluttonous",
            content: "Something happened in HP",
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

**Example: Upsert**

.. code-block:: graphql

    mutation upsert_author {
      insert_author (
        objects: [
          {
            name: "John",
            id:12
          }
        ],
        on_conflict: {
          constraint: author_name_key,
          update_columns: [name, id]
        }
      ) {
        affected_rows
      }
    }

.. _insert_upsert_one_syntax:

**insert_one** syntax
---------------------

.. code-block:: none

    mutation [<mutation-name>] {
      <mutation-field-name> (
        [<input-object>!]
        [conflict-clause]
      )
      [mutation-response!]
    }

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - mutation-name
     - false
     - Value
     - Name of mutation for observability
   * - mutation-field-name
     - true
     - Value
     - Name of the auto-generated mutation field, e.g. *insert_author_one*
   * - input-object
     - true
     - InputObject_
     - Name of the auto-generated mutation field
   * - mutation-response
     - true
     - :ref:`SimpleObject`
     - Object to be returned after mutation succeeds
   * - on-conflict
     - false
     - ConflictClause_
     - Converts *insert* to *upsert* by handling conflict

**Example: Insert One**

.. code-block:: graphql

    mutation insert_article_one {
      insert_article_one(
        object: {
          title: "Software is gluttonous",
          content: "Something happened in HP",
          author_id: 3
        }
      ) {
        id
        title
      }
    }

.. _update_by_pk_syntax:

**update_by_pk** syntax
-----------------------

.. code-block:: none

    mutation [<mutation-name>] {
      <mutation-field-name> (
        [pk-columns-argument!],
        [set-argument!]
      )
      <object-fields>
    }

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - mutation-name
     - false
     - Value
     - Name of mutation for observability
   * - mutation-field-name
     - true
     - Value
     - Name of the auto-generated update mutation field, e.g. *update_author_by_pk*
   * - pk-columns-argument
     - true
     - pkColumnsArgExp_
     - Primary key(s) for row(s) to be updated
   * - set-argument
     - false
     - setArgExp_
     - Data to be updated in the table
   * - inc-argument
     - false
     - incArgExp_
     - Integer value to be incremented to Int columns in the table
   * - append-argument
     - false
     - appendArgExp_
     - JSON value to be appended to JSONB columns in the table
   * - prepend-argument
     - false
     - prependArgExp_
     - JSON value to be prepended to JSONB columns in the table
   * - delete-key-argument
     - false
     - deleteKeyArgExp_
     - Key to be deleted in the value of JSONB columns in the table
   * - delete-elem-argument
     - false
     - deleteElemArgExp_
     - Array element to be deleted in the value of JSONB columns in the table
   * - delete-at-path-argument
     - false
     - deleteAtPathArgExp_
     - Element at path to be deleted in the value of JSONB columns in the table

**Example: Update by PK**

.. code-block:: graphql

    mutation update_articles {
      update_article_by_pk (
        pk_columns: {
          id: 1
        }
        _set: { is_published: true }
      ) {
        id
        title
      }
    }

.. _update_syntax:

**update** syntax
-----------------

.. code-block:: none

    mutation [<mutation-name>] {
      <mutation-field-name> (
        [where-argument!],
        [set-argument!]
      )
      [mutation-response!]
    }

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - mutation-name
     - false
     - Value
     - Name of mutation for observability
   * - mutation-field-name
     - true
     - Value
     - Name of the auto-generated update mutation field, e.g. *update_author*
   * - where-argument
     - true
     - whereArgExp_
     - Selection criteria for rows to be updated
   * - set-argument
     - false
     - setArgExp_
     - Data to be updated in the table
   * - inc-argument
     - false
     - incArgExp_
     - Integer value to be incremented to Int columns in the table
   * - append-argument
     - false
     - appendArgExp_
     - JSON value to be appended to JSONB columns in the table
   * - prepend-argument
     - false
     - prependArgExp_
     - JSON value to be prepended to JSONB columns in the table
   * - delete-key-argument
     - false
     - deleteKeyArgExp_
     - Key to be deleted in the value of JSONB columns in the table
   * - delete-elem-argument
     - false
     - deleteElemArgExp_
     - Array element to be deleted in the value of JSONB columns in the table
   * - delete-at-path-argument
     - false
     - deleteAtPathArgExp_
     - Element at path to be deleted in the value of JSONB columns in the table
   * - mutation-response
     - true
     - MutationResponse_
     - Object to be returned after mutation succeeds

**Example: Update**

.. code-block:: graphql

    mutation update_author{
      update_author(
        where: {id: {_eq: 3}},
        _set: {name: "Jane"}
      ) {
        affected_rows
      }
    }

.. _delete_by_pk_syntax:

**delete_by_pk** syntax
-----------------------

.. code-block:: none

    mutation [<mutation-name>] {
      <mutation-field-name> (
        column1: value1
        column2: value2
      )
      <object-fields>
    }

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - mutation-name
     - false
     - Value
     - Name of mutation for observability
   * - mutation-field-name
     - true
     - Value
     - Name of the auto-generated delete mutation field, e.g. *delete_author_by_pk*

**Example: Delete by PK**

.. code-block:: graphql

    mutation delete_articles {
      delete_article_by_pk (
        id: 1
      ) {
        id
        title
      }
    }

.. _delete_syntax:

**delete** syntax
-----------------

.. code-block:: none

    mutation [<mutation-name>] {
      <mutation-field-name> (
        [where-argument!]
      )
      [mutation-response!]
    }

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - mutation-name
     - false
     - Value
     - Name of mutation for observability
   * - mutation-field-name
     - true
     - Value
     - Name of the auto-generated delete mutation field, e.g. *delete_author*
   * - where-argument
     - true
     - whereArgExp_
     - Selection criteria for rows to delete
   * - mutation-response
     - true
     - MutationResponse_
     - Object to be returned after mutation succeeds

**Example: Delete**

.. code-block:: graphql

    mutation delete_articles {
      delete_article(
        where: {author: {id: {_eq: 7}}}
      ) {
        affected_rows
        returning {
          id
        }
      }
    }


.. note::

    For more examples and details of usage, please see :ref:`this <mutations>`.

Syntax definitions
------------------

.. _MutationResponse:

Mutation response
^^^^^^^^^^^^^^^^^
.. code-block:: none

    {
      affected_rows
      returning {
        response-field1
        response-field2
        ..
      }
    }

**Example**

.. code-block:: graphql

    {
      affected_rows
      returning {
        id
        author_id
      }
    }

.. _InputObjects:

**objects** argument
^^^^^^^^^^^^^^^^^^^^

.. code-block:: none

    objects: [
      {
        field1: value,
        field2: value,
        <object-rel-name>: {
          data: <Input-Object>!,
          on_conflict: <Conflict-Clause>
        },
        <array-rel-name>: {
          data: [<Input-Object>!]!,
          on_conflict: <Conflict-Clause>
        }
        ..
      },
      ..
    ]
    # no nested objects

**Example**

.. code-block:: graphql

    objects: [
      {
        title: "Software is eating the world",
        content: "This week, Hewlett-Packard...",
        author: {
          data: {
            id: 1,
            name: "Sydney"
          }
        }
      }
    ]

.. _InputObject:

**object** argument
^^^^^^^^^^^^^^^^^^^

.. code-block:: none

    object: {
      field1: value,
      field2: value,
      <object-rel-name>: {
        data: <Input-Object>!,
        on_conflict: <Conflict-Clause>
      },
      <array-rel-name>: {
        data: [<Input-Object>!]!,
        on_conflict: <Conflict-Clause>
      }
      ..
    }


**Example**

.. code-block:: graphql

    object: {
      title: "Software is eating the world",
      content: "This week, Hewlett-Packard...",
      author: {
        data: {
          id: 1,
          name: "Sydney"
        }
      }
    }

.. _ConflictClause:

**on_conflict** argument
^^^^^^^^^^^^^^^^^^^^^^^^
The conflict clause is used to convert an *insert* mutation to an *upsert* mutation. *Upsert* respects the table's *update*
permissions before editing an existing row in case of a conflict. Hence the conflict clause is permitted only if a
table has *update* permissions defined.

.. code-block:: none

    on_conflict: {
      constraint: table_constraint!
      update_columns: [table_update_column!]!
      where: table_bool_exp
    }

**Example**

.. code-block:: graphql

    on_conflict: {
      constraint: author_name_key
      update_columns: [name]
      where: {id: {_lt: 1}}
    }

.. _pkColumnsArgExp:

**pk_columns** argument
^^^^^^^^^^^^^^^^^^^^^^^

The ``pk_columns`` argument is used to identify an object by its primary key columns in *update* mutations. 

.. code-block:: none

    pk_columns: {
      column-1: value-1
      column-2: value-2
    }

**Example**

.. code-block:: graphql

    pk_columns: {
      id: 1
      name: "Harry"
    }

.. _whereArgExp:

**where** argument
^^^^^^^^^^^^^^^^^^

.. parsed-literal::

    where: BoolExp_

**Example**

.. code-block:: graphql

  where: {
    rating: {_eq: 5}
  }

BoolExp
*******

.. parsed-literal::

    AndExp_ | OrExp_ | NotExp_ | TrueExp_ | ColumnExp_

AndExp
######

.. parsed-literal::

    {
      _and: [BoolExp_]
    }


**Example**

.. code-block:: graphql

  _and: [
    {rating: {_gt: 5}}, 
    {updated_at: {_gt: "2019-01-01"}}
  ]

OrExp
#####

.. parsed-literal::

    {
      _or: [BoolExp_]
    }

**Example**

.. code-block:: graphql

  _or: [
    {rating: {_is_null: true}}, 
    {rating: {_lt: 4}}
  ]

NotExp
######

.. parsed-literal::

    {
      _not: BoolExp_
    }

**Example**

.. code-block:: graphql

  _not: {
    title: {_eq: ""}
  }

TrueExp
#######

.. parsed-literal::

    {}

**Example**

.. code-block:: graphql

  author(where: {articles: {}})

.. note::

  ``{}`` evaluates to true whenever an object exists (even if it's ``null``).

ColumnExp
#########

.. parsed-literal::

    {
      field-name: {Operator_: Value }
    }

**Example**

.. code-block:: graphql

  {rating: {_eq: 5}}

Operator
########

**Generic operators (all column types except json, jsonb):**

- ``_eq``
- ``_ne``
- ``_in``
- ``_nin``
- ``_gt``
- ``_lt``
- ``_gte``
- ``_lte``

**Operators for comparing columns (all column types except json, jsonb)**:

- ``_ceq``
- ``_cneq``
- ``_cgt``
- ``_clt``
- ``_cgte``
- ``_cnlte``

**Text related operators:**

- ``_like``
- ``_nlike``
- ``_ilike``
- ``_nilike``
- ``_similar``
- ``_nsimilar``

**Checking for NULL values:**

- ``_is_null`` (takes true/false as values)

.. _setArgExp:

**_set** argument
^^^^^^^^^^^^^^^^^

.. code-block:: none

    _set: {
      field-name-1 : value,
      field-name-2 : value,
      ..
    }

.. _incArgExp:

**_inc** argument
^^^^^^^^^^^^^^^^^

.. code-block:: none

   _inc: {
     field-name-1 : int-value,
     field-name-2 : int-value,
     ..
   }

.. _appendArgExp:

**_append** argument
^^^^^^^^^^^^^^^^^^^^

.. code-block:: none

   _append: {
     field-name-1 : $json-variable-1,
     field-name-2 : $json-variable-1,
     ..
   }

**Example**

.. code-block:: json

   {
     "json-variable-1": "value",
     "json-variable-2": "value"
   }

.. _prependArgExp:

**_prepend** argument
^^^^^^^^^^^^^^^^^^^^^

.. code-block:: none

   _prepend: {
     field-name-1 : $json-variable-1,
     field-name-2 : $json-variable-1,
     ..
   }

**Example**

.. code-block:: json

   {
     "json-variable-1": "value",
     "json-variable-2": "value"
   }

.. _deleteKeyArgExp:

**_delete_key** argument
^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: none

   _delete_key: {
     field-name-1 : "key",
     field-name-2 : "key",
     ..
   }

.. _deleteElemArgExp:

**_delete_elem** argument
^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: none

   _delete_elem: {
     field-name-1 : int-index,
     field-name-2 : int-index,
     ..
   }

.. _deleteAtPathArgExp:

**_delete_at_path** argument
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: none

   _delete_at_path: {
     field-name-1 : ["path-array"],
     field-name-2 : ["path-array"],
     ..
   }
