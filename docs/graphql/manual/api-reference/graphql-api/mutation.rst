API Reference - Mutation
========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

.. _insert_upsert_syntax:

Insert/Upsert syntax
--------------------

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
     - Name mutation for observability
   * - mutation-field-name
     - true
     - Value
     - name of the auto-generated mutation field. E.g. *insert_author*
   * - input-object
     - true
     - InputObject_
     - name of the auto-generated mutation field
   * - mutation-response
     - true
     - MutationResponse_
     - Object to be returned after mutation succeeds.
   * - on-conflict
     - false
     - ConflictClause_
     - Converts *insert* to *upsert* by handling conflict

**E.g. INSERT**:

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

**E.g. UPSERT**:

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

.. _update_syntax:

Update syntax
-------------

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
     - name of the auto-generated update mutation field. E.g. *update_author*
   * - where-argument
     - true
     - whereArgExp_
     - selection criteria for rows to be updated
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
     - key to be deleted in the value of JSONB columns in the table
   * - delete-elem-argument
     - false
     - deleteElemArgExp_
     - array element to be deleted in the value of JSONB columns in the table
   * - delete-at-path-argument
     - false
     - deleteAtPathArgExp_
     - element at path to be deleted in the value of JSONB columns in the table
   * - mutation-response
     - true
     - MutationResponse_
     - Object to be returned after mutation succeeds.

**E.g. UPDATE**:

.. code-block:: graphql
    
    mutation update_author{
      update_author(
        where: {id: {_eq: 3}},
        _set: {name: "Jane"}
      ) {
        affected_rows
      }
    }

.. _delete_syntax:

Delete syntax
-------------

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
     - name of the auto-generated delete mutation field. E.g. *delete_author*
   * - where-argument
     - true
     - whereArgExp_
     - selection criteria for rows to delete
   * - mutation-response
     - true
     - MutationResponse_
     - Object to be returned after mutation succeeds.

**E.g. DELETE**:

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
    
    For more examples and details of usage, please see :doc:`this <../../mutations/index>`.

Syntax definitions
------------------

.. _MutationResponse:

Mutation Response
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

E.g.:

.. code-block:: graphql

    {
      affected_rows
      returning {
        id
        author_id
      }
    }

.. _InputObject:

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

E.g.:

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

.. _ConflictClause:

**on_conflict** argument
^^^^^^^^^^^^^^^^^^^^^^^^
Conflict clause is used to convert an *insert* query to an *upsert* query. *Upsert* respects the table's *update*
permissions before editing an existing row in case of a conflict. Hence the conflict clause is permitted only if a
table has *update* permissions defined.

.. code-block:: none
    
    on_conflict: {
      constraint: table_constraint!
      update_columns: [table_update_column!]!
    }

E.g.:

.. code-block:: graphql

    on_conflict: {
      constraint: author_name_key
      update_columns: [name]
    }

.. _whereArgExp:

**where** argument
^^^^^^^^^^^^^^^^^^

.. parsed-literal::

    where: BoolExp_

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


OrExp
#####

.. parsed-literal::

    {
      _or: [BoolExp_]
    }

NotExp
######

.. parsed-literal::

    {
      _not: BoolExp_
    }


TrueExp
#######

.. parsed-literal::

    {}

ColumnExp
#########

.. parsed-literal::

    {
      field-name: {Operator_: Value }
    }

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

E.g.

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

E.g.

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
