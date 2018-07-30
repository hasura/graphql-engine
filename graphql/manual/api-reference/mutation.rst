.. title:: API Reference - Mutation

API Reference - Mutation
========================

Insert/Upsert syntax
--------------------

.. parsed-literal::
   :class: haskell-pre

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

.. parsed-literal::
   :class: haskell-pre
    
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

.. parsed-literal::
   :class: haskell-pre
    
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
          action: update
        }
      ) {
        affected_rows
      }
    }


Update syntax
-------------

.. parsed-literal::
   :class: haskell-pre

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

.. parsed-literal::
   :class: haskell-pre
    
    mutation update_author{
      update_author(
        where: {id: {_eq: 3}},
        _set: {name: "Jane"}
      ) {
        affected_rows
      }
    }

Delete syntax
-------------

.. parsed-literal::
   :class: haskell-pre

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

.. parsed-literal::
   :class: haskell-pre
    
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
    
    For more examples and details of usage, please see :doc:`this <../mutations/index>`.

Syntax definitions
------------------

.. _InputObject:

Input Object
^^^^^^^^^^^^

.. parsed-literal::
   :class: haskell-pre

    objects: [
      {
        field1: value,
        field2: value,
        ..
      },
      ..
    ]
    # no nested objects

E.g.:

.. parsed-literal::
   :class: haskell-pre
    
    objects: [
      {
        title: "Software is eating the world",
        content: "This week, Hewlett-Packard...",
      }
    ]

.. _MutationResponse:

Mutation Response
^^^^^^^^^^^^^^^^^
.. parsed-literal::
   :class: haskell-pre
   
    {
      affected_rows
      returning {
        response-field1
        response-field2
        ..
      }
    }

E.g.:

.. parsed-literal::
   :class: haskell-pre

    {
      affected_rows
      returning {
        id
        author_id
      }
    }

.. _ConflictClause:

Conflict Clause
^^^^^^^^^^^^^^^
.. parsed-literal::
   :class: haskell-pre
    
    on_conflict: {
      action: [update|ignore],
      [constraint: <unique_constraint_name>]
    }

E.g.:

.. parsed-literal::
   :class: haskell-pre

    on_conflict: {
      action: ignore,
      constraint: author_name_key
    }

.. _whereArgExp:

``where`` argument
^^^^^^^^^^^^^^^^^^

.. parsed-literal::
   :class: haskell-pre

    where: BoolExp_

.. _BoolExp:

BoolExp
*******

.. parsed-literal::
   :class: haskell-pre

    AndExp_ | OrExp_ | NotExp_ | ColumnExp_

AndExp
######

.. parsed-literal::
   :class: haskell-pre

    {
      _and: [BoolExp_]
    }


OrExp
#####

.. parsed-literal::
   :class: haskell-pre

    {
      _or: [BoolExp_]
    }

NotExp
######

.. parsed-literal::
   :class: haskell-pre

    {
      _not: BoolExp_
    }

ColumnExp
#########

.. parsed-literal::
   :class: haskell-pre

    {
      field-name: {Operator_: Value }
    }

Operator
########
Generic operators (all column types except json, jsonb) :

- ``_eq``
- ``_ne``
- ``_in``
- ``_nin``
- ``_gt``
- ``_lt``
- ``_gte``
- ``_lte``

Operators for comparing columns (all column types except json, jsonb):

- ``_ceq``
- ``_cneq``
- ``_cgt``
- ``_clt``
- ``_cgte``
- ``_cnlte``

Text related operators :

- ``_like``
- ``_nlike``
- ``_ilike``
- ``_nilike``
- ``_similar``
- ``_nsimilar``

Checking for ``null`` values :

- ``_is_null`` (takes true/false as values)

.. _setArgExp:

``_set`` argument
^^^^^^^^^^^^^^^^^

.. parsed-literal::
   :class: haskell-pre

    _set: {
      field-name-1 : value,
      field-name-2 : value,
      ..
    }

.. _incArgExp:

``_inc`` argument
^^^^^^^^^^^^^^^^^

.. parsed-literal::
   :class: haskell-pre

   _inc: {
     field-name-1 : int-value,
     field-name-2 : int-value,
     ..
   }

.. _appendArgExp:

``_append`` argument
^^^^^^^^^^^^^^^^^^^^

.. parsed-literal::
   :class: haskell-pre

   _append: {
     field-name-1 : $json-variable-1,
     field-name-2 : $json-variable-1,
     ..
   }

.. code-block:: json

   {
     "json-variable-1": "value",
     "json-variable-2": "value"
   }

.. _prependArgExp:

``_prepend`` argument
^^^^^^^^^^^^^^^^^^^^^

.. parsed-literal::
   :class: haskell-pre

   _prepend: {
     field-name-1 : $json-variable-1,
     field-name-2 : $json-variable-1,
     ..
   }

.. code-block:: json

   {
     "json-variable-1": "value",
     "json-variable-2": "value"
   }

.. _deleteKeyArgExp:

``_delete_key`` argument
^^^^^^^^^^^^^^^^^^^^^^^^

.. parsed-literal::
   :class: haskell-pre

   _delete_key: {
     field-name-1 : "key",
     field-name-2 : "key",
     ..
   }

.. _deleteElemArgExp:

``_delete_elem`` argument
^^^^^^^^^^^^^^^^^^^^^^^^^

.. parsed-literal::
   :class: haskell-pre

   _delete_elem: {
     field-name-1 : int-index,
     field-name-2 : int-index,
     ..
   }

.. _deleteAtPathArgExp:

``_delete_at_path`` argument
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. parsed-literal::
   :class: haskell-pre

   _delete_at_path: {
     field-name-1 : ["path-array"],
     field-name-2 : ["path-array"],
     ..
   }
