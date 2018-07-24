Mutations
=========
GraphQL mutations are used to modify server-side data i.e. write, update or delete data. As with queries, mutation
fields are auto-generated based on the Postgres schema. Here’s a sample mutation field from our reference
author/article schema:

.. code-block:: none

    insert_article(
      objects: [article_input!] 
      on_conflict: conflict_clause
      ): article_mutation_response

As you can see from the schema, you can:

#. Pass multiple objects to the mutation.
#. Return objects, from the affected rows, in the response.

.. note::

    As of now you cannot return a nested object in the mutation response *(we are working on this feature)*.

Let's use this reference author/article schema to look at different types of mutations.

.. toctree::
  :maxdepth: 1

  Insert <insert>
  Upsert <upsert>
  Update <update>
  Delete <delete>
  multiple-mutations











