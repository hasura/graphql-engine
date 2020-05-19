.. meta::
   :description: Hasura action examples
   :keywords: hasura, docs, actions, examples

.. _action_examples:

Action examples
===============

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

This page provides reference examples of typical action use cases.

Relationships
-------------

Array relationship from an action
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Let's say we have the following two tables in our schema:

.. code-block:: sql

    author (id int, name text, is_active boolean, rating int)

    article (id int, title text, content text, author_id int)

Now we have an action ``updateAuthor`` that looks as follows:

.. thumbnail:: /img/graphql/manual/actions/update-author-action-definition.png
       :alt: Update author action
       :width: 65%

We can now add an array relationship from the ``updateAuthor`` action to the ``article`` table in our schema.
We call it ``updatedAuthorArticles``.

.. thumbnail:: /img/graphql/manual/actions/action-array-relationship.png
       :alt: Array relationship from action
       :width: 65%

It's now possible to return articles from the ``updateAuthor`` action through the newly created relationship:

.. graphiql::
  :view_only:
  :query:
    mutation {
      updateAuthor(author: 
        { 
          id: 442, 
          name: "Joanne K. Rowling", 
          is_active: true, 
          rating: 10
        }
      ) {
          id
          updatedAuthorArticles {
            title
          }
        }
    }
  :response:
    {
      "data": {
        "updateAuthor": {
          "id": 442,
          "updatedAuthorArticles": [
            {
                "title": "Harry Potter and the Philosopher's Stone"
            },
            {
                "title": "Harry Potter and the Chamber of Secrets"
            }
          ]
        }
      }
    }

.. note::

    Only non-list type scalars (e.g. ``Int``, ``String``) can be mapped through relationships.
