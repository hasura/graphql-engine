Postgres full text search
================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

In this guide we will walk through how to implement a full text search in
Postgres by creating a custom SQL function and exposing it on the GraphQL API.

What is full text search?
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Imagine we have a set of text documents stored in a database. These
documents could be an abstract of certain text article or the entire
article itself and we want to find out if certain words are present in
them or not.

The way FTS is implemented in Postgres is by getting a semantic vector
for all of the words contained in the document. So, when we search for
words like "jump", we will match all instances of the word like
"jumping", "jumped". So in essence we will be searching just the vector
and not the document which is fast.

Functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^

PostgreSQL has two functions that do exactly what we intend to do:

-  ``to_tsvector``: This will create a list of tokens.
-  ``to_tsquery``: To query the vector for occurences of certain words.

**to_tsvector()**

Example: To create a vector for the sentence "The quick brown fox jumped
over the lazy dog"

.. code:: sql

   SELECT to_tsvector('The quick brown fox jumped over the lazy dog');

returns:

.. code:: bash
                         to_tsvector
   -------------------------------------------------------
    'brown':3 'dog':9 'fox':4 'jump':5 'lazi':8 'quick':2
Every word is normalized into a lexeme.

.. note::
   Sometimes the word won't be normalized depending on the
   localization settings of your postgres installation. Default language
   is English but this can be changed by passing the language of you are
   working with as an argument.

.. code:: sql

   SELECT to_tsvector('French', 'Le rapide renard brun sauta par dessus le chien paresseux');

It would return a vector normalized according to the French language.

.. code:: bash
                         to_tsvector
   -------------------------------------------------------------------------
    'brun':4 'chien':9 'dessus':7 'paress':10 'rapid':2 'renard':3 'saut':5
**to_tsquery()**

This function will accept a list of words that will be checked against
the normalized vector we created with ``to_tsvector()``

Example:

.. code:: sql

   SELECT to_tsvector('The quick brown fox jumped over the lazy dog') @@ to_tsquery('fox');

.. code:: bash
    ?column?
   ----------
    t
The ``@@`` operator is used to check if the ``tsquery`` matches
``tsvector``.

**tsquery** also provides a set of operators such as:

-  AND operator (&)

.. code:: sql

   SELECT to_tsvector('The quick brown fox jumped over the lazy dog') @@ to_tsquery('fox & dog');

-  OR operator (|)

.. code:: sql

   SELECT to_tsvector('The quick brown fox jumped over the lazy dog') @@ to_tsquery('fox | clown');

-  NEGATION operator (!)

.. code:: sql

   SELECT to_tsvector('The quick brown fox jumped over the lazy dog') @@ to_tsquery('!clown');

Creating and Storing the tsvector Data Type
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Let’s say we have two simple tables for an article/author schema, if you
don't have the tables have a look at this `guide`_.

We will store the vectors in the same table instead of vectorizing the documents on the fly because the execution time is faster.

.. code:: sql

   ALTER TABLE article
   ADD COLUMN document tsvector;
   update article
   set document = to_tsvector(title || ' ' || content);

We can take this up another notch up by adding index to the pre computed tsvector column.

.. code:: sql

   ALTER TABLE article
   ADD COLUMN document_with_idx tsvector;
   update artile
   set document_with_idx = to_tsvector(title || ' ' || content);
   CREATE INDEX document_idx
   ON card
   USING GIN (document_with_idx);

And it can be queried like this

.. code:: sql

   SELECT name, artist, text from card
   WHERE document_with_idx @@ to_tsquery('hasura');

Creating SQL functions
^^^^^^^^^^^^^^^^^^^^^^

We can now create a custom SQL function out of the above implementation and expose it over the GraphQL API in the Hasura console.

.. code:: sql

   CREATE FUNCTION search_articles(search text)
   RETURNS SETOF article AS $$
   SELECT *
   FROM article
   WHERE document_with_idx @@ to_tsquery('' || search || '')
   $$ LANGUAGE sql STABLE;


This function filters rows from the ``article`` table based on the input text argument, ``search``

* Head to the ``Data -> SQL`` section of the Hasura console
* Enter the above function
* Select the ``track this`` checkbox to expose the new function over the GraphQL API
* Hit the ``Run`` button

You can use the custom function as follows:

.. graphiql::
  :view_only:
  :query:
    query {
      search_articles(
        args: {search: "hasura"}
      ){
        id
        title
        content
      }
    }
  :response:
    {
      "data": {
        "search_articles": [
          {
            "id": 1,
            "title": "first post by hasura",
            "content": "some content for post"
          },
          {
            "id": 2,
            "title": "second post by hasura",
            "content": "some other content for post"
          }
        ]
      }
    }


.. _guide: https://docs.hasura.io/1.0/graphql/manual/schema/basics.html
