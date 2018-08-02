Common GraphQL Engine Errors
============================

We will look at the common issues which occur due to manipulation of the database schema via psql or using any other postgresql client. Hasura GraphQL engine reads the state (schema) from the database and provides a GraphQL endpoint. Once the GraphQL engine successfully starts, any modifications to the schema should be done via the console to avoid corrupting the GraphQL engine state. 

The following are the list of error messages returned by the GraphQL Engine when it encounters an inconsistent state.

.. note::
  This section requires a basic knowledge of how GraphQL engine works. Kindly checkout this url for more info.

Error: no such table/view exists in postgres
--------------------------------------------

The error is due to ``table/view`` tracked by the Hasura GraphQL engine is deleted or is not available in the database.

Example
^^^^^^^

- Create a table called `author` from console.
- Open `psql` or `adminer` or any other postgresql client and delete `author` table.
- Restart GraphQL engine.

Solution
^^^^^^^^

From the above example, it is clear that GraphQL engine expects the to be available in the database to function properly. There are multiple ways to solve this problem. One of the easiest way is to delete from the row from ``hdb_table`` table under ``hdb_catalog`` schema

Error: no foreign constraint exists on the given column
-------------------------------------------------------

Hasura GraphQL Engine validates all the relationships before it starts serving. When it encounters a relationship defined from table ``A -> B`` it looks for a foreign key constraint in table ``A`` and when it couldn't find, it throws the above error.

Solution
^^^^^^^^

Connect to psql and switch to ``hdb_catalog`` schema, look for a table called ``hdb_relationship``. Find the entry for the above relationship and delete it. 

Restart GraphQL engine to verify.

Error: field already exists
---------------------------

When a relationship is created using Hasura GraphQL Engine. It creates a placeholder key with the relationship name which is extremely useful while fetching from multiple tables in a single request. 

Example
^^^^^^^

Lets say we have a table called ``article`` as follows

article

-------

id
text
author_id

author -> author::id ( Author is an object relationship to author table using the foreign key constraint author_id -> id )

When this table is described using psql. ``author`` field will not be available as part of the list of fields returned by the describe command as it is something added by Hasura GraphQL engine. Now if a new column is created with the same name via psql, Hasura GraphQL engine will throw the above error when restarted.

Solution
^^^^^^^^

1. Remove the problamatic column from the table or remove the relationship from the ``hdb_relationship`` table under ``hdb_catalog``
