Part VI: Customise Schema with Views
====================================

In the previous steps, we have created tables, added foreign keys, relationships and applied permissions as well. GraphQL APIs were automatically generated based on the available schema. There are instances where you would like to fetch data from multiple tables based on various conditions and joins or if you have a lot of complex calculations.

**What are views in SQL?**

Views are a virtual table based on the result of a SQL statement. It is just like a real table except for the fact that it consists of data from one or more tables.

**When are views useful?**

To simplify complex SQL statements.
If you have a query that fetches data from multiple tables based on various conditions and joins or if you have a lot of complex calculations or logic with your query, you can create a view and then directly SELECT that data from a view.

**Security**

Let’s say you have multiple tables which has both public and private data. In this case, you can create a view with just the public data from these tables and only expose this view.

Now to cater to above use cases, we can create views in Postgres and expose them via GraphQL APIs for querying.

**Note:** Views are read-only data. Mutations cannot be performed since views are calculated data.

Creating a view
---------------

We already have article, author, like and comment tables. Let’s create a view to get the average rating of all articles written by an author.

Let's `define the view in SQL <https://www.postgresql.org/docs/current/static/sql-createview.html>`_:

.. code-block:: sql

    CREATE VIEW author_average_rating AS
        SELECT author.id, avg(article.rating)
        From author, article
        WHERE author.id = article.author_id
        GROUP BY author.id

Now, let's set this view up on Hasura via the API console:

Navigate to the *Data > SQL* tab in the API console and enter the above SQL statement. 

Ensure the *This is a migration*
and *Track table* boxes are checked so that the query is added as a database migration and the Data microservice is
aware of the created view.

.. image:: ../../../img/complete-tutorial/tutorial-create-view.png

Hit *Run* to create the view on the Postgres database and to track it in the Data microservice (ie: allow querying via
Data APIs).

Now, you can use a ``select`` query to fetch the author's average rating as if ``author_average_rating`` is a table.

.. admonition:: Views are read only!

   Views are like read-only logical tables on the database.
   So that means that Data API requests to select will work, but you cannot
   insert/update/delete items from the view.

Now, let's fetch author's average rating details:

.. code-block:: none

 query fetch_author_average_rating {
   author_average_rating() {
     id
     avg
   }
 }

.. note::
   The above query will work only after adding permissions to the ``author_average_rating`` view.

Relationships to/from views
---------------------------

We have seen how we can get author's average rating using the Data APIs. However, additional information of each author
can be attached to the ``author_average_rating`` view using an object relationship say, ``author``.

All the relationships that we've defined till now use foreign key constraints. However, you cannot define foreign key
constraints on/to views. So, in these cases, we have to manually define a relationship.

Here, we are defining a relationship from a ``author_average_rating`` view to ``author`` table:

.. image:: ../../../img/complete-tutorial/tutorial-add-manual-relationship.png

The above relationship will allow you to fetch author's details when querying the view. But we might want to fetch
author's average rating when querying the author table itself. So we will now create a relationship from ``author``
table to ``author_average_rating`` view:

.. image:: ../../../img/complete-tutorial/tutorial-add-manual-rel-from-table.png

Now, let's fetch author details with their average rating:

.. code-block:: none

 query fetch_author {
   author(order_by: ["+name"]) {
     id
     name
     average_rating {
       avg
     }
   }
 }


Next: Customise Schema with Resolvers
-------------------------------------

Next, let's head to :doc:`write-your-own-resolvers`.
