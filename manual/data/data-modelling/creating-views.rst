============
Derive views
============

Why views?
----------

- Abstraction

The relational data queries can be quite complex sometimes, leading to many joins or calculations. Instead the same complex query can be written as a view and the application can make a simpler query to the view instead of the table with joins.
Column names can be aliased to make it suitable for the application needs, abstracting the original table.

- Permissions

Views are also used to apply granular permissions for underlying tables. Views can be made accessible to users while the underlying tables are not directly accessible. This allows the DBA to give users only the data they need, while protecting other data in the same table. Hasura lets you define permissions for views.

- Legacy Code & Database Refactoring

Views help place logic in a single location, so that you do not have to change it all over the code base. Suppose you make a modification to your underlying schema, your application can still keep using the same query, provided the view is also aligned to the data structure. Eventually though, you might have to make the changes in your code base if it gets too complex.

Creating views
--------------

Consider a sample schema with an ``article`` and an ``author`` table. Let us create a view that shows average rating of authors. The SQL looks like:

#. Go to the API console and open the ``Data`` tab.

#. Choose the ``SQL`` section on the left panel.

#. Following is the SQL snippet for our desired view. Copy it in the input box.

   .. code-block:: sql

        CREATE VIEW author_average_rating AS
          SELECT author.id, avg(article.rating)
          From author, article
          WHERE author.id = article.author_id
          GROUP BY author.id

#. Check the ``This is a migration`` checkbox so that a :doc:`migration <../data-migration>` is created in the ``/migrations`` directory.

#. Hit ``Run``.
