Part VI: Data modelling for the blog app
========================================

Our basic data requirements are as follows:

* Storing the profile data of each user
* Storing the article data and the author of each article
* Storing the likes given by users to articles
* Storing the comments written by users on articles

Create tables
-------------

As you would normally do with any relational database, data is modelled as tables.

+----------------------------------------+----------------------------------------+
|Table                                   |Columns                                 |
+========================================+========================================+
|author                                  |id, name                                |
+----------------------------------------+----------------------------------------+
|article                                 |id, title, content, rating, author_id   |
+----------------------------------------+----------------------------------------+
|like                                    |user_id, article_id                     |
+----------------------------------------+----------------------------------------+
|comment                                 |id, user_id, article_id, comment        |
+----------------------------------------+----------------------------------------+

**Note:** If you are using the ``hello-world`` quickstart, tables ``author`` and ``article`` have already been created
via migrations

You can create tables via the ``API console``. Head to *Data -> Add Table*.

Create Table ``like``:

.. image:: ../../img/complete-tutorial/tutorial-create-table-like.png

Create Table ``comment``:

.. image:: ../../img/complete-tutorial/tutorial-create-table-comment.png

Create the ``author`` and ``article`` tables similarly if not already created.

Add foreign key constraints:
----------------------------

After creating the tables, the following foreign key constraints need to be set up to complete our data modelling:

* ``article::author_id -> author::id``
* ``like::article_id -> article::id``
* ``comment::article_id -> article::id``

To add foreign key constraints, click on the table, go to the *Modify* tab and edit the columns for which you wish
to add foreign keys.
	    
Adding foreign key constraint to ``article_id`` of ``like`` table:

.. image:: ../../img/complete-tutorial/tutorial-modify-table-like.png

Similarly, add the other foreign key constraints.

	    
Next: Explore the Data APIs
---------------------------

Next, head to :doc:`explore-data-apis`.
