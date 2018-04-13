Part VI: Data modelling for the blog app
========================================

Our basic data requirements are as follows:

* Storing the profile data of each user
* Storing the article data and the author of each article
* Storing the likes given by users to articles
* Storing the comments written by users on articles

..
   `Basic data modelling <https://youtu.be/EdBjQ3zRBWg>`_
   The video shows you how to create data models using the project console.
   The document below however, is an API driven approach to creating data models.
   Don't skip reading this guide if you need a solid developer intro to modelling,
   especially if you want to start versioning your schema!

Creating tables
---------------

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

**Note:** If you are using ``hello-world`` quickstart, tables ``author`` and ``article`` has been already imported via migrations 

You can Create tables and model foreign key constraints via the API console like this:

Create Table ``like``

.. image:: ../../img/complete-tutorial/tutorial-create-table-like.png

Create Table ``comment``

.. image:: ../../img/complete-tutorial/tutorial-create-table-comment.png


To add foreign key constraints, after creating the tables go to *Modify* and edit the columns for which you wish to add foreign keys.
	    
Adding foreign key constraints to ``article_id`` of ``like`` table.

.. image:: ../../img/complete-tutorial/tutorial-modify-table-like.png

Now similarly create the other tables and add foreign key constraints.

	    
Next: Explore the Data APIs
---------------------------

Next, head to :doc:`explore-data-apis`.
