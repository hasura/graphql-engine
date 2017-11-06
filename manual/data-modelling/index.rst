.. Hasura Platform documentation master file, created by
   sphinx-quickstart on Thu Jun 30 19:38:30 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Data Modelling
==============

.. todo::

   + Data-modelling goals <> Hasura data modelling philosophy
     + Don't duplicate data to make management easy (NoSQL modelling is harder)
     + Add constraints for relationships (to make data consistent)
     + Derive views (to avoid duplicating data)
     + Embed permissions in the data model (to make thinking about access control natural)
     + Handle migrations explicitly in version control (to make it easy to upgrade, test etc)
   + Introduction to modelling with postgres
     + Tables, column types
     + Constraints on the table: primary, unique
     + Permissions
   + Relationships
     + Foreign-keys
     + Hasura relationships
     + examples: many-to-one, many-to-many relationships
   + Derived data (aggregations etc)
     + Introduce idea of deriving data from data that is already available
     + Typically requirements come from the app.
       + Without a view, fetch from multiple sources, process data
       + Eg: tweets with no. likes, no. replies.
   + Examples:
     + ArticleAuthor: hello-world
     + eCommerce: e-commerce
     + todo: todo

.. toctree::
   :hidden:
   :maxdepth: 1
