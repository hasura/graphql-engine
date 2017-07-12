:orphan:

.. meta::
   :description: A tutorial on modeling users or adding user related data to your tables in the application database in addition to the auth service's database.
   :keywords: hasura, docs, tutorials, user-modeling, user data
   :content-tags: data-modeling, user-data

==========================================
Modeling Users in the application database
==========================================

.. image:: ../img/inter-service-communication-block-diagram.png

The auth service handles user management related use-cases for your application and store its data in a different, isolated database in the Postgres instance. However, most applications may need to either store custom fields that are not supported by the service or hold a copy of a subset of the user data in the application database i.e. ``hasuradb`` (*or perhaps both*). In this tutorial, we will take a look at how to do this.

