.. meta::
  :description: Hasura Cloud for YugabyteDB
  :keywords: hasura, docs, databases, yugabytedb, hasura-cloud

.. _database_yugabytedb_cloud:

Get Started with Hasura Cloud & YugabyteDB
==================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Try it out
----------

Prerequisites
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* A YugabyteDB cluster.
   - You can `sign up for Yugabyte Cloud <https://cloud.yugabyte.com/register/>`__ and get a free YugabyteDB cluster.
   - You can `install YugabyteDB <https://docs.yugabyte.com/latest/quick-start/install/>`__ on MacOS, Linux, Docker, or Kubernetes.

Step 1: Create an account on Hasura Cloud and create a new Hasura Project
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Navigate to `cloud.hasura.io
<https://cloud.hasura.io/signup/?pg=docs&plcmt=body&cta=navigate-to-cloud-hasura-io&tech=default>`__, and
create a new Hasura Cloud account.

Once you create a project on Hasura Cloud, hit the "Launch Console" button
to open the Hasura Console for your project.

.. thumbnail:: /img/graphql/cloud/getting-started/create-project.png
   :alt: Connect new or existing database
   :width: 600px

Step 2: Add your YugabyteDB database as a source to Hasura
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In the ``Data > Data Manager > Connect Existing Database`` section on the console, 
select ``PostgreSQL`` from the ``Data Source Driver`` dropdown and add the connection string
directly or through an environment variable. As YugabyteDB uses same protocol as 
PostgreSQL, the connection string will start with ``postgres://``, i.e, there is no
difference between YugabyteDB’s connection strings and PostgreSQL’s connection strings.

.. thumbnail:: /img/graphql/core/databases/yugabytedb/1-add-source.png
   :alt: Add source
   :width: 600px

Once you add the database, you'll see your database pop up on the sidebar.

Step 3: Track existing tables or create new tables
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If you have existing tables, head to the database page by clicking on the database name on 
the sidebar. You should see a list of tables.

.. thumbnail:: /img/graphql/core/databases/yugabytedb/2-manage-mydb.png
   :alt: Manage my-db
   :width: 1000px

Track tables selectively or all of them so that Hasura can inspect the tables and create the 
corresponding GraphQL schema.

If you have foreign keys, you'll also see suggested relationships. Again, you can choose to 
track them selectively or all at once.

If you don't have existing tables, go ahead and add new tables and data and try out some 
queries, just like with a regular PostgreSQL database.

Step 4: Try out a GraphQL query
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Head to the ``GraphiQL`` tab in the console and try running a GraphQL query! Use the explorer 
sidebar on GraphQL to get help in creating a GraphQL query.

.. thumbnail:: /img/graphql/core/databases/yugabytedb/3-make-graphql-query.png
   :alt: Make GraphQL query
   :width: 1000px
