.. .. meta::
   :description: Manual for accessing postgres directly
   :keywords: hasura, docs, postgres, tunnel

.. _schema_migrations:

Data/schema migrations
======================

Why are migrations required?
----------------------------

Migrations are essentially version control for your database as much as git is for your application code. Migrations keep a record of how the database schema was created and modified over time. It also makes it easier to re-deploy the application or distribute it with collaborators, who can start with a new cluster and apply the migrations to get started quickly.

Lets say you don't use migrations; In this case, there is no record of your database structure bundled into your app. unless you have taken a pg_dump of your schema and stored it somewhere.

Migrations are incremental and you can apply them one by one or go back one by one. They are not tied to your application code. Your application code runs in a separate microservice and you can apply migrations independently.

The core principle is that every change you make to the database must be backward compatible.

How API console creates migrations?
-----------------------------------

Every time you make a modification to the schema via the API Console, it generates migration files in your local project directory.
Go to your project directory where a hasura cluster is setup.

.. code-block:: bash

  cd myproject/migrations && ls

Here's a sample of migration files that have been generated in a hasura project's migrations folder.

.. image:: ../../img/manual/data/migrations.png

How to manually create migrations?
----------------------------------

If you want to directly create migrations manually, you can use the commandline to do the same via ``hasura CLI``

.. code-block:: bash

  cd myproject
  hasura migration generate create_table_likes

The above command generates 4 files inside your project's migrations folder

- timestamp_create_table_likes.up.sql
- timestamp_create_table_likes.up.yaml
- timestamp_create_table_likes.down.sql
- timestamp_create_table_likes.down.yaml

You can now open these files to modify according to your requirement.
Lets say you wanted to manually create the table ``likes`` using commandline. Open the create_table_likes.up.sql and put in the actual CREATE TABLE syntax in that file. It is recommended that you also write the down migration to revert back to original state. In this case it would be ``DROP TABLE likes`` command inside create_table_likes.down.sql

Once you are done making changes to migration files, you can now ``apply`` the changes to the cluster.

.. code-block:: bash

  hasura migration apply

You can also do a git push to the hasura remote (if you have already generated one).

.. code-block:: bash

  git commit -am "Migration changes"
  git push hasura master

Applying migrations to a new cluster
------------------------------------

Lets say you have been added as a collaborator to an existing Hasura project, or you have cloned one of the hasura projects from Hasura Hub, you would like to apply the migrations to your new cluster.

.. code-block:: bash

  cd myproject
  git add .
  git commit -m "Going to apply hasura migrations"
  git push hasura master
