.. Hasura Platform documentation master file, created by
   sphinx-quickstart on Thu Jun 30 19:38:30 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Production Guidelines
=====================

In this section, we will look at the guidelines for maintaining a production
Hasura project.

Managing Postgres and HasuraDB
------------------------------

This section describes the management of data related components (Postgres and HasuraDB) and also a workflow for making changes on the staging server and applying the changes to the production server.

1) Managing data components
^^^^^^^^^^^^^^^^^^^^^^^^^^^

``Exporting Postgres data``

Use pg_dump.

``Exporting HasuraDB state``

HasuraDB only tracks relationships and permissions. The entire hasuradb state can be exported in json. The exported json is essentially a hasuradb query which when executed on a vanilla hasuradb, all the relationships and permissions are added to the tables.

* ``Step-1:`` Setup a tunnel from your local machine to the appropriate Postgres service on which hasuradb is running.

.. code-block:: console

  $ ssh -p 22 -L 0.0.0.0:6432:postgres.hasura:5432 hasura@ssh.<project-name>.hasura-app.io

Change the port number and project name parameters appropriately.

.. note::
  Notice the 0.0.0.0; it is needed to bind the port to all interfaces on the host. We’ll need it to run the next command.

* ``Step-2:`` Export the state

.. code-block:: console

  $ docker run hasura/raven:0.8.8 raven --host 172.17.0.1 -p 6432 -u admin -p 'password' -d hasuradb export > hdb-metadata.json
.. note::
  Notice the **172.17.0.1**, this is the docker interface. The remote postgres can also be contacted at this ip because we bound the remote postgres at 6432 on all network interfaces of the local machine (using 0.0.0.0:6432 in the ssh command). On a Mac, this have to change to your docker machine’s ip.

``Resetting HasuraDB``

Resetting HasuraDB is to remove all its state (relationships and permissions). This comes in handy in these scenarios:

1. HasuraDB is failing to start because of a schema change which resulted in an inconsistent state of relationships and permissions.
2. Clone the HasuraDB state from another instance.

Following these steps will reset the entire HasuraDB's state:

1. Setup a tunnel to the appropriate postgres instance.
2. Setup kubectl's context to point to the right k8s cluster.
3. Stop hasuradb

.. code-block:: console

  $ kubectl scale --replicas=0 deployment/data

4. Clean and initialise hasuradb

.. code-block:: console

  $ docker run hasura/raven:0.8.8 raven --host 172.17.0.1 -p 6432 -u admin -p 'password' -d hasuradb clean
  $ docker run hasura/raven:0.8.8 raven --host 172.17.0.1 -p 6432 -u admin -p 'password' -d hasuradb initialise

5. Start hasuradb

.. code-block:: console

  $ kubectl scale --replicas=1 deployment/data

.. note::

  Always export hasuradb's state before resetting it.


2) Workflow for propagating changes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

``One time step``

*Assumption*: The current staging the production systems are identical.

1. Setup a tunnel to the staging postgres service
2. Take a backup of the 'public' schema of the hasuradb database.

.. code-block:: console

  $ pg_dump -h localhost -p 6432 -U admin -d hasuradb -n public > myproject-base.sql

3. Export the hasuradb state (refer to section 1.2) to say *hdb-metadata-base.json*.
4. Setup alembic. Refer to http://alembic.zzzcomputing.com/en/latest/tutorial.html. The scripts directory should be tracked in git as a part of the larger repo or on its own.


``The workflow``

Let's say we are adding a new feature which requires adding new tables, columns, relationships and permissions. This is the procedure that needs to be followed:

#.  Develop the feature on staging

    #. Create an alembic migration with the necessary changes to tables and columns (tables/columns). **DO NOT** use  console/psql/adminer to modify Postgres schema. You can however use these to modify the data.

    #. Add the necessary relationships and permissions either from the console UI or directly POSTing a query at data service's v1/query

#.  Applying these changes on production

    a. Export hasuradb state from staging (section 1.2). Let’s call this file *hdb-metadat-staging.json*.

    b. Setup a tunnel to the production’s postgres and data service:

    .. code-block:: console
        
      $ ssh -p 22 -L 0.0.0.0:6432:postgres.hasura:5432 -L 8080:data.hasura:80 hasura@ssh.<project-name>.hasura-app.io
  
    c. Take backup of postgres database and the metadata of the data service.

    d. Ensure that you haven’t skipped step 3.

    e. Apply staging Postgres changes to production Postgres:
    
    .. code-block:: console

      $ alembic upgrade head

    f. Reset the production hasuradb' state (section 1.3)

    g. Import production hasuradb’s state from hdb-metadata-staging.json:

    .. code-block:: console

      $ cat hdb-metadata-staging.json | http POST http://localhost:8080/v1/query 'X-Hasura-User-Id:0' 'X-Hasura-Role:admin'


Database backups and Point-in-time recovery (PITR) on Hasura
-----------------------------------------------------------------------

For instructions on setting up backup and Point-in-time recovery (PITR) for your project's PostgreSQL instance, please refer to https://github.com/hasura/continuous-backup. You will find a set of shell scripts and steps that will help you set this up. Please note that the scripts currently support backing up your data to only Amazon S3. Support for Azure blob storage, Google container storage and OpenStack Swift is coming soon.

.. _Postgres continuous archiving: https://www.postgresql.org/docs/current/static/continuous-archiving.html

.. _Backup strategies on Postgres: https://www.postgresql.org/docs/current/static/backup.html

.. note:: 
  
  Further reading on this topic:

  #. `Postgres continuous archiving`_

  #. `Backup strategies on Postgres`_