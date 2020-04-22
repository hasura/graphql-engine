.. meta::
   :description: Instructions to migrate your graph.cool project to Hasura
   :keywords: hasura, docs, guide, GraphQL subscriptions, apollo, apollo-client

Migrate from graph.cool to Hasura
=================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

If you are looking to migrate your graph.cool project to Hasura, these are the steps that you will need to roughly follow.

Migrating Data
--------------

1. Export your Graph.cool data using their https://export.graph.cool export tool. This will give a MySQL binary dump of your current data.

.. thumbnail:: ../../../../img/graphql/manual/guides/graphcool-export.png
   :alt: Graph.cool Export

2. Start MySQL with docker

docker run --name graphcool-mysql -e MYSQL_ROOT_PASSWORD=my-secret-pw -p 3306:3306 -d mysql:latest --default-authentication-plugin=mysql_native_password

Replace the password as required.

3. Connect to MySQL via mysql CLI

mysql --user=root --password=my-secret-pw --host=<host>

Replace the host and password as necessary.

4. Create a database in MySQL

```
create database public;
```

5. Import graphcool's MySQL export into your local MySQL instance

```
mysql --user=root --password=my-secret-pw --host=<host> public --binary-mode=1 < <pathtomysqlexport>
```

Replace host and pathtomysqlexport as required. This step should have migrated schema with the data.

6. Start Hasura and Postgres using docker-compose

Since MySQL is now setup with all the data from graph.cool, we need to create Hasura and Postgres instance to proceed.

Refer the `Getting started: <https://hasura.io/docs/1.0/graphql/manual/getting-started/docker-simple.html#docker-simple>`__ guide to setup Hasura using docker-compose.


We will use `pgloader` to migrate from MySQL to Postgres. Refer their `installation guide <https://github.com/dimitri/pgloader>`__ for setting this up.

Once you have installed, execute the following command:

```
pgloader mysql://root:my-secret-pw@<host>/public postgresql://postgres:postgrespassword@<host>:5432/postgres
```

Replace <host> as necessary.

This step should have migrated data from MySQL to Postgres.

Connect Hasura to Postgres
--------------------------
Once the dataset is in Postgres, Hasura should be able to track tables and relationships. 


You might also have to look at configuring enums differently in case you have them in your Graph.cool project. (https://hasura.io/docs/1.0/graphql/manual/schema/enums.html#enums-in-the-hasura-graphql-engine)

Restructuring connection tables
-------------------------------
Now you can rename tables/columns to match your client side queries as required. 
Do note that, for every one to one relationship, Graph.cool would have created a connection table to link them. This would require a bit of work to restructure. Currently there is no automation available for this step. Carefully review the connection tables and make the necessary changes.


Migrating Functions
-------------------

In case you have Functions in Graph.cool, Hasura has an equivalent feature in Event Triggers (https://hasura.io/docs/1.0/graphql/manual/event-triggers/index.html) and Custom Resolver (https://hasura.io/docs/1.0/graphql/manual/actions/index.html). Migrating this would involve taking your code and deploying it on a different platform (preferably serverless functions).

Do note that for event triggers, the payload that Hasura sends might be different and you might have to change the way request body parameters are handled in your function code.


Migrating Auth
--------------

If you were using Auth0 with Graph.cool, the migration should be fairly straightforward. You can configure Hasura with Auth0 easily by following this `guide - <https://hasura.io/docs/1.0/graphql/manual/guides/integrations/auth0-jwt.html#auth0-jwt>`__

Migrating Permissions
---------------------

The CRUD permissions in Graph.cool can be manually migrated to Hasura's Permission system. You can define role(s) in Hasura and configure permissions declaratively for all the CRUD operations. 

Note that this guide is not comprehensive and some steps require manual intervention.

