Migrations
==========

By default, when you run the GraphQL engine you can use the Hasura console served by the GraphQL engine to make
changes to your schema. However, if you have different environments or are adding GraphQL to an existing application or database, you'll
need migrations to make sure that your iteration and CI/CD process is smooth.

Hasura GraphQL engine comes with powerful Rails-inspired migration tooling to help you keep track of the changes
you make to your schema. As you use the Hasura console, the Hasura CLI will spit out migration files for
you that you can put in version control and even edit manually.

Follow the guide that best fits your use-case:

- :doc:`Migrations for a new project <new-app>`: If you are just starting to setup the Hasura GraphQL engine.
- :doc:`Migrations for an existing project <enable-migrations>`: In case you already have a Hasura GraphQL engine setup
  and want to start using migrations.
- :doc:`Migrations with an existing database or migration system <existing-database>`: Use Hasura only for GraphQL
  schema changes and not database migrations.

.. toctree::
   :hidden:

   New project <new-app>
   Existing project <enable-migrations>
   With an existing database <existing-database>
