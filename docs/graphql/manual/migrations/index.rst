.. _migrations:

Migrations
==========

By default, when you run the GraphQL engine you can use the Hasura console served by the GraphQL engine to make
changes to your schema. However, if you have different environments or are adding GraphQL to an existing application
or database, you'll need migrations to make sure that your iteration and CI/CD process is smooth.

Hasura GraphQL engine comes with powerful Rails-inspired migration tooling to help you keep track of the changes
you make to your schema. As you use the Hasura console, the Hasura CLI will spit out migration files for
you that you can put in version control and even edit manually.

Follow the guide that best fits your scenario:

- :doc:`Migrations for a new project <new-project>`: If you haven't yet started setting up your schema.
- :doc:`Migrations for an existing project <existing-project>`: In case you already have set up a schema
  and want to start using migrations on it.
- :doc:`Migrations with a database with an existing migration system <database-with-migrations>`: Use Hasura only
  for GraphQL schema changes and not database migrations.

Advanced:

- :doc:`Auto-apply migrations when server starts <auto-apply-migrations>`

.. toctree::
   :hidden:

   For new project <new-project>
   For existing project <existing-project>
   With a database with an existing migration system <database-with-migrations>
   Auto-apply migrations when server starts <auto-apply-migrations>
