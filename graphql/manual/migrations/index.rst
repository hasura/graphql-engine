Migrations
==========

By default, when you run the GraphQL engine you can use the Hasura console served by the GraphQL engine to make changes to your schema.

However, if you have different environments or are adding GraphQL to an existing application or database, you'll need migrations to make sure that your iteration and CI/CD process is smooth.

Hasura comes with powerful rails-inspired migration tooling to help you keep track of the changes you make to your schema. As you use the Hasura console, the hasura CLI will spit out migration files for you that you can put in version control and even edit manually.

Follow the guide that best fits your use-case:

- :doc:`Enable migrations <enable-migrations>`: In case you already have a Hasura GraphQL engine setup and want to start using migrations.
- :doc:`Working on a new app <new-app>`: Use Hasura for tracking both database and GraphQL schema migrations. Ideal when you're building a new app.
- :doc:`Working alongside an existing database or migration system <existing-database>`: Use Hasura only for GraphQL schema changes and not database migrations.

.. toctree::
   :hidden:

   enable-migrations
   new-app
   existing-database
