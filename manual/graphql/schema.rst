GraphQL Schema
==============

Traditionally, while implementing a GraphQL backend, you need to define your schema, types and fields. However, in case of Hasura, you just have to create tables and everything else is automatically generated.

Accessing the schema
--------------------

To get your GraphQL schema:

1. Go to the :ref:`API-Explorer <api_explorer>`.

2. Go to the JSON Raw section on the left.

3. Add your admin token to the headers and make a ``GET`` request to:

   .. code-block:: http

      https://data.<cluster-name>.hasura-app.io/v1alpha1/graphql/schema

4. Hit send

   .. image:: img/graphql-get-schema.png


Using the community tooling around the schema
---------------------------------------------

- **Apollo codegen**

  As we don't yet support introspection over the graphql endpoint, the standard tooling (`apollo-codegen <https://github.com/apollographql/apollo-codegen>`_) to generate ``schema.json`` will not work out of the box. You'll need to run an additional command to fetch the schema as follows:

  .. code-block:: Bash

     $ curl -H 'Authorization: Bearer <auth-token>' 'https://data.<cluster-name>.hasura-app.io/v1alpha1/graphql/schema' | jq -r '.schema' > schema.graphql

  Now that you have the GraphQL schema, you can generate ``schema.json`` as follows:

  .. code-block:: Bash

     $ apollo-codegen introspect-schema schema.graphql --output schema.json

- **ESLint**: Check :ref:`this guide <guide-graphql-eslint>` for using your schema to set up ESLint.
