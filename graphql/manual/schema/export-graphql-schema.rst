Exporting the Hasura GraphQL schema
===================================
If you need to share or export the GraphQL schema, you can use the **GET** ``/v1alpha1/graphql/schema`` API to do so.

.. code-block:: Bash

   # If Hasura is running locally at localhost:8080
   # without an access key
   $ curl 'http://localhost:8080/v1alpha1/graphql/schema' | jq -r '.schema' > schema.graphql

   # If Hasura is running with an access key
   $ curl -H 'X-Hasura-Access-Key: xxxxxxxxx' 'http://localhost:8080/v1alpha1/graphql/schema' | jq -r '.schema' > schema.graphql

Once you have this schema, you can use community tooling to do things like codegen.

.. code-block:: Bash

   $ npm install -g apollo-codegen
   $ apollo-codegen introspect-schema schema.graphql --output schema.json
