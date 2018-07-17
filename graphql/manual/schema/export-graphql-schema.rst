Exporting the Hasura GraphQL schema
===================================

If you need to share or export the GraphQL schema, you can use community tooling such as `apollo <https://github.com/apollographql/apollo-cli>`_, `get-graphql-schema <https://github.com/prismagraphql/get-graphql-schema>`_ etc. For example, using `apollo`, you can get the schema as follows:

Run ``npm install -g apollo`` to install ``apollo``. Then you can run the following commands to download the GraphQL schema.

.. code-block:: bash

   # If the GraphQL engine is running locally at http://localhost:8080/v1alpha1/graphql,
   # without an access key
   $ apollo schema:download --endpoint=http://localhost:8080/v1alpha1/graphql

   # If Hasura GraphQL Engine is running with an access key
   $ apollo schema:download --header='x-hasura-access-key':'12345' --endpoint=http://localhost:8080/v1alpha1/graphql


By default, it downloads the schema in ``json`` format. If you want it in native GraphQL format, you can use an additional arguement ``schemaName.graphql``.

.. code-block:: bash

	# Getting the schema in .graphql format	
	$ apollo schema:download --endpoint=http://localhost:8080/v1alpha1/graphql ./schema.graphql