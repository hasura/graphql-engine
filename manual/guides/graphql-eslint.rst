Setting up Eslint for Graphql and Apollo Client
===============================================

.. image:: eslint.gif

Install the eslint plugin by running the following in the same directory as your ``package.json`` file.

.. code-block:: bash

  $ npm install --save-dev eslint-plugin-graphql

Next, we need to fetch the graphql schema

.. code-block:: Bash

   $ curl -H 'Authorization: Bearer <admin-token>' 'https://data.<cluster-name>.hasura-app.io/v1alpha1/graphql/schema' | jq -r '.schema' > schema.graphql

Now that you have the GraphQL schema, you can generate ``schema.json`` as follows:

.. code-block:: Bash

   $ npm install -g apollo-codegen
   $ apollo-codegen introspect-schema schema.graphql --output schema.json


The above command will save a ``schema.json`` file in your current directory.

Next, create a file named ``.eslintrc.js`` in the directory with your ``package.json`` file

.. code-block:: javascript

  module.exports = {
    parser: "babel-eslint",
    rules: {
      "graphql/template-strings": ['error', {
        env: 'apollo',

        // Import your schema JSON here
        schemaJson: require('./schema.json'),
      }]
    },
    plugins: [
      'graphql'
    ]
  }

To know about the other rules that you can apply, check out the repository `here <https://github.com/apollographql/eslint-plugin-graphql>`_.
