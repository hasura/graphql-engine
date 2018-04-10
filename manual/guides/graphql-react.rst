GraphQL with React and Apollo Client
====================================

This section covers how to make GraphQL queries in your react app using the `Apollo Client <https://www.apollographql.com/docs/react/>`_.

Configuration
-------------

1. Install the required dependencies

.. code-block:: bash

   $ npm install apollo-boost react-apollo graphql --save

2. Create an `ApolloClient <https://www.apollographql.com/docs/react/basics/setup.html#ApolloClient>`_ instance and point it to the Hasura Data GraphQL URL via `Apollo Link <https://www.apollographql.com/docs/link/>`_.

.. code-block:: js

  import ApolloClient from 'apollo-client';
  import { createHttpLink } from 'apollo-link-http';
  import { ApolloProvider } from 'react-apollo';
  import { InMemoryCache } from 'apollo-cache-inmemory';

  const GRAPHQL_URL = "https://data.<cluster-name>.hasura-app.io/v1alpha1/graphql"; //Replace <cluster-name> with the name of your cluster

  const client = new ApolloClient({
    link: createHttpLink({
      uri: GRAPHQL_URL,
      credentials: 'include' // Include this to send the cookie along with every request
    }),
    cache: new InMemoryCache({
      addTypename: false
    })
  });

.. note::

  You have to configure ``addTypename`` to false in the ``InMemoryCache`` constructor.

3. Connect the client to your component tree using the ``ApolloProvider`` component. It is important to put ``ApolloProvider`` above every component where you need the GraphQL data. For example, it could be before your root component.

.. code-block:: js

  import React from 'react';
  import ReactDOM from 'react-dom';
  import App from './App';
  import ApolloClient from 'apollo-client';
  import { createHttpLink } from 'apollo-link-http';
  import { ApolloProvider } from 'react-apollo';
  import { InMemoryCache } from 'apollo-cache-inmemory';

  const GRAPHQL_URL = "https://data.<cluster-name>.hasura-app.io/v1alpha1/graphql"; //Replace <cluster-name> with the name of your cluster

  const client = new ApolloClient({
    link: createHttpLink({
      uri: GRAPHQL_URL,
      credentials: 'include' // Include this to send the cookie along with every request
    }),
    cache: new InMemoryCache({
      addTypename: false
    })
  });

  ReactDOM.render(
    <ApolloProvider client={client}>
      <App />
    </ApolloProvider>,
    document.getElementById('root')
  );

That's it. You can now make queries and mutations in all the children components.

Exploring the GraphQL queries
-----------------------------

We will use a simple TODO-list schema for demonstrating the queries.

**Table Name: todos**

+-----------+-----------------------------+
| Column    | Type                        |
+===========+=============================+
| id        | serial NOT NULL primary key |
+-----------+-----------------------------+
| task      | text NOT NULL               |
+-----------+-----------------------------+
| completed | bool NOT NULL               |
+-----------+-----------------------------+
| user_id   | int NOT NULL                |
+-----------+-----------------------------+

Querying for data
^^^^^^^^^^^^^^^^^

The graphql query to fetch all the todos from the `todos` table will be:

.. code-block:: js

  const QUERY_TODO = gql`
    query fetch_todos {
      todos {
        id
        task
        completed
      }
    }
  `;

``todos`` above is the name of the table. This is the convention followed by Hasura for GraphQL queries.

Mutations
^^^^^^^^^

To **insert** a row into a table, in this case, the ``todos`` table, the GraphQL mutation will be:

.. code-block:: js

  const MUTATION_TODO_ADD = gql`
    mutation insertTodoMutation ($objects: [todos_input]){
      insert_todos(objects: $objects) {
        affected_rows
        returning {
          id
          task
          completed
        }
      }
    }
  `;

There are a few things to be noted here:

* ``insert_todos`` is a convention when inserting data into a table. It is of type ``insert_<TABLE_NAME>``, where <TABLE_NAME> is to be replaced with the name of the table to which data is being inserted.

* You can insert more than one row at a time. Denoted by ``$objects: [todos_input]``.

* ``todos_input`` is the type of the data that will be inserted into the table. The convention followed is ``<TABLE_NAME>_input`` where <TABLE_NAME> is to be replaced with the name of the table to which data is being inserted.

* ``affected_rows`` is the number of rows that were inserted.

* The ``returning`` key specifies the data you want returned after a successful insertion. In this case, we are asking for the ``id``, ``task`` and ``completed`` columns.

To **update** a row in the table, the mutation looks like:

.. code-block:: js

  const MUTATION_TODO_UPDATE = gql`
    mutation updateTodoMutation ($todoId: Int, $set: todos_input) {
      update_todos(where: {id: {_eq: $todoId}} _set: $set) {
        affected_rows
      }
    }
  `;

Things to be noted:

- ``update_todos`` is a convention when updating data in a table. It is of type ``update_<TABLE_NAME>``, where <TABLE_NAME> is to be replaced with the name of the table where you want to update data.

- ``$todoId`` is the id of the row that needs to be updated and is of type ``Int``.

- ``$set`` is the object that this mutation expects, in this case, it should be of type ``todos_input``. The convention is the same as the one for the insert mutation.

- ``where: {id: {_eq: $todoId}}`` checks that the ``id`` of the row that is being updated is the same as the value of ``$todoId`` which will be passed as a variable to this mutation.

- The ``returning`` key is optional. You need not specify it, unless you want the data returned after the mutation.

To **delete** a row in the table, the mutation looks like:

.. code-block:: js

  const MUTATION_TODO_DELETE = gql`
    mutation deleteTodoMutation ($todoId: Int) {
      delete_todos(where: {id: {_eq: $todoId}}) {
        affected_rows
      }
    }
  `;

The query is very similar to the update mutation, the only difference is that the convention to delete data from a table is ``delete_<TABLE_NAME>`` where <TABLE_NAME> is to be replaced with the name of the table where you want to delete data.


Reference
----------

* `Hasura GraphQL <https://docs.hasura.io/0.15/manual/data/graphql.html>`_
* `Apollo Client <https://www.apollographql.com/docs/react/>`_
