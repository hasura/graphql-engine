GraphQL with React Native
=========================

This section covers how to use GraphQL with React Native using Apollo Client.

Configuration
-------------

1. Install the required dependencies

.. code-block:: bash

  $ npm install apollo-boost react-apollo graphql --save

2. Create an `ApolloClient <https://www.apollographql.com/docs/react/basics/setup.html#ApolloClient>`_ instance and point it to the Hasura Data GraphQL URL via `Apollo Link <https://www.apollographql.com/docs/link/>`_.

   .. code-block:: javascript

    import { ApolloClient } from 'apollo-client';
    import { HttpLink } from 'apollo-link-http';
    import { InMemoryCache } from 'apollo-cache-inmemory';

    const GRAPHQL_URL = "https://data.<cluster-name>.hasura-app.io/v1alpha1/graphql";
    const httpLink = new HttpLink({ uri: GRAPHQL_URL});

    const client = new ApolloClient({
      link: httpLink,
      cache: new InMemoryCache({
        addTypename: false
      })
    });

   .. note::

     Important: You have to configure ``addTypename`` to false in the ``InMemoryCache`` constructor.

   If you have to authorize your queries and mutations, you need to pass request headers to the Apollo Client using a middleware.

   .. code-block:: javascript

    const GRAPHQL_URL = `https://data.<cluster-name>.hasura-app.io/v1alpha1/graphql`
    const httpLink = new HttpLink({ uri: GRAPHQL_URL});

    // adding auth headers
    const authMiddleware = new ApolloLink((operation, forward) => {
      AsyncStorage.getItem("@<cluster-name>:myapp").then((session) => {
        operation.setContext({
          headers: {
            authorization: session ? "Bearer " + session.token : null
          }
        });
      })
      return forward(operation);
    });

    // Creating a client instance with auth middlewar
    const client = new ApolloClient({
      link: concat(authMiddleware, httpLink),
      cache: new InMemoryCache({
        addTypename: false
      })
    });


3. Connect the client to your component tree using the ``ApolloProvider`` component. It is important to put ``ApolloProvider`` above every component where you need the GraphQL data. For example, it could be before registering your root component.

   .. code-block:: javascript

    import { ApolloProvider } from 'react-apollo';
    import { ApolloClient } from 'apollo-client';
    import { HttpLink } from 'apollo-link-http';
    import { InMemoryCache } from 'apollo-cache-inmemory';
    import { App } from './App';

    const GRAPHQL_URL = "https://data.<cluster-name>.hasura-app.io/v1alpha1/graphql";

    const client = new ApolloClient({
      link: new HttpLink({uri: GRAPHQL_URL}),
      cache: new InMemoryCache()
    });

    const AppWithClient= () => (
      <ApolloProvider client={client}>
        <App/>
      </ApolloProvider>
    );

    AppRegistry.registerComponent('MyApplication', () => AppWithClient);


That's it. You can now make queries and mutations in all the children components.

Examples
--------

We will use a simple TODO-list schema for demonstrating queries with React Native components.

**Table Name: todos**

+-----------+-----------------------------+
| column    | type                        |
+===========+=============================+
| id        | serial NOT NULL primary key |
+-----------+-----------------------------+
| task      | text NOT NULL               |
+-----------+-----------------------------+
| completed | bool NOT NULL               |
+-----------+-----------------------------+
| user_id   | int NOT NULL                |
+-----------+-----------------------------+


Queries
~~~~~~~

To fetch all todos, and render the tasks as a  list of `<Text>`, you can use

.. code-block:: javascript

  export default graphql(gql`
    query fetch_todos{
      todos {
        id
        task
        completed
        user_id
      }
    }
  `)((props) => {
    return props.data.todos.map((todo, index) => {
      return (
        <Text>{todo.task}</Text>
      );
    });
  });

``todos`` above is the name of the table. This is the convention followed by Hasura for GraphQL queries.


Mutations
~~~~~~~~~

Insert
^^^^^^

Below is the code snippet for a ``Button`` that ``inserts`` an element in the ``todos`` table.

.. code-block:: javascript

  export default graphql(gql`
    mutation insert_todos ($objects: [todos_input!]){
      insert_todos(objects: $objects) {
        affected_rows
        returning {
          id
        }
      }
    }
  `)((props) => {
    return (
      <Button
        title="Insert Todo"
        onPress={() => {
          props.mutate({
            variables: {
              objects: [{
                task: "New task",
                completed: false,
                user_id: 34
              }]
            }
          });
        }}
      />
    );
  });

There are a few things to be noted here:

* ``insert_todos`` is a convention when inserting data into a table. It is of type ``insert_<TABLE_NAME>``, where <TABLE_NAME> is to be replaced with the name of the table to which data is being inserted.

* You can insert more than one row at a time. Denoted by ``$objects: [todos_input]``.

* ``todos_input`` is the type of the data that will be inserted into the table. The convention followed is ``<TABLE_NAME>_input`` where <TABLE_NAME> is to be replaced with the name of the table to which data is being inserted.

* ``affected_rows`` is the number of rows that were inserted.

* The ``returning`` key specifies the data you want returned after a successful insertion. In this case, we are asking for the ``id``, ``task`` and ``completed`` columns.

Update
^^^^^^

The ``Button`` below, sets the ``completed`` status of a task (id = 4) to ``true``.

.. code-block:: javascript

  export default graphql(gql`
    mutation update_todos{
      update_todos(where: {id: {_eq: 4}} _set: {completed: true}) {
        affected_rows
      }
    }
  `)((props) => {
    return (
      <Button
        title="Update Todo"
        onPress={() => {
          props.mutate();
        }}
      />
    );
  });

Things to be noted:

- ``update_todos`` is a convention when updating data in a table. It is of type ``update_<TABLE_NAME>``, where <TABLE_NAME> is to be replaced with the name of the table where you want to update data.

- ``where: {id: {_eq: 4}}`` checks that the ``id`` of the row that is being updated is ``4``.

Delete
^^^^^^

Finally, if you want a ``Button`` to delete a task with ``id = 4``, you can use

.. code-block:: javascript

  export default graphql(gql`
    mutation delete_todos{
      delete_todos(where: {id: {_eq: 4}}) {
        affected_rows
      }
    }
  `)((props) => {
    return (
      <Button
        title="Delete Todo"
        onPress={() => {
          props.mutate();
        }}
      />
    );
  });

The query is very similar to the update mutation, the only difference is that the convention to delete data from a table is ``delete_<TABLE_NAME>`` where <TABLE_NAME> is to be replaced with the name of the table where you want to delete data.

Reference
----------

* `Hasura GraphQL <https://docs.hasura.io/0.15/manual/data/graphql.html>`_
* `Apollo Client <https://www.apollographql.com/docs/react/>`_
