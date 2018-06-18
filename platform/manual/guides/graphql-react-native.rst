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


Queries and mutations
~~~~~~~~~~~~~~~~~~~~~

Firstly, we will define our required queries and mutations as graphql strings.

.. snippet:: javascript
  :filename: graphqlStrings.js

    const FETCH_TODOS = gql`
      query fetch_todos{
        todos {
          id
          task
          completed
          user_id
        }
      }
    `;

    const INSERT_TODO = gql`
      mutation insert_todos ($objects: [todos_input!]){
        insert_todos(objects: $objects) {
          affected_rows
          returning {
            id
          }
        }
      }
    `;

    const UPDATE_TODO = gql`
      mutation update_todos{
        update_todos(where: {id: {_eq: $todo_id}} _set: {completed: $completed}) {
          affected_rows
        }
      }
    `;

    const DELETE_TODO = gql`
      mutation delete_todos{
        delete_todos(where: {id: {_eq: $todo_id}}) {
          affected_rows
        }
      }
    `;

    export {
      INSERT_TODO,
      FETCH_TODOS,
      UPDATE_TODO,
      DELETE_TODO
    }

In the above queries:

* ``todos`` above is the name of the table. This is the convention followed by Hasura for GraphQL queries.

* ``insert_todos`` is a convention when inserting data into a table. It is of type ``insert_<TABLE_NAME>``, where <TABLE_NAME> is to be replaced with the name of the table to which data is being inserted.

* You can insert more than one row at a time. Denoted by ``$objects: [todos_input]``.

* ``todos_input`` is the type of the data that will be inserted into the table. The convention followed is ``<TABLE_NAME>_input`` where <TABLE_NAME> is to be replaced with the name of the table to which data is being inserted.

* ``affected_rows`` is the number of rows that were inserted.

* The ``returning`` key specifies the data you want returned after a successful insertion. In this case, we are asking for the ``id``, ``task`` and ``completed`` columns.

* ``update_todos`` is a convention when updating data in a table. It is of type ``update_<TABLE_NAME>``, where <TABLE_NAME> is to be replaced with the name of the table where you want to update data.

* ``where: {id: {_eq: 4}}`` checks that the ``id`` of the row that is being updated is ``4``.

* The ``delete_todos`` query is very similar to the update mutation, the only difference is that the convention to delete data from a table is ``delete_<TABLE_NAME>`` where <TABLE_NAME> is to be replaced with the name of the table where you want to delete data.

Now lets write sample React Native components that use these queries.

Query Components
~~~~~~~~~~~~~~~~

To fetch all todos, and render the tasks as a  list of `<Text>`, you can use

.. code-block:: javascript


  const TodoListComponent = () => (
    <Query
      query={FETCH_TODOS}
    >
      {({loading, error, data}) => {
        return data.todos.map((todo, index) => {
          return (
            <Text>{todo.task}</Text>
          );
        });
      }}
    </Query>
  )


Mutation Components
~~~~~~~~~~~~~~~~~~~

Insert
^^^^^^

Below is the code snippet for a ``Button`` that ``inserts`` an element in the ``todos`` table.

.. code-block:: javascript

  const AddButton = (props) => (
    <Mutation
      mutation={INSERT_TODO}
      {(insert_todos, {data}) => (
        <Button
          title="Insert Todo"
          style={props.style}
          onPress={() => {
            insert_todos({
              variables: {
                objects: [{
                  task: "Sample todo task",
                  completed: false,
                  user_id: 44
                }]
              }
            });
          }}
        />
      )}
    </Mutation>
  )

In most cases, you also need to update the memory cache of the Apollo client in order to reflect changes in the UI. To do that, you just have to add an update prop in the ``Mutation`` component:

.. code-block:: javascript

    update= {(cache, {data: {insert_todos}}) => {
      const data = cache.readQuery({ query: FETCH_TODOS});
      const newTodo = {
        task: "Sample todo task"
        completed: false,
        user_id: 44,
        id: insert_todos.returning[0].id
      }
      data.todos.push(newTodo);
      cache.writeQuery({query: FETCH_TODOS, data})
    }}

Update
^^^^^^

The ``Button`` below, sets the ``completed`` status of a task (id = 4) to ``true``.

.. code-block:: javascript

  // this component should receive a prop called `todo` which is the todo item object to be updated
  const UpdateButton= (props) => {
    return (
      <Mutation
        mutation={
          gql`
            mutation update_todos{
              update_todos(where: {id: {_eq: 4}} _set: {completed: true}) {
                affected_rows
              }
            }
          `
        }
      >
        {
          (update_todos) => (
            <Button
              onPress={() => {
                update_todos({
                  variables: {
                    todo_id: props.todo.id,
                    completed: !props.todo.completed
                  }
                })
              }}
              title="Update todo"
            >
          )
        }
      </Mutation>
    )
  }

To update the Apollo cache after performing the mutation, you just have to add an update prop in the ``Mutation`` component:

.. code-block:: javascript

    update={(cache) => {
      const data = cache.readQuery({ query: FETCH_TODOS});
      cache.writeQuery({
        query: FETCH_TODOS,
        data: {
          ...data,
          todos: data.todos.map((todo) => {
            if (todo.id === props.todo.id) {
              return {
                ...todo,
                completed: !todo.completed
              }
            }
            return todo;
          })
        }
      })
    }}


Delete
^^^^^^

Finally, if you want a ``Button`` to delete a task with ``id = 4``, you can use

.. code-block:: javascript

    // this component should receive a prop called `todo` which is the todo item to be updated
    const DeleteButton = (props) => (
      <Mutation
        mutation={DELETE_TODO}

      >
        {(delete_todos, {data}) => (
          <Button
            title="Delete"
            style={props.style}
            onPress={() => {
              delete_todos({
                variables: {
                  todo_id: props.todo.id
                }
              });
            }}
          />
        )}
      </Mutation>
    );


To update the cache after deletion, add the following ``update`` prop to the Mutation component:

.. code-block:: javascript

    update= {(cache) => {
      const data = cache.readQuery({ query: FETCH_TODOS});
      cache.writeQuery({
        query: FETCH_TODOS,
        data: {
          ...data,
          todos: data.todos.filter((todo) => (props.todo.id !== todo.id))
        }
      })
    }}

Reference
----------

* `Hasura GraphQL <https://docs.hasura.io/0.15/manual/data/graphql.html>`_
* `Apollo Client <https://www.apollographql.com/docs/react/>`_
