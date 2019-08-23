---
title: "<Query> component"
metaTitle: "Apollo Query Component | GraphQL React Apollo Tutorial"
metaDescription: "We will use the Apollo Client Query component from react-apollo. It is a render prop API to fetch data and handle data, loading and error props"
---

import GithubLink from "../../src/GithubLink.js";
import YoutubeEmbed from "../../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/60-y9jygWBA" />

In this section, we will implement GraphQL Queries and integrate with the react UI.
With Apollo Client, you can send queries in 2 different ways.

1. Using the `query` method directly and then process the response.
2. New Render Prop API with React. (Recommended)

### Apollo Query Component
The recommended method is to use the render prop method, where you will just pass your GraphQL query as prop and `<Query />` component will fetch the data automatically and will present it in the component's render prop function.

Great! Now let's define the graphql query to be used:

Open `src/components/Todo/TodoPrivateList.js` and add the following code:

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/react-apollo/app-final/src/components/Todo/TodoPrivateList.js" text="src/components/Todo/TodoPrivateList.js" />

```javascript
import React, { Component, Fragment } from "react";

import TodoItem from "./TodoItem";
import TodoFilters from "./TodoFilters";
+ import gql from 'graphql-tag';

+ const GET_MY_TODOS = gql`
+  query getMyTodos {
+    todos(where: { is_public: { _eq: false} }, order_by: { created_at: desc }) {
+      id
+      title
+      created_at
+      is_completed
+  }
+ }`;
```

We have now written the graphql query as a javascript constant using the `gql` parser function. This function is used to parse the plain string as a graphql query.

What does this query do? 
------------------------
The query fetches `todos` with a simple condition; `is_public` must be false. We sort the todos descending by its `created_at` time according to the schema. We specify which fields we need for the todos node.

The query is now ready, let's integrate it with our react code.

```javascript

+ import {Query} from 'react-apollo';

```

`<Query>` component is being imported from `react-apollo`

```javascript

import React, { Component, Fragment } from "react";

import TodoItem from "./TodoItem";
import TodoFilters from "./TodoFilters";
import gql from 'graphql-tag';

const GET_MY_TODOS = gql`
  query getMyTodos {
    todos(where: { is_public: { _eq: false} }, order_by: { created_at: desc }) {
      id
      title
      created_at
      is_completed
  }
}`;

class TodoPrivateList extends Component {
  ...
}

+ const TodoPrivateListQuery = () => {
+  return (
+    <Query query={GET_MY_TODOS}>
+      {({ loading, error, data, client}) => {
+        if (loading) {
+          return (<div>Loading...</div>);
+        }
+        if (error) {
+          console.error(error);
+          return (<div>Error!</div>);
+        }
+        return (<TodoPrivateList client={client} todos={data.todos} />);
+      }}
+    </Query>
+  );
+ };

export default TodoPrivateList;
```

Remember that we wrapped our App component with `<ApolloProvider>` and passed `client` as a prop. We are using the same client prop to send it down to the components.

We are importing the `Query` component from `react-apollo` and the graphql query we defined above to fetch the todo data.

Let's remove the mock `todos` data which was used to populate sample data.

```javascript

class TodoPrivateList extends Component {
  constructor(props) {
    super(props);

    this.state = {
      filter: "all",
      clearInProgress: false,
-      todos: [
-        {
-          id: "1",
-          title: "This is private todo 1",
-          is_completed: true,
-          is_public: false
-        },
-        {
-          id: "2",
-          title: "This is private todo 2",
-          is_completed: false,
-          is_public: false
-        }
-      ]
    };

    this.filterResults = this.filterResults.bind(this);
    this.clearCompleted = this.clearCompleted.bind(this);
  }

  filterResults(filter) {
    this.setState({
      ...this.state,
      filter: filter
    });
  }

  clearCompleted() {}

  render() {
-    let filteredTodos = this.state.todos;
+    const {todos} = this.props;
+
+    let filteredTodos = todos;
    if (this.state.filter === "active") {
-     filteredTodos = this.state.todos.filter(todo => todo.is_completed !== true);
+     filteredTodos = todos.filter(todo => todo.is_completed !== true);
    } else if (this.state.filter === "completed") {
-     filteredTodos = this.state.todos.filter(todo => todo.is_completed === true);
+     filteredTodos = todos.filter(todo => todo.is_completed === true);
    }

    const todoList = [];
    filteredTodos.forEach((todo, index) => {
      todoList.push(
        <TodoItem
          key={index}
          index={index}
          todo={todo}
        />
      );
    });

    return (
      ...
    );
  }
}

```

Finally, update the exports to render the function returning the `<Query>` component.

```javascript
- export default TodoPrivateList;
+ export default TodoPrivateListQuery;
+ export {GET_MY_TODOS};
```

Then, we wrap the new functional component with `Query` passing our graphql query.

Woot! You have written your first GraphQL integration with React. Easy isn't it?

How does this work?
-------------------
When you wrapped your return with `<Query>` component, Apollo injected props into the component’s render prop function. Most important ones are:

`loading`: A boolean that indicates whether the request is in flight. If loading is true, then the request hasn't finished. Typically this information can be used to display a loading spinner.

`error`: A runtime error with graphQLErrors and networkError properties. Contains information about what went wrong with your query.

`data`: An object containing the result of your GraphQL query. This will contain our actual data from the server. In our case, it will be the todo data.

You can read more about other render props that Apollo passes [here](https://www.apollographql.com/docs/react/essentials/queries.html#render-prop)

Using the `data` prop, we are parsing the results from the server. In our query, `data` prop has an array `todos` which can be mapped over to render each `TodoItem`.

If you noted, there has been some client side filtering to the todos that are displayed.
