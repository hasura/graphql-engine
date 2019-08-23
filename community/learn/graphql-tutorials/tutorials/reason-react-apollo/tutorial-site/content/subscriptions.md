---
title: "Subscriptions to show online users"
metaTitle: "Update last seen of user with Mutation | GraphQL ReasonML React Apollo Tutorial"
metaDescription: "GraphQL Mutation to update last seen of user to make them available online. Use setInterval to trigger mutation every few seconds "
---

import GithubLink from "../src/GithubLink.js";
import YoutubeEmbed from "../src/YoutubeEmbed.js";

We cruised through our GraphQL queries and mutations. We queried for todos, added a new todo, updated an existing todo, removed an existing todo.

Now let's get to the exciting part.

GraphQL Subscriptions
---------------------

We have a section of UI which displays the list of online users. So far we have made queries to fetch data and display them on the UI. But typically online users data is dynamic.

We can make use of GraphQL Subscription API to get realtime data from the graphql server to efficiently handle this.

But but but...

We need to tell the server that the user who is logged in is online. We have to poll our server to do a mutation which updates the `last_seen` timestamp value of the user.

We have to make this change to see yourself online first. Remember that you are already logged in, registered your data in the server, but not updated your `last_seen` value.?

The goal is to update every few seconds from the client that you are online. Ideally you should do this after you have successfully authenticated with Auth0. So let's update some code to handle this. 

Firstly, we need to access the `client` instance to be able to fire mutations without using Mutation component.

Open `src/App.re` and `OnlineUsersWrapper` component with `ReasonApollo.Consumer` and pass the client instance as a prop to `OnlineUsersWrapper`. You can also pass it directly using `ApolloClient.instance` but using `ReasonApollo.Consumer` is the recommended way.

```js
  <ReasonApollo.Provider client=ApolloClient.instance>
    <Navbar />
    <div className="container-fluid p-left-right-0">
      <div className="col-xs-12 col-md-9 p-left-right-0">
        <div className="col-xs-12 col-md-6 sliderMenu p-30">
          <TodoPrivateWrapper />
        </div>
        <div className="col-xs-12 col-md-6 sliderMenu p-30 bg-gray border-right">
          <TodoPublicWrapper />
        </div>
      </div>
      <div className="col-xs-12 col-md-3 p-left-right-0">
        <div className="col-xs-12 col-md-12 sliderMenu p-30 bg-gray">
+        <ReasonApollo.Consumer>
+          {
+            client => {
+              <OnlineUsersWrapper client={client}/>
+            }
+          }
+        </ReasonApollo.Consumer>
-        <OnlineUsersWrapper/>
        </div>
      </div>
    </div>
  </ReasonApollo.Provider>

```
 
Also change the `OnlineUsersWrapper` component to accept the `client` prop.

```js
-let make = () => {
+let make = (~client) => {
```

Now, open `src/GraphQLQueries.re` and define a mutation to update last seen of the current user. Add the following code at the bottom of the file.

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/reason-react-apollo/app-final/src/GraphQLQueries.re" text="GraphQLQueries.re" />

```
// GraphQL mutation to update last seen
module UpdateLastSeen = [%graphql
  {|
    mutation updateLastSeen {
      update_users(where: {}, _set: {last_seen: "now()"}) {
        affected_rows
      }
    }
  |}
];
```

Now we can fire this mutation every 5 seconds to mark the current user as online. Let's first write a function to fire this mutation first. Open `src/online-users/OnlineUsersWrapper.re` and define this function inside the `make` function.


<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/reason-react-apollo/app-final/src/online-users/OnlineUsersWrapper.re" text="OnlineUsersWrapper.re" />

```js
let updateMyLastSeen = () => {
  let updateLastSeenMutation = GraphQLQueries.UpdateLastSeen.make(());
  let mutation = {
    "mutation": ApolloClient.gql(. updateLastSeenMutation##query),
    "variables": updateLastSeenMutation##variables
  };
  client##mutate(mutation);
  ();
};
```

Here, we are first defining a mutation object by passing our mutation and variables. Note that we are wrapping the mutation in `ApolloClient.gql`; this converts the query string into an AST that our ApolloClient instance understands. We finally call the `client##mutate` method with the mutation object to fire the mutation.

Now using the useEffect hook, start firing this mutation every 5 seconds:

```js
React.useEffect0(
  () => {
    let timerId = Js.Global.setInterval(updateMyLastSeen, 5000);
    Some(() => Js.Global.clearInterval(timerId));
  }
);
```

After adding the above two code snippets, your `OnlineUsersWrapper.re` file should look something like:

```js
[@react.component]
-let make = () => {
+let make = (~client) => {
+  let updateMyLastSeen = () => {
+    let updateLastSeenMutation = GraphQLQueries.UpdateLastSeen.make(());
+    let mutation = {
+      "mutation": ApolloClient.gql(. updateLastSeenMutation##query),
+      "variables": updateLastSeenMutation##variables
+    };
+    client##mutate(mutation);
+    ();
+  }
+  React.useEffect0(
+    () => {
+      let timerId = Js.Global.setInterval(updateMyLastSeen, 5000);
+      Some(() => Js.Global.clearInterval(timerId));
+    }
+  );
   <div className="onlineUsersWrapper">
      <div className="sliderHeader">
        {ReasonReact.string(onlineUsersTitle)}
      </div>
      {ReasonReact.array(Array.of_list(onlineUsers))}
   </div>
}
```

Great! Now the metadata about whether the user is online will be available in the backend. Let's now do the integration to display realtime data of online users.
