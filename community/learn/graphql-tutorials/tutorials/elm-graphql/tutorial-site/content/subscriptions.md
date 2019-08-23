---
title: "Subscriptions to show online users"
metaTitle: "Update last seen of user with Mutation | GraphQL Elm Tutorial"
metaDescription: "GraphQL Mutation to update last seen of user to make them available online."
---

import GithubLink from "../src/GithubLink.js";

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

Open `src/Main.elm` and configure the following ports

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/elm-graphql/app-final/src/Main.elm" text="src/Main.elm" />

```
main : Program () Model Msg                                                                                                                          
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }

+port createSubscriptionToOnlineUsers : ( String, String ) -> Cmd msg
+port gotOnlineUsers : (Json.Decode.Value -> msg) -> Sub msg
```

What we have done here is we have created two ports - one to send information from elm and one to receive information from js.

Lets setup a timer on the elm side which will invoke a function every 30 seconds. We can setup a timer using subscriptions. Here is how the setup will look like

```

-- Imports --
import Iso8601
import Time

-- Subscriptions --

subscriptions : Model -> Sub Msg
subscriptions model =
- 	Sub.none
+   case String.length model.authData.authToken of
+       0 ->
+           Sub.none

+       _ ->
+           Sub.batch
+               [ Time.every 30000 Tick
+               ]

-- Msg --

type Msg = 
	{- Rest of the messages -}
+	| Tick Time.Posix

update msg model =
	case msg of
		{- Rest -}
+   Tick newTime ->
+       let
+           currentIsoTime =
+               Iso8601.fromTime newTime
+
+           {-
+              Perform an action
+           -}
+       in
+       ( model, Cmd.none )

```

Now let's write the definition of the `updateLastSeen`.

### Imports

```
import Hasura.InputObject
    exposing
        ( Boolean_comparison_exp
        , Integer_comparison_exp
        , Todos_bool_exp
        , Todos_insert_input
        , Todos_order_by
        , Todos_set_input
+       , Users_set_input
        , buildBoolean_comparison_exp
        , buildInteger_comparison_exp
        , buildTodos_bool_exp
        , buildTodos_insert_input
        , buildTodos_order_by
        , buildTodos_set_input
+       , buildUsers_bool_exp
+       , buildUsers_set_input
        )
import Hasura.Mutation as Mutation
    exposing
        ( DeleteTodosRequiredArguments
        , InsertTodosRequiredArguments
        , UpdateTodosOptionalArguments
        , UpdateTodosRequiredArguments
+       , UpdateUsersOptionalArguments
+       , UpdateUsersRequiredArguments
        , insert_todos
        )
+import Hasura.Scalar as Timestamptz exposing (Timestamptz(..))
+import Hasura.Object.Users_mutation_response as UsersMutation
```

### Construct GraphQL Mutation

```
deleteAllCompletedItems : SelectionSet (Maybe MutationResponse) RootMutation -> String -> Cmd Msg
deleteAllCompletedItems mutation authToken =
    makeGraphQLMutation
        authToken
        mutation
        (RemoteData.fromResult >> AllCompletedItemsDeleted)

+updateUserSetArg : String -> Users_set_input
+updateUserSetArg timestamp =
+    buildUsers_set_input
+        (\args ->
+            { args
+                | last_seen = Present (Timestamptz timestamp)
+            }
+        )
+
+
+updateLastSeenArgs : String -> UpdateUsersOptionalArguments -> UpdateUsersOptionalArguments
+updateLastSeenArgs timestamp optionalArgs =
+    { optionalArgs
+        | set_ = Present (updateUserSetArg timestamp)
+    }
+
+
+updateLastSeenWhere : UpdateUsersRequiredArguments
+updateLastSeenWhere =
+    UpdateUsersRequiredArguments
+        (buildUsers_bool_exp (\args -> args))
+
+
+selectionUpdateUserLastSeen : SelectionSet MutationResponse Hasura.Object.Users_mutation_response
+selectionUpdateUserLastSeen =
+    SelectionSet.map MutationResponse
+        UsersMutation.affected_rows
+
+
+updateUserLastSeen : String -> SelectionSet (Maybe MutationResponse) RootMutation
+updateUserLastSeen currTime =
+    Mutation.update_users (updateLastSeenArgs currTime) updateLastSeenWhere selectionUpdateUserLastSeen
+
+
+updateLastSeen : String -> SelectionSet (Maybe MutationResponse) RootMutation -> Cmd Msg
+updateLastSeen authToken updateQuery =
+    makeGraphQLMutation
+        authToken
+        updateQuery
+        (RemoteData.fromResult >> UpdateLastSeen)


```

### Add Data Types

Lets add new data types required to perform this operation

```
type alias AllDeleted =
    RemoteData (Graphql.Http.Error (Maybe MutationResponse)) (Maybe MutationResponse)


+type alias UpdateLastSeenResponse =
+    RemoteData (Graphql.Http.Error (Maybe MutationResponse)) (Maybe MutationResponse)
```

### Add new Msg type

Lets add `Msg` types required to perform this operation

```
type Msg
    = EnteredEmail String
    | EnteredPassword String
    | EnteredUsername String
    | MakeLoginRequest
    | MakeSignupRequest
    | ToggleAuthForm DisplayForm
    | GotLoginResponse LoginResponseParser
    | GotSignupResponse SignupResponseParser
    | ClearAuthToken
    | FetchPrivateDataSuccess TodoData
    | InsertPrivateTodo
    | UpdateNewTodo String
    | InsertPrivateTodoResponse (GraphQLResponse MaybeMutationResponse)
   	| MarkCompleted Int Bool
   	| UpdateTodo UpdateTodoItemResponse
    | DelTodo Int
    | TodoDeleted DeleteTodo
    | AllCompletedItemsDeleted AllDeleted
    | DeleteAllCompletedItems
+   | Tick Time.Posix
+   | UpdateLastSeen UpdateLastSeenResponse
```

### Handle new Msg types in update

```
        AllCompletedItemsDeleted _ ->
            ( model
            , fetchPrivateTodos model.authData.authToken
            )

+       Tick newTime ->
+           let
+               currentIsoTime =
+                   Iso8601.fromTime newTime

+               updateQuery =
+                   updateUserLastSeen currentIsoTime
+           in
+           ( model
+           , updateLastSeen model.authData.authToken updateQuery
+           )

+       UpdateLastSeen _ ->
+           ( model
+           , Cmd.none
+           )

```

Great! Now the metadata about whether the user is online will be available in the backend. Let's now do the integration to display realtime data of online users.
