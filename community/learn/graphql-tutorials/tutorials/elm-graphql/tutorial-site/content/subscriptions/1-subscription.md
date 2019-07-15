---
title: "Subscription"
metaTitle: "Set up GraphQL Subscriptions using Apollo Client | GraphQL Elm Tutorial"
metaDescription: "You will learn how to configure GraphQL Subscriptions using Apollo Client by installing dependencies like apollo-link-ws, subscriptions-transport-ws. This will also have authorization token setup"
---

import GithubLink from "../../src/GithubLink.js";

Lets use the ports defined in the previous step to open a subscription client


### Imports

Open `src/Main.elm` and add the following code:

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/elm-graphql/app-final/src/Main.elm" text="src/Main.elm" />

```
+import Graphql.Document
import Graphql.Http
- import Graphql.Operation exposing (RootMutation, RootQuery)
+import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
+import Hasura.Object.Online_users as OnlineUser
+import Hasura.Subscription as Subscription
```

### Construct GraphQL Subscription

```
updateLastSeen : String -> SelectionSet (Maybe MutationResponse) RootMutation -> Cmd Msg
updateLastSeen authToken updateQuery =
    makeGraphQLMutation
        authToken
        updateQuery
        (RemoteData.fromResult >> UpdateLastSeen)

+onlineUsersSubscription : SelectionSet OnlineUsers RootSubscription
+onlineUsersSubscription =
+    Subscription.online_users identity onlineUsersSelection
+
+
+onlineUsersSelection : SelectionSet OnlineUser Hasura.Object.Online_users
+onlineUsersSelection =
+    SelectionSet.map2 OnlineUser
+        OnlineUser.id
+        (OnlineUser.user selectUser)

```

### Add/Update Data Types

Configure subscriptions and update `getInitialEvent` to initiate a subscription connection via ports. The subscription in this case will remain active as long as apollo client connection is live.

Here is what it is doing:
  1) Initiate a subscription a connection on page load/successfull login
  2) Configure elm subscription to listen to any changes from the javascript side.

```

- type alias OnlineUser =
-     { id : String
-     , user : User
-     }

+type alias OnlineUser =
+    { id : Maybe String
+    , user : Maybe User
+    }

+type alias OnlineUsersData =
+    RemoteData Json.Decode.Error OnlineUsers


type alias Model =
    { privateData : PrivateTodo
    , publicTodoInsert : String
    , publicTodoInfo : PublicTodoData
-   , online_users : OnlineUsers
+   , online_users : OnlineUsersData
    , authData : AuthData
    , authForm : AuthForm
    }


- generateOnlineUser : Int -> OnlineUser
- generateOnlineUser id =
-     OnlineUser (String.fromInt id) (generateUser id)
- 
- 
- getOnlineUsers : OnlineUsers
- getOnlineUsers =
-     List.map generateOnlineUser seedIds


initialize : Model
initialize =
    { privateData = initializePrivateTodo
-   , online_users = getOnlineUsers
+   , online_users = RemoteData.NotAsked
    , publicTodoInsert = ""
    , publicTodoInfo = PublicTodoData getPublicTodos 0 1 0 True
    , authData = AuthData "" "" "" ""
    , authForm = AuthForm Login False False ""
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    case String.length model.authData.authToken of
        0 ->
            Sub.none

        _ ->
            Sub.batch
+               [ gotOnlineUsers GotOnlineUsers
                , Time.every 30000 Tick
                ]


getInitialEvent : String -> Cmd Msg
getInitialEvent authToken =
    Cmd.batch
        [ fetchPrivateTodos authToken
+       , createSubscriptionToOnlineUsers ( onlineUsersSubscription |> Graphql.Document.serializeSubscription, authToken )
        ]



```

### Add new Msg type

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
    | Tick Time.Posix
    | UpdateLastSeen UpdateLastSeenResponse
+   | GotOnlineUsers Json.Decode.Value
```


### Handle new Msg types in update

```
        UpdateLastSeen _ ->
            ( model
            , Cmd.none
            )

+       GotOnlineUsers data ->
+           let
+               remoteData =
+                   Json.Decode.decodeValue (onlineUsersSubscription |> Graphql.Document.decoder) data |> RemoteData.fromResult
+           in
+           ( { model | online_users = remoteData }, Cmd.none )


```

### Update render functions

```

- generateOnlineUsersList : OnlineUsers -> List (Html msg)        
- generateOnlineUsersList onlineUser =
-     List.map viewOnlineUser onlineUser

+generateOnlineUsersList : OnlineUsersData -> List (Html msg)
+generateOnlineUsersList onlineUser =
+    case onlineUser of
+        RemoteData.Success d ->
+            List.map viewOnlineUser d
+
+        _ ->
+            [ text "" ]	


- getOnlineUsersCount : OnlineUsers -> Int
- getOnlineUsersCount onlineUsers =
-     List.length onlineUsers

+getOnlineUsersCount : OnlineUsersData -> Int
+getOnlineUsersCount onlineUsers =
+    case onlineUsers of
+        RemoteData.Success data ->
+            List.length data
+
+        _ ->
+            0

- viewOnlineUser : OnlineUser -> Html msg
- viewOnlineUser onlineUser =
-     viewUserName onlineUser.user.name

+viewOnlineUser : OnlineUser -> Html msg
+viewOnlineUser onlineUser =
+    case onlineUser.user of
+        Just user ->
+            viewUserName user.name
+
+        Nothing ->
+            text ""

```

Add port integration to index.js

Open `src/index.js` and add the following code:

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/elm-graphql/app-final/src/index.js" text="src/index.js" />

```
document.addEventListener("DOMContentLoaded", function() {
  var app = Elm.Main.init({
    node: document.getElementById("root")
  });
+ app.ports.createSubscriptionToOnlineUsers.subscribe(function(data) {
+   /* Initiate subscription request */
+   var [ data, authToken ] = data;
+   if (authToken.length > 0) {
+     getClient(authToken).subscribe({
+       query: gql`${data}`,
+       variables: {}
+     }).subscribe({
+       next(resp) {
+         app.ports.gotOnlineUsers.send(resp);
+       },
+       error(err) {
+         console.log('error is');
+         console.log(err);
+       }
+     });
+   }
+ });
})
```

Awesome! You have completed basic implementations of a GraphQL Query, Mutation and Subscriptions. Easy isn't it?
