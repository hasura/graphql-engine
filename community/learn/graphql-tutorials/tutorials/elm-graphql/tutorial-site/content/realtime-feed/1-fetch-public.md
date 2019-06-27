---
title: "Fetch public todos - subscription"
metaTitle: "Fetch public todos using Subscription | GraphQL Elm Tutorial"
metaDescription: "You will learn how to make use of GraphQL Subscriptions to get notified whenever a new todo comes in React app"
---

import GithubLink from "../../src/GithubLink.js";

Lets setup ports to subscribe to public todo data and fetch the initial list of public todo list the user will see after successfull login.

Open `src/Main.elm` and add the following code:

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/elm-graphql/app-final/src/Main.elm" text="src/Main.elm" />

```

port createSubscriptionToOnlineUsers : ( String, String ) -> Cmd msg
port gotOnlineUsers : (Json.Decode.Value -> msg) -> Sub msg

+port createSubscriptionToPublicTodos : ( String, String ) -> Cmd msg
+
+port gotRecentPublicTodoItem : (Json.Decode.Value -> msg) -> Sub msg
```

We are creating two more ports to query latest public todo.


### Construct GraphQL Subscription

```
onlineUsersSelection : SelectionSet OnlineUser Hasura.Object.Online_users
onlineUsersSelection =
    SelectionSet.map2 OnlineUser
        OnlineUser.id
        (OnlineUser.user selectUser)


+publicTodoListSubscriptionOptionalArgument : TodosOptionalArguments -> TodosOptionalArguments
+publicTodoListSubscriptionOptionalArgument optionalArgs =
+    { optionalArgs | where_ = whereIsPublic True, order_by = orderByCreatedAt Desc, limit = OptionalArgument.Present 1 }
+
+
+publicListSubscription : SelectionSet Todos RootSubscription
+publicListSubscription =
+    Subscription.todos publicTodoListSubscriptionOptionalArgument todoListSelection


+publicTodoListQueryLimit : Int -> OptionalArgument Int
+publicTodoListQueryLimit limit =
+    Present limit
+
+
+lteLastTodoId : Int -> OptionalArgument Integer_comparison_exp
+lteLastTodoId id =
+    Present
+        (buildInteger_comparison_exp
+            (\args ->
+                { args
+                    | lte_ = Present id
+                }
+            )
+        )
+
+
+publicTodoListOffsetWhere : Int -> OptionalArgument Todos_bool_exp
+publicTodoListOffsetWhere id =
+    Present
+        (buildTodos_bool_exp
+            (\args ->
+                { args
+                    | id = lteLastTodoId id
+                    , is_public = equalToBoolean True
+                }
+            )
+        )
+
+
+publicTodoListQueryOptionalArgs : Int -> Int -> TodosOptionalArguments -> TodosOptionalArguments
+publicTodoListQueryOptionalArgs id limit optionalArgs =
+    { optionalArgs | where_ = publicTodoListOffsetWhere id, order_by = orderByCreatedAt Desc, limit = publicTodoListQueryLimit limit }
+
+
+todoListSelectionWithUser : SelectionSet Todo Hasura.Object.Todos
+todoListSelectionWithUser =
+    SelectionSet.map5 Todo
+        Todos.id
+        Todos.user_id
+        Todos.is_completed
+        Todos.title
+        (Todos.user selectUser)
+
+
+loadPublicTodoList : Int -> SelectionSet Todos RootQuery
+loadPublicTodoList id =
+    Query.todos (publicTodoListQueryOptionalArgs id 7) todoListSelectionWithUser
+
+
+makeRequest : SelectionSet Todos RootQuery -> String -> Cmd Msg
+makeRequest query authToken =
+    makeGraphQLQuery
+        authToken
+        query
+        (RemoteData.fromResult >> FetchPublicDataSuccess)

```

### Add/Update Data Types

```


initialize : Model
initialize =
    { privateData = initializePrivateTodo
    , online_users = RemoteData.NotAsked
    , publicTodoInsert = ""
-   , publicTodoInfo = PublicTodoData getPublicTodos 0 1 0 True
+   , publicTodoInfo = PublicTodoData [] 0 0 0 True
    , authData = AuthData "" "" "" ""
    , authForm = AuthForm Login False False ""
    }

getInitialEvent : String -> Cmd Msg
getInitialEvent authToken =
    Cmd.batch
        [ fetchPrivateTodos authToken
+       , createSubscriptionToPublicTodos ( publicListSubscription |> Graphql.Document.serializeSubscription, authToken )
        , createSubscriptionToOnlineUsers ( onlineUsersSubscription |> Graphql.Document.serializeSubscription, authToken )
        ]

subscriptions : Model -> Sub Msg
subscriptions model =
    case String.length model.authData.authToken of
        0 ->
            Sub.none

        _ ->
            Sub.batch
+               [ gotRecentPublicTodoItem RecentPublicTodoReceived
                , gotOnlineUsers GotOnlineUsers
                , Time.every 30000 Tick
                ]

+updatePublicTodoData : (PublicTodoData -> PublicTodoData) -> Model -> Cmd Msg -> ( Model, Cmd Msg )
+updatePublicTodoData transform model cmd =
+    ( { model | publicTodoInfo = transform model.publicTodoInfo }, cmd )

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
    | GotOnlineUsers Json.Decode.Value
+   | RecentPublicTodoReceived Json.Decode.Value
+		| FetchPublicDataSuccess PublicDataFetched
```


### Handle new Msg types in update

```
        UpdateLastSeen _ ->
            ( model
            , Cmd.none
            )

        GotOnlineUsers data ->
            let
                remoteData =
                    Json.Decode.decodeValue (onlineUsersSubscription |> Graphql.Document.decoder) data |> RemoteData.fromResult
            in
            ( { model | online_users = remoteData }, Cmd.none )

+       RecentPublicTodoReceived data ->
+           let
+               remoteData =
+                   Json.Decode.decodeValue (publicListSubscription |> Graphql.Document.decoder) data |> RemoteData.fromResult
+           in
+           case remoteData of
+               RemoteData.Success recentData ->
+                   case List.length recentData > 0 of
+                       True ->
+                           case Array.get 0 (Array.fromList recentData) of
+                               Just recDat ->
+                                   case model.publicTodoInfo.oldestTodoId of
+                                       0 ->
+                                           let
+                                               queryObj =
+                                                   loadPublicTodoList recDat.id
+                                           in
+                                           updatePublicTodoData (\publicTodoInfo -> { publicTodoInfo | currentLastTodoId = recDat.id }) model (makeRequest queryObj model.authData.authToken)

+                                       _ ->
+                                           let
+                                               updatedNewTodoCount =
+                                                   model.publicTodoInfo.newTodoCount + 1
+                                           in
+                                           case model.publicTodoInfo.currentLastTodoId >= recDat.id of
+                                               True ->
+                                                   ( model, Cmd.none )

+                                               False ->
+                                                   updatePublicTodoData (\publicTodoInfo -> { publicTodoInfo | newTodoCount = updatedNewTodoCount }) model Cmd.none

+                               Nothing ->
+                                   ( model, Cmd.none )

+                       False ->
+                           ( model, Cmd.none )

+               _ ->
+                   ( model, Cmd.none )


+        FetchPublicDataSuccess response ->
+            case response of
+                RemoteData.Success successData ->
+                    case List.length successData of
+                        0 ->
+                            ( model, Cmd.none )
+
+                        _ ->
+                            let
+                                oldestTodo =
+                                    Array.get 0 (Array.fromList (List.foldl (::) [] successData))
+                            in
+                            case oldestTodo of
+                                Just item ->
+                                    updatePublicTodoData (\publicTodoInfo -> { publicTodoInfo | todos = successData, oldestTodoId = item.id }) model Cmd.none
+
+                                Nothing ->
+                                    ( model, Cmd.none )
+
+                _ ->
+                    ( model, Cmd.none )

```

Now subscribe to the most recent todo over ports

Open `src/index.js` and add the following code:

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/elm-graphql/app-final/src/index.js" text="src/index.js" />

```

document.addEventListener("DOMContentLoaded", function() {
  var app = Elm.Main.init({
    node: document.getElementById("root")
  });
+ app.ports.createSubscriptionToPublicTodos.subscribe(function(data) {
+   /* Initiate subscription request */
+   var [ data, authToken ] = data;
+   if (authToken.length > 0) {
+     // app.ports.creatingSubscriptionToTasks.send(1);
+     getClient(authToken).subscribe({
+       query: gql`${data}`,
+       variables: {}
+     }).subscribe({
+       next(resp) {
+         app.ports.gotRecentPublicTodoItem.send(resp);
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

### Remove dummy values

```
+ seedIds : List Int
+ seedIds =
+     [ 1, 2 ]
+ 
+ 
+ publicSeedIds : List Int
+ publicSeedIds =
+     [ 1, 2, 3, 4 ]
+ 
+ 
+ todoPublicPlaceholder : String
+ todoPublicPlaceholder =
+     "This is public todo"
+ 
+ 
+ generateUser : Int -> User
+ generateUser id =
+     User ("someUser" ++ String.fromInt id)
+ 
+ 
+ generatePublicTodo : String -> Int -> Todo
+ generatePublicTodo placeholder id =
+     Todo id ("User" ++ String.fromInt id) False (placeholder ++ " " ++ String.fromInt id) (generateUser id)
+ 
+ 
+ getPublicTodos : Todos
+ getPublicTodos =
+     List.map (generatePublicTodo todoPublicPlaceholder) publicSeedIds

```

Awesome! You have successfully subscribed to new public todos.
