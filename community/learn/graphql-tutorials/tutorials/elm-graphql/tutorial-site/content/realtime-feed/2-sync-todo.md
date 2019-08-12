---
title: "Sync new todos"
metaTitle: "Sync new todos in public feed | GraphQL Elm Tutorial"
metaDescription: "You will learn how to sync new todos added by other people in the public feed by fetching older and newer data using GraphQL Queries"
---

Once a new todo is entered in a public list, it needs to appear in the UI. Instead of automatically displaying the todo in the UI, we use a Feed like Notification banner which appears whenever a new todo is received.

Remember that previously we have subscribed to a query which was fetching the most recent public todo added. We then fetch the initial list using the most recent public todo id.

Lets add functionality to the loadMoreSections

### Construct GraphQL Queries

Lets construct the GraphQL query for the above two operations

```
makeRequest : SelectionSet Todos RootQuery -> String -> Cmd Msg
makeRequest query authToken =
    makeGraphQLQuery
        authToken
        query
        (RemoteData.fromResult >> FetchPublicDataSuccess)

+gtLastTodoId : Int -> OptionalArgument Integer_comparison_exp
+gtLastTodoId id =
+    Present
+        (buildInteger_comparison_exp
+            (\args ->
+                { args
+                    | gt_ = Present id
+                }
+            )
+        )
+
+
+newPublicTodosWhere : Int -> OptionalArgument Todos_bool_exp
+newPublicTodosWhere id =
+    Present
+        (buildTodos_bool_exp
+            (\args ->
+                { args
+                    | id = gtLastTodoId id
+                    , is_public = equalToBoolean True
+                }
+            )
+        )
+
+
+
+{-
+   Generates argument as below
+   ```
+    order_by : [
+      {
+        created_at: desc
+      }
+    ],
+     where_ : {
+       id: {
+         _gt: <id>
+       },
+       is_public: {
+         _eq: True
+       }
+     }
+   ```
+-}
+
+
+newPublicTodoListQueryOptionalArgs : Int -> TodosOptionalArguments -> TodosOptionalArguments
+newPublicTodoListQueryOptionalArgs id optionalArgs =
+    { optionalArgs | where_ = newPublicTodosWhere id, order_by = orderByCreatedAt Desc }
+
+
+newTodoQuery : Int -> SelectionSet Todos RootQuery
+newTodoQuery id =
+    Query.todos (newPublicTodoListQueryOptionalArgs id) todoListSelectionWithUser
+
+
+loadNewTodos : SelectionSet Todos RootQuery -> String -> Cmd Msg
+loadNewTodos q authToken =
+    makeGraphQLQuery authToken q (RemoteData.fromResult >> FetchNewTodoDataSuccess)
+
+
+ltLastTodoId : Int -> OptionalArgument Integer_comparison_exp
+ltLastTodoId id =
+    Present
+        (buildInteger_comparison_exp
+            (\args ->
+                { args
+                    | lt_ = Present id
+                }
+            )
+        )
+
+
+oldPublicTodosWhere : Int -> OptionalArgument Todos_bool_exp
+oldPublicTodosWhere id =
+    Present
+        (buildTodos_bool_exp
+            (\args ->
+                { args
+                    | id = ltLastTodoId id
+                    , is_public = equalToBoolean True
+                }
+            )
+        )
+
+
+oldPublicTodoListQueryOptionalArgs : Int -> TodosOptionalArguments -> TodosOptionalArguments
+oldPublicTodoListQueryOptionalArgs id optionalArgs =
+    { optionalArgs | where_ = oldPublicTodosWhere id, order_by = orderByCreatedAt Desc, limit = OptionalArgument.Present 7 }
+
+
+oldTodoQuery : Int -> SelectionSet Todos RootQuery
+oldTodoQuery id =
+    Query.todos (oldPublicTodoListQueryOptionalArgs id) todoListSelectionWithUser
+
+
+loadOldTodos : SelectionSet Todos RootQuery -> String -> Cmd Msg
+loadOldTodos q authToken =
+    makeGraphQLQuery authToken q (RemoteData.fromResult >> FetchOldTodoDataSuccess)


```

### Add new Msg type

Lets add new `Msg` types which will be called the `loadNew` and `loadOld` buttons are clicked

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
    | RecentPublicTodoReceived Json.Decode.Value
 		| FetchPublicDataSuccess PublicDataFetched
+   | FetchNewTodoDataSuccess PublicDataFetched
+   | FetchOldTodoDataSuccess PublicDataFetched
+   | FetchNewPublicTodos
+   | FetchOldPublicTodos
```


### Handle new Msg types in update

Lets add it to our update function to update the models appropriately


```

+       FetchNewPublicTodos ->
+           let
+               newQuery =
+                   newTodoQuery model.publicTodoInfo.currentLastTodoId
+           in
+           ( model, loadNewTodos newQuery model.authData.authToken )

+       FetchOldPublicTodos ->
+           let
+               oldQuery =
+                   oldTodoQuery model.publicTodoInfo.oldestTodoId
+           in
+           ( model, loadOldTodos oldQuery model.authData.authToken )

+       FetchOldTodoDataSuccess response ->
+           case response of
+               RemoteData.Success successData ->
+                   case List.length successData of
+                       0 ->
+                           updatePublicTodoData
+                               (\publicTodoInfo -> { publicTodoInfo | oldTodosAvailable = False })
+                               model
+                               Cmd.none

+                       _ ->
+                           let
+                               oldestTodo =
+                                   Array.get 0 (Array.fromList (List.foldl (::) [] successData))
+                           in
+                           case oldestTodo of
+                               Just item ->
+                                   updatePublicTodoData (\publicTodoInfo -> { publicTodoInfo | todos = List.append publicTodoInfo.todos successData, oldestTodoId = item.id }) model Cmd.none

+                               Nothing ->
+                                   ( model, Cmd.none )

+               _ ->
+                   ( model, Cmd.none )

+       FetchNewTodoDataSuccess response ->
+           case response of
+               RemoteData.Success successData ->
+                   case List.length successData of
+                       0 ->
+                           ( model, Cmd.none )

+                       _ ->
+                           let
+                               newestTodo =
+                                   Array.get 0 (Array.fromList successData)
+                           in
+                           case newestTodo of
+                               Just item ->
+                                   updatePublicTodoData (\publicTodoInfo -> { publicTodoInfo | todos = List.append successData publicTodoInfo.todos, currentLastTodoId = item.id, newTodoCount = 0 }) model Cmd.none

+                               Nothing ->
+                                   ( model, Cmd.none )

+               _ ->
+                   ( model, Cmd.none )



```

### Update loadLatestPublicTodo and loadOldPublicTodos

Lets update our render functions to invoke relevant actions on click

```
loadLatestPublicTodo : Int -> Html Msg
loadLatestPublicTodo count =
    case count of
        0 ->
            nothing

        _ ->
-           div [ class "loadMoreSection" ]
+           div [ class "loadMoreSection", onClick FetchNewPublicTodos ]
                [ text ("New tasks have arrived! (" ++ String.fromInt count ++ ")")
                ]


loadOldPublicTodos : Bool -> Html Msg
loadOldPublicTodos oldTodosAvailable =
    case oldTodosAvailable of
        True ->
-           div [ class "loadMoreSection" ]
+           div [ class "loadMoreSection", onClick FetchOldPublicTodos  ]
                [ text "Load older tasks"
                ]

        False ->
            div [ class "loadMoreSection" ]
                [ text "No more public tasks!"
                ]
```
