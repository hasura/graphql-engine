---
title: "Delete bulk todos - mutation"
metaTitle: "Mutation to delete bulk todos | GraphQL Elm Apollo Tutorial"
metaDescription: "GraphQL Mutation to delete bulk personal todos. Try the mutation in GraphiQL, passing the Authorization token to get authenticated results."
---

In this part of the tutorial, you will learn how to delete existing todos by using GraphQL Mutations.

Let's define a graphql mutation to perform bulk delete of todos.

```graphql
mutation clearCompleted {
  delete_todos(where: {is_completed: {_eq: true}, is_public: {_eq: false}}) {
    affected_rows
  }
}
```

You will also need to pass in the values for the variables.

[Try](https://learn.hasura.io/graphql/graphiql?tutorial=react-native) this mutation in GraphiQL against the application database to see what the response looks like.

Let's now integrate this graphql mutation into our elm app.

We will construct a GraphQL Mutation to update a private todo and integrate it with the elm app.

### Construct GraphQL Mutation

We will be constructing a GraphQL mutation as above

```
deleteSingleTodoItem : SelectionSet (Maybe MutationResponse) RootMutation -> String -> Cmd Msg
deleteSingleTodoItem mutation authToken =
    makeGraphQLMutation
        authToken
        mutation
        (RemoteData.fromResult >> TodoDeleted)

+deleteAllCompletedTodo : SelectionSet (Maybe MutationResponse) RootMutation
+deleteAllCompletedTodo =
+    Mutation.delete_todos (setTodoListDeleteAllCompletedWhere True) mutationResponseSelection
+
+
+setTodoListValueForTodoStatus : Bool -> Boolean_comparison_exp
+setTodoListValueForTodoStatus status =
+    buildBoolean_comparison_exp
+        (\args ->
+            { args
+                | eq_ = Present status
+            }
+        )
+
+
+setTodoListDeleteAllCompletedWhere : Bool -> DeleteTodosRequiredArguments
+setTodoListDeleteAllCompletedWhere status =
+    DeleteTodosRequiredArguments
+        (buildTodos_bool_exp
+            (\args ->
+                { args
+                    | is_completed = Present (setTodoListValueForTodoStatus status)
+                }
+            )
+        )
+
+
+delAllResponseSelection : SelectionSet MutationResponse Hasura.Object.Todos_mutation_response
+delAllResponseSelection =
+    SelectionSet.map MutationResponse
+        TodosMutation.affected_rows
+
+
+deleteAllCompletedItems : SelectionSet (Maybe MutationResponse) RootMutation -> String -> Cmd Msg
+deleteAllCompletedItems mutation authToken =
+    makeGraphQLMutation
+        authToken
+        mutation
+        (RemoteData.fromResult >> AllCompletedItemsDeleted)

```

### Add Data Types

Lets add new data types required to perform this operation

```

type alias DeleteTodo =
    RemoteData (Graphql.Http.Error (Maybe MutationResponse)) (Maybe MutationResponse)

+type alias AllDeleted =
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
+   | AllCompletedItemsDeleted AllDeleted
+   | DeleteAllCompletedItems
```


### Handle new Msg types in update

```
        TodoDeleted _ ->
            ( model
            , fetchPrivateTodos model.authData.authToken
            )
+       DeleteAllCompletedItems ->
+           ( model, deleteAllCompletedItems deleteAllCompletedTodo model.authData.authToken )
+
+       AllCompletedItemsDeleted _ ->
+           ( model
+           , fetchPrivateTodos model.authData.authToken
+           )


```


### Update render functions


```
clearButton : Html Msg
clearButton =
-   button [ class "clearComp" ]
+   button [ class "clearComp", onClick DeleteAllCompletedItems ]
        [ text "Clear completed"
        ]
```
