port module Main exposing (main)

{-
   Graphql-elm imports
-}

import Array
import Browser
import GraphQLClient exposing (makeGraphQLMutation, makeGraphQLQuery)
import Graphql.Document
import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Hasura.Enum.Order_by exposing (Order_by(..))
import Hasura.InputObject
    exposing
        ( Boolean_comparison_exp
        , Integer_comparison_exp
        , Todos_bool_exp
        , Todos_insert_input
        , Todos_order_by
        , Todos_set_input
        , Users_set_input
        , buildBoolean_comparison_exp
        , buildInteger_comparison_exp
        , buildTodos_bool_exp
        , buildTodos_insert_input
        , buildTodos_order_by
        , buildTodos_set_input
        , buildUsers_bool_exp
        , buildUsers_set_input
        )
import Hasura.Mutation as Mutation
    exposing
        ( DeleteTodosRequiredArguments
        , InsertTodosRequiredArguments
        , UpdateTodosOptionalArguments
        , UpdateTodosRequiredArguments
        , UpdateUsersOptionalArguments
        , UpdateUsersRequiredArguments
        , insert_todos
        )
import Hasura.Object
import Hasura.Object.Online_users as OnlineUser
import Hasura.Object.Todos as Todos
import Hasura.Object.Todos_mutation_response as TodosMutation
import Hasura.Object.Users as Users
import Hasura.Object.Users_mutation_response as UsersMutation
import Hasura.Query as Query exposing (TodosOptionalArguments)
import Hasura.Scalar as Timestamptz exposing (Timestamptz(..))
import Hasura.Subscription as Subscription
import Html exposing (Html, a, button, div, form, h1, i, img, input, label, li, nav, p, span, text, ul)
import Html.Attributes
    exposing
        ( checked
        , class
        , classList
        , disabled
        , for
        , href
        , id
        , placeholder
        , title
        , type_
        , value
        )
import Html.Events
    exposing
        ( onClick
        , onInput
        , onSubmit
        )
import Html.Keyed as Keyed
import Http
import Iso8601
import Json.Decode exposing (Decoder, field, int, string)
import Json.Encode as Encode
import RemoteData exposing (RemoteData)
import Time



{-
   Constants
-}


failMsg : String
failMsg =
    "Something went wrong, check network tab for details"


signup_url : String
signup_url =
    "https://learn.hasura.io/auth/signup"


login_url : String
login_url =
    "https://learn.hasura.io/auth/login"



{- -}
---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }



---- Ports ----


port createSubscriptionToOnlineUsers : ( String, String ) -> Cmd msg


port gotOnlineUsers : (Json.Decode.Value -> msg) -> Sub msg


port createSubscriptionToPublicTodos : ( String, String ) -> Cmd msg


port gotRecentPublicTodoItem : (Json.Decode.Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    case String.length model.authData.authToken of
        0 ->
            Sub.none

        _ ->
            Sub.batch
                [ gotRecentPublicTodoItem RecentPublicTodoReceived
                , gotOnlineUsers GotOnlineUsers
                , Time.every 30000 Tick
                ]



---- MODEL ----


type alias User =
    { name : String
    }


type alias Todo =
    { id : Int
    , user_id : String
    , is_completed : Bool
    , title : String
    , user : User
    }


type alias Todos =
    List Todo


type alias OnlineUsers =
    List OnlineUser


type alias OnlineUser =
    { id : Maybe String
    , user : Maybe User
    }


type alias MutationResponse =
    { affected_rows : Int
    }


type alias MaybeMutationResponse =
    Maybe MutationResponse


type GraphQLResponse decodesTo
    = GraphQLResponse (RemoteData (Graphql.Http.Error decodesTo) decodesTo)


type alias PrivateTodo =
    { todos : TodoData
    , visibility : String
    , newTodo : String
    , mutateTodo : GraphQLResponse MaybeMutationResponse
    }


type alias PublicTodoData =
    { todos : Todos
    , oldestTodoId : Int
    , newTodoCount : Int
    , currentLastTodoId : Int
    , oldTodosAvailable : Bool
    }


type Operation
    = NotYetInitiated
    | OnGoing
    | OperationFailed String



{-
   Login and Signup models
-}


type alias AuthData =
    { email : String
    , password : String
    , username : String
    , authToken : String
    }


type alias AuthForm =
    { displayForm : DisplayForm
    , isRequestInProgress : Bool
    , isSignupSuccess : Bool
    , requestError : String
    }


type alias LoginResponseParser =
    RemoteData Http.Error LoginResponseData


type alias LoginResponseData =
    { token : String }


type alias SignupResponseParser =
    RemoteData Http.Error SignupResponseData


type alias SignupResponseData =
    { id : String }


type DisplayForm
    = Login
    | Signup


type alias TodoData =
    RemoteData (Graphql.Http.Error Todos) Todos


type alias UpdateTodoItemResponse =
    RemoteData (Graphql.Http.Error (Maybe MutationResponse)) (Maybe MutationResponse)


type alias DeleteTodo =
    RemoteData (Graphql.Http.Error (Maybe MutationResponse)) (Maybe MutationResponse)


type alias AllDeleted =
    RemoteData (Graphql.Http.Error (Maybe MutationResponse)) (Maybe MutationResponse)


type alias UpdateLastSeenResponse =
    RemoteData (Graphql.Http.Error (Maybe MutationResponse)) (Maybe MutationResponse)


type alias OnlineUsersData =
    RemoteData Json.Decode.Error OnlineUsers


type alias PublicDataFetched =
    RemoteData (Graphql.Http.Error Todos) Todos


type alias Model =
    { privateData : PrivateTodo
    , publicTodoInsert : String
    , publicTodoInfo : PublicTodoData
    , online_users : OnlineUsersData
    , authData : AuthData
    , authForm : AuthForm
    , publicTodoInsertStatus : Operation
    , publicTodoLoadingStatus : Bool
    }


initializePrivateTodo : PrivateTodo
initializePrivateTodo =
    { todos = RemoteData.Loading
    , visibility = "All"
    , newTodo = ""
    , mutateTodo = GraphQLResponse RemoteData.NotAsked
    }


initialize : Model
initialize =
    { privateData = initializePrivateTodo
    , online_users = RemoteData.NotAsked
    , publicTodoInsert = ""
    , publicTodoInfo = PublicTodoData [] 0 0 0 True
    , authData = AuthData "" "" "" ""
    , authForm = AuthForm Login False False ""
    , publicTodoInsertStatus = NotYetInitiated
    , publicTodoLoadingStatus = False
    }


getInitialEvent : String -> Cmd Msg
getInitialEvent authToken =
    Cmd.batch
        [ fetchPrivateTodos authToken
        , createSubscriptionToPublicTodos ( publicListSubscription |> Graphql.Document.serializeSubscription, authToken )
        , createSubscriptionToOnlineUsers ( onlineUsersSubscription |> Graphql.Document.serializeSubscription, authToken )
        ]


init : ( Model, Cmd Msg )
init =
    ( initialize
    , Cmd.none
    )



---- Application logic and variables ----


orderByCreatedAt : Order_by -> OptionalArgument (List Todos_order_by)
orderByCreatedAt order =
    Present <| [ buildTodos_order_by (\args -> { args | created_at = OptionalArgument.Present order }) ]


equalToBoolean : Bool -> OptionalArgument Boolean_comparison_exp
equalToBoolean isPublic =
    Present <| buildBoolean_comparison_exp (\args -> { args | eq_ = OptionalArgument.Present isPublic })


whereIsPublic : Bool -> OptionalArgument Todos_bool_exp
whereIsPublic isPublic =
    Present <| buildTodos_bool_exp (\args -> { args | is_public = equalToBoolean isPublic })


todoListOptionalArgument : TodosOptionalArguments -> TodosOptionalArguments
todoListOptionalArgument optionalArgs =
    { optionalArgs | where_ = whereIsPublic False, order_by = orderByCreatedAt Desc }


selectUser : SelectionSet User Hasura.Object.Users
selectUser =
    SelectionSet.map User
        Users.name


todoListSelection : SelectionSet Todo Hasura.Object.Todos
todoListSelection =
    SelectionSet.map5 Todo
        Todos.id
        Todos.user_id
        Todos.is_completed
        Todos.title
        (Todos.user selectUser)


fetchPrivateTodosQuery : SelectionSet Todos RootQuery
fetchPrivateTodosQuery =
    Query.todos todoListOptionalArgument todoListSelection


fetchPrivateTodos : String -> Cmd Msg
fetchPrivateTodos authToken =
    makeGraphQLQuery authToken
        fetchPrivateTodosQuery
        (RemoteData.fromResult >> FetchPrivateDataSuccess)



{-
   Insert private todo
-}


insertTodoObjects : String -> Bool -> Todos_insert_input
insertTodoObjects newTodo isPublic =
    buildTodos_insert_input
        (\args ->
            { args
                | title = Present newTodo
                , is_public = Present isPublic
            }
        )


insertArgs : String -> Bool -> InsertTodosRequiredArguments
insertArgs newTodo isPublic =
    InsertTodosRequiredArguments [ insertTodoObjects newTodo isPublic ]


getTodoListInsertObject : String -> Bool -> SelectionSet (Maybe MutationResponse) RootMutation
getTodoListInsertObject newTodo isPublic =
    insert_todos identity (insertArgs newTodo isPublic) mutationResponseSelection


mutationResponseSelection : SelectionSet MutationResponse Hasura.Object.Todos_mutation_response
mutationResponseSelection =
    SelectionSet.map MutationResponse
        TodosMutation.affected_rows


makeMutation : SelectionSet (Maybe MutationResponse) RootMutation -> String -> Cmd Msg
makeMutation mutation authToken =
    makeGraphQLMutation authToken mutation (RemoteData.fromResult >> GraphQLResponse >> InsertPrivateTodoResponse)



{-
   Update todo
-}


updateTodoStatus : Int -> Bool -> SelectionSet (Maybe MutationResponse) RootMutation
updateTodoStatus todoId status =
    Mutation.update_todos (setTodoListUpdateArgs status) (setTodoListUpdateWhere todoId) mutationResponseSelection


setTodoListSetArg : Bool -> Todos_set_input
setTodoListSetArg status =
    buildTodos_set_input
        (\args ->
            { args
                | is_completed = OptionalArgument.Present status
            }
        )


setTodoListUpdateArgs : Bool -> UpdateTodosOptionalArguments -> UpdateTodosOptionalArguments
setTodoListUpdateArgs status optionalArgs =
    { optionalArgs
        | set_ = Present (setTodoListSetArg status)
    }


setTodoListValueForId : Int -> Integer_comparison_exp
setTodoListValueForId todoId =
    buildInteger_comparison_exp
        (\args ->
            { args
                | eq_ = Present todoId
            }
        )


setTodoListUpdateWhere : Int -> UpdateTodosRequiredArguments
setTodoListUpdateWhere todoId =
    UpdateTodosRequiredArguments
        (buildTodos_bool_exp
            (\args ->
                { args
                    | id = Present (setTodoListValueForId todoId)
                }
            )
        )


updateTodoList : SelectionSet (Maybe MutationResponse) RootMutation -> String -> Cmd Msg
updateTodoList mutation authToken =
    makeGraphQLMutation
        authToken
        mutation
        (RemoteData.fromResult >> UpdateTodo)



{-
   Delete todo
-}


deleteSingleTodo : Int -> SelectionSet (Maybe MutationResponse) RootMutation
deleteSingleTodo todoId =
    Mutation.delete_todos (setTodoListDeleteWhere todoId) mutationResponseSelection


setTodoListDeleteWhere : Int -> DeleteTodosRequiredArguments
setTodoListDeleteWhere todoId =
    DeleteTodosRequiredArguments
        (buildTodos_bool_exp
            (\args ->
                { args
                    | id = Present (setTodoListValueForId todoId)
                }
            )
        )


delResponseSelection : SelectionSet MutationResponse Hasura.Object.Todos_mutation_response
delResponseSelection =
    SelectionSet.map MutationResponse
        TodosMutation.affected_rows


deleteSingleTodoItem : SelectionSet (Maybe MutationResponse) RootMutation -> String -> Cmd Msg
deleteSingleTodoItem mutation authToken =
    makeGraphQLMutation
        authToken
        mutation
        (RemoteData.fromResult >> TodoDeleted)



{-
   Delete all completed tasks
-}


deleteAllCompletedTodo : SelectionSet (Maybe MutationResponse) RootMutation
deleteAllCompletedTodo =
    Mutation.delete_todos (setTodoListDeleteAllCompletedWhere True) mutationResponseSelection


setTodoListValueForTodoStatus : Bool -> Boolean_comparison_exp
setTodoListValueForTodoStatus status =
    buildBoolean_comparison_exp
        (\args ->
            { args
                | eq_ = Present status
            }
        )


setTodoListDeleteAllCompletedWhere : Bool -> DeleteTodosRequiredArguments
setTodoListDeleteAllCompletedWhere status =
    DeleteTodosRequiredArguments
        (buildTodos_bool_exp
            (\args ->
                { args
                    | is_completed = Present (setTodoListValueForTodoStatus status)
                }
            )
        )


delAllResponseSelection : SelectionSet MutationResponse Hasura.Object.Todos_mutation_response
delAllResponseSelection =
    SelectionSet.map MutationResponse
        TodosMutation.affected_rows


deleteAllCompletedItems : SelectionSet (Maybe MutationResponse) RootMutation -> String -> Cmd Msg
deleteAllCompletedItems mutation authToken =
    makeGraphQLMutation
        authToken
        mutation
        (RemoteData.fromResult >> AllCompletedItemsDeleted)



{-
   Update user last_seen
-}


updateUserSetArg : String -> Users_set_input
updateUserSetArg timestamp =
    buildUsers_set_input
        (\args ->
            { args
                | last_seen = Present (Timestamptz timestamp)
            }
        )


updateLastSeenArgs : String -> UpdateUsersOptionalArguments -> UpdateUsersOptionalArguments
updateLastSeenArgs timestamp optionalArgs =
    { optionalArgs
        | set_ = Present (updateUserSetArg timestamp)
    }


updateLastSeenWhere : UpdateUsersRequiredArguments
updateLastSeenWhere =
    UpdateUsersRequiredArguments
        (buildUsers_bool_exp (\args -> args))


selectionUpdateUserLastSeen : SelectionSet MutationResponse Hasura.Object.Users_mutation_response
selectionUpdateUserLastSeen =
    SelectionSet.map MutationResponse
        UsersMutation.affected_rows


updateUserLastSeen : String -> SelectionSet (Maybe MutationResponse) RootMutation
updateUserLastSeen currTime =
    Mutation.update_users (updateLastSeenArgs currTime) updateLastSeenWhere selectionUpdateUserLastSeen


updateLastSeen : String -> SelectionSet (Maybe MutationResponse) RootMutation -> Cmd Msg
updateLastSeen authToken updateQuery =
    makeGraphQLMutation
        authToken
        updateQuery
        (RemoteData.fromResult >> UpdateLastSeen)



{-
   Subscribe to active users
-}


onlineUsersSubscription : SelectionSet OnlineUsers RootSubscription
onlineUsersSubscription =
    Subscription.online_users identity onlineUsersSelection


onlineUsersSelection : SelectionSet OnlineUser Hasura.Object.Online_users
onlineUsersSelection =
    SelectionSet.map2 OnlineUser
        OnlineUser.id
        (OnlineUser.user selectUser)



{-
   Subscription query to fetch recent todos
-}


publicTodoListSubscriptionOptionalArgument : TodosOptionalArguments -> TodosOptionalArguments
publicTodoListSubscriptionOptionalArgument optionalArgs =
    { optionalArgs | where_ = whereIsPublic True, order_by = orderByCreatedAt Desc, limit = OptionalArgument.Present 1 }


publicListSubscription : SelectionSet Todos RootSubscription
publicListSubscription =
    Subscription.todos publicTodoListSubscriptionOptionalArgument todoListSelection



{-
   Generates argument as below
   ```
    limit: <value>,
    order_by : [
      {
        created_at: desc
      }
    ],
     where_ : {
       id: {
         _lte: <id>
       },
       is_public: {
         _eq: True
       }
     }
   ```
-}


publicTodoListQueryLimit : Int -> OptionalArgument Int
publicTodoListQueryLimit limit =
    Present limit


lteLastTodoId : Int -> OptionalArgument Integer_comparison_exp
lteLastTodoId id =
    Present
        (buildInteger_comparison_exp
            (\args ->
                { args
                    | lte_ = Present id
                }
            )
        )


publicTodoListOffsetWhere : Int -> OptionalArgument Todos_bool_exp
publicTodoListOffsetWhere id =
    Present
        (buildTodos_bool_exp
            (\args ->
                { args
                    | id = lteLastTodoId id
                    , is_public = equalToBoolean True
                }
            )
        )


publicTodoListQueryOptionalArgs : Int -> Int -> TodosOptionalArguments -> TodosOptionalArguments
publicTodoListQueryOptionalArgs id limit optionalArgs =
    { optionalArgs | where_ = publicTodoListOffsetWhere id, order_by = orderByCreatedAt Desc, limit = publicTodoListQueryLimit limit }


todoListSelectionWithUser : SelectionSet Todo Hasura.Object.Todos
todoListSelectionWithUser =
    SelectionSet.map5 Todo
        Todos.id
        Todos.user_id
        Todos.is_completed
        Todos.title
        (Todos.user selectUser)


loadPublicTodoList : Int -> SelectionSet Todos RootQuery
loadPublicTodoList id =
    Query.todos (publicTodoListQueryOptionalArgs id 7) todoListSelectionWithUser


makeRequest : SelectionSet Todos RootQuery -> String -> Cmd Msg
makeRequest query authToken =
    makeGraphQLQuery
        authToken
        query
        (RemoteData.fromResult >> FetchPublicDataSuccess)



{-
   Fetching new todos
-}


gtLastTodoId : Int -> OptionalArgument Integer_comparison_exp
gtLastTodoId id =
    Present
        (buildInteger_comparison_exp
            (\args ->
                { args
                    | gt_ = Present id
                }
            )
        )


newPublicTodosWhere : Int -> OptionalArgument Todos_bool_exp
newPublicTodosWhere id =
    Present
        (buildTodos_bool_exp
            (\args ->
                { args
                    | id = gtLastTodoId id
                    , is_public = equalToBoolean True
                }
            )
        )



{-
   Generates argument as below
   ```
    order_by : [
      {
        created_at: desc
      }
    ],
     where_ : {
       id: {
         _gt: <id>
       },
       is_public: {
         _eq: True
       }
     }
   ```
-}


newPublicTodoListQueryOptionalArgs : Int -> TodosOptionalArguments -> TodosOptionalArguments
newPublicTodoListQueryOptionalArgs id optionalArgs =
    { optionalArgs | where_ = newPublicTodosWhere id, order_by = orderByCreatedAt Desc }


newTodoQuery : Int -> SelectionSet Todos RootQuery
newTodoQuery id =
    Query.todos (newPublicTodoListQueryOptionalArgs id) todoListSelectionWithUser


loadNewTodos : SelectionSet Todos RootQuery -> String -> Cmd Msg
loadNewTodos q authToken =
    makeGraphQLQuery authToken q (RemoteData.fromResult >> FetchNewTodoDataSuccess)


ltLastTodoId : Int -> OptionalArgument Integer_comparison_exp
ltLastTodoId id =
    Present
        (buildInteger_comparison_exp
            (\args ->
                { args
                    | lt_ = Present id
                }
            )
        )


oldPublicTodosWhere : Int -> OptionalArgument Todos_bool_exp
oldPublicTodosWhere id =
    Present
        (buildTodos_bool_exp
            (\args ->
                { args
                    | id = ltLastTodoId id
                    , is_public = equalToBoolean True
                }
            )
        )


oldPublicTodoListQueryOptionalArgs : Int -> TodosOptionalArguments -> TodosOptionalArguments
oldPublicTodoListQueryOptionalArgs id optionalArgs =
    { optionalArgs | where_ = oldPublicTodosWhere id, order_by = orderByCreatedAt Desc, limit = OptionalArgument.Present 7 }


oldTodoQuery : Int -> SelectionSet Todos RootQuery
oldTodoQuery id =
    Query.todos (oldPublicTodoListQueryOptionalArgs id) todoListSelectionWithUser


loadOldTodos : SelectionSet Todos RootQuery -> String -> Cmd Msg
loadOldTodos q authToken =
    makeGraphQLQuery authToken q (RemoteData.fromResult >> FetchOldTodoDataSuccess)



{-
   Insert into public todos
-}


getPublicTodoInsertObj : String -> SelectionSet (Maybe MutationResponse) RootMutation
getPublicTodoInsertObj newPublicTodo =
    Mutation.insert_todos identity (insertPublicTodoArgs newPublicTodo) publicTodoMutateResponseSelection


insertPublicTodoObjects : String -> Todos_insert_input
insertPublicTodoObjects newPublicTodo =
    buildTodos_insert_input
        (\args ->
            { args
                | title = Present newPublicTodo
                , is_public = Present True
            }
        )


insertPublicTodoArgs : String -> InsertTodosRequiredArguments
insertPublicTodoArgs newPublicTodo =
    InsertTodosRequiredArguments [ insertPublicTodoObjects newPublicTodo ]


publicTodoMutateResponseSelection : SelectionSet MutationResponse Hasura.Object.Todos_mutation_response
publicTodoMutateResponseSelection =
    SelectionSet.map MutationResponse
        TodosMutation.affected_rows


insertPublicTodo : SelectionSet (Maybe MutationResponse) RootMutation -> String -> Cmd Msg
insertPublicTodo mutation authToken =
    makeGraphQLMutation
        authToken
        mutation
        (RemoteData.fromResult >> GraphQLResponse >> InsertPublicTodoResponse)



---- UPDATE ----


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
    | UpdateVisibility String
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
    | FetchNewTodoDataSuccess PublicDataFetched
    | FetchOldTodoDataSuccess PublicDataFetched
    | FetchNewPublicTodos
    | FetchOldPublicTodos
    | InsertPublicTodo
    | UpdatePublicNewTodo String
    | InsertPublicTodoResponse (GraphQLResponse MaybeMutationResponse)



{-
   Login encoder and decoder
-}


loginDataEncoder : AuthData -> Encode.Value
loginDataEncoder authData =
    Encode.object
        [ ( "username", Encode.string authData.username )
        , ( "password", Encode.string authData.password )
        ]


decodeLogin : Decoder LoginResponseData
decodeLogin =
    Json.Decode.map LoginResponseData
        (field "token" string)



{-
   Signup encoder and decoder
-}


signupDataEncoder : AuthData -> Encode.Value
signupDataEncoder authData =
    Encode.object
        [ ( "username", Encode.string authData.username )
        , ( "password", Encode.string authData.password )
        , ( "confirmPassword", Encode.string authData.password )
        ]


decodeSignup : Decoder SignupResponseData
decodeSignup =
    Json.Decode.map SignupResponseData
        (field "id" string)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClearAuthToken ->
            updateAuthData (\authData -> { authData | authToken = "" }) model Cmd.none

        GotLoginResponse data ->
            case data of
                RemoteData.Success d ->
                    updateAuthAndFormData (\authForm -> { authForm | isRequestInProgress = False, isSignupSuccess = False }) (\authData -> { authData | authToken = d.token }) model (getInitialEvent d.token)

                RemoteData.Failure err ->
                    updateAuthFormData (\authForm -> { authForm | isRequestInProgress = False, requestError = "Unable to authenticate you" }) model Cmd.none

                _ ->
                    ( model, Cmd.none )

        GotSignupResponse data ->
            case data of
                RemoteData.Success d ->
                    updateAuthFormData (\authForm -> { authForm | isRequestInProgress = False, requestError = "", displayForm = Login, isSignupSuccess = True }) model Cmd.none

                RemoteData.Failure err ->
                    updateAuthFormData (\authForm -> { authForm | isRequestInProgress = False, requestError = "Signup failed!" }) model Cmd.none

                _ ->
                    ( model, Cmd.none )

        MakeLoginRequest ->
            let
                loginRequest =
                    Http.post
                        { url = login_url
                        , body = Http.jsonBody (loginDataEncoder model.authData)
                        , expect = Http.expectJson (RemoteData.fromResult >> GotLoginResponse) decodeLogin
                        }
            in
            updateAuthFormData (\authForm -> { authForm | isRequestInProgress = True }) model loginRequest

        MakeSignupRequest ->
            let
                signupRequest =
                    Http.post
                        { url = signup_url
                        , body = Http.jsonBody (signupDataEncoder model.authData)
                        , expect = Http.expectJson (RemoteData.fromResult >> GotSignupResponse) decodeSignup
                        }
            in
            updateAuthFormData (\authForm -> { authForm | isRequestInProgress = True, isSignupSuccess = False }) model signupRequest

        ToggleAuthForm displayForm ->
            updateAuthFormData (\authForm -> { authForm | displayForm = displayForm, isSignupSuccess = False, requestError = "" }) model Cmd.none

        EnteredEmail email ->
            updateAuthData (\authData -> { authData | email = email }) model Cmd.none

        EnteredPassword password ->
            updateAuthData (\authData -> { authData | password = password }) model Cmd.none

        EnteredUsername name ->
            updateAuthData (\authData -> { authData | username = name }) model Cmd.none

        FetchPrivateDataSuccess response ->
            updatePrivateData (\privateData -> { privateData | todos = response }) model Cmd.none

        InsertPrivateTodoResponse response ->
            updatePrivateData (\privateData -> { privateData | mutateTodo = response, newTodo = "" }) model (fetchPrivateTodos model.authData.authToken)

        InsertPrivateTodo ->
            case String.length model.privateData.newTodo of
                0 ->
                    ( model, Cmd.none )

                _ ->
                    let
                        mutationObj =
                            getTodoListInsertObject model.privateData.newTodo False
                    in
                    updatePrivateData (\privateData -> { privateData | mutateTodo = GraphQLResponse RemoteData.Loading }) model (makeMutation mutationObj model.authData.authToken)

        UpdateNewTodo newTodo ->
            updatePrivateData (\privateData -> { privateData | newTodo = newTodo }) model Cmd.none

        UpdateVisibility visibility ->
            updatePrivateData (\privateData -> { privateData | visibility = visibility }) model Cmd.none

        MarkCompleted id completed ->
            let
                updateObj =
                    updateTodoStatus id (not completed)
            in
            ( model, updateTodoList updateObj model.authData.authToken )

        UpdateTodo _ ->
            ( model
            , fetchPrivateTodos model.authData.authToken
            )

        DelTodo id ->
            let
                deleteObj =
                    deleteSingleTodo id
            in
            ( model, deleteSingleTodoItem deleteObj model.authData.authToken )

        TodoDeleted _ ->
            ( model
            , fetchPrivateTodos model.authData.authToken
            )

        DeleteAllCompletedItems ->
            ( model, deleteAllCompletedItems deleteAllCompletedTodo model.authData.authToken )

        AllCompletedItemsDeleted _ ->
            ( model
            , fetchPrivateTodos model.authData.authToken
            )

        Tick newTime ->
            let
                currentIsoTime =
                    Iso8601.fromTime newTime

                updateQuery =
                    updateUserLastSeen currentIsoTime
            in
            ( model
            , updateLastSeen model.authData.authToken updateQuery
            )

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

        RecentPublicTodoReceived data ->
            let
                remoteData =
                    Json.Decode.decodeValue (publicListSubscription |> Graphql.Document.decoder) data |> RemoteData.fromResult
            in
            case remoteData of
                RemoteData.Success recentData ->
                    case List.length recentData > 0 of
                        True ->
                            case Array.get 0 (Array.fromList recentData) of
                                Just recDat ->
                                    case model.publicTodoInfo.oldestTodoId of
                                        0 ->
                                            let
                                                queryObj =
                                                    loadPublicTodoList recDat.id
                                            in
                                            updatePublicTodoData (\publicTodoInfo -> { publicTodoInfo | currentLastTodoId = recDat.id }) model (makeRequest queryObj model.authData.authToken)

                                        _ ->
                                            let
                                                updatedNewTodoCount =
                                                    model.publicTodoInfo.newTodoCount + 1
                                            in
                                            case model.publicTodoInfo.currentLastTodoId >= recDat.id of
                                                True ->
                                                    ( model, Cmd.none )

                                                False ->
                                                    updatePublicTodoData (\publicTodoInfo -> { publicTodoInfo | newTodoCount = updatedNewTodoCount }) model Cmd.none

                                Nothing ->
                                    ( model, Cmd.none )

                        False ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        FetchPublicDataSuccess response ->
            case response of
                RemoteData.Success successData ->
                    case List.length successData of
                        0 ->
                            ( model, Cmd.none )

                        _ ->
                            let
                                oldestTodo =
                                    Array.get 0 (Array.fromList (List.foldl (::) [] successData))
                            in
                            case oldestTodo of
                                Just item ->
                                    updatePublicTodoData (\publicTodoInfo -> { publicTodoInfo | todos = successData, oldestTodoId = item.id }) model Cmd.none

                                Nothing ->
                                    ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        FetchNewPublicTodos ->
            let
                newQuery =
                    newTodoQuery model.publicTodoInfo.currentLastTodoId
            in
            ( model, loadNewTodos newQuery model.authData.authToken )

        FetchOldPublicTodos ->
            let
                oldQuery =
                    oldTodoQuery model.publicTodoInfo.oldestTodoId
            in
            ( model, loadOldTodos oldQuery model.authData.authToken )

        FetchOldTodoDataSuccess response ->
            case response of
                RemoteData.Success successData ->
                    case List.length successData of
                        0 ->
                            updatePublicTodoData
                                (\publicTodoInfo -> { publicTodoInfo | oldTodosAvailable = False })
                                model
                                Cmd.none

                        _ ->
                            let
                                oldestTodo =
                                    Array.get 0 (Array.fromList (List.foldl (::) [] successData))
                            in
                            case oldestTodo of
                                Just item ->
                                    updatePublicTodoData (\publicTodoInfo -> { publicTodoInfo | todos = List.append publicTodoInfo.todos successData, oldestTodoId = item.id }) model Cmd.none

                                Nothing ->
                                    ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        FetchNewTodoDataSuccess response ->
            case response of
                RemoteData.Success successData ->
                    case List.length successData of
                        0 ->
                            ( model, Cmd.none )

                        _ ->
                            let
                                newestTodo =
                                    Array.get 0 (Array.fromList successData)
                            in
                            case newestTodo of
                                Just item ->
                                    updatePublicTodoData (\publicTodoInfo -> { publicTodoInfo | todos = List.append successData publicTodoInfo.todos, currentLastTodoId = item.id, newTodoCount = 0 }) model Cmd.none

                                Nothing ->
                                    ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        InsertPublicTodo ->
            case String.length model.publicTodoInsert of
                0 ->
                    ( model, Cmd.none )

                _ ->
                    let
                        mutationObj =
                            getPublicTodoInsertObj model.publicTodoInsert
                    in
                    ( { model | publicTodoInsertStatus = OnGoing }, insertPublicTodo mutationObj model.authData.authToken )

        UpdatePublicNewTodo newPublicTodo ->
            ( { model | publicTodoInsert = newPublicTodo }, Cmd.none )

        InsertPublicTodoResponse publicTodoInsertResponse ->
            let
                responseBody =
                    retrieveGraphQLResponseBody publicTodoInsertResponse
            in
            case responseBody of
                RemoteData.Success response ->
                    ( { model | publicTodoInsertStatus = NotYetInitiated, publicTodoInsert = "" }, Cmd.none )

                RemoteData.Failure failureResp ->
                    ( { model | publicTodoInsertStatus = OperationFailed failMsg }, Cmd.none )

                RemoteData.NotAsked ->
                    ( model, Cmd.none )

                RemoteData.Loading ->
                    ( model, Cmd.none )



{-
   Helper funcs
-}


retrieveGraphQLResponseBody : GraphQLResponse decodesTo -> RemoteData (Graphql.Http.Error decodesTo) decodesTo
retrieveGraphQLResponseBody (GraphQLResponse value) =
    value


updatePublicTodoData : (PublicTodoData -> PublicTodoData) -> Model -> Cmd Msg -> ( Model, Cmd Msg )
updatePublicTodoData transform model cmd =
    ( { model | publicTodoInfo = transform model.publicTodoInfo }, cmd )


updatePrivateData : (PrivateTodo -> PrivateTodo) -> Model -> Cmd Msg -> ( Model, Cmd Msg )
updatePrivateData transform model cmd =
    ( { model | privateData = transform model.privateData }, cmd )


updateAuthAndFormData : (AuthForm -> AuthForm) -> (AuthData -> AuthData) -> Model -> Cmd Msg -> ( Model, Cmd Msg )
updateAuthAndFormData transformForm transform model cmd =
    ( { model | authData = transform model.authData, authForm = transformForm model.authForm }, cmd )


updateAuthData : (AuthData -> AuthData) -> Model -> Cmd Msg -> ( Model, Cmd Msg )
updateAuthData transform model cmd =
    ( { model | authData = transform model.authData }, cmd )


updateAuthFormData : (AuthForm -> AuthForm) -> Model -> Cmd Msg -> ( Model, Cmd Msg )
updateAuthFormData transform model cmd =
    ( { model | authForm = transform model.authForm }, cmd )



---- VIEW ----


viewListItem : Todo -> Html Msg
viewListItem todo =
    li []
        [ div [ class "view" ]
            [ div [ class "round" ]
                [ input [ checked todo.is_completed, type_ "checkbox", id (String.fromInt todo.id), onClick (MarkCompleted todo.id todo.is_completed) ] []
                , label [ for (String.fromInt todo.id) ] []
                ]
            ]
        , div
            [ classList
                [ ( "labelContent", True )
                , ( "completed", todo.is_completed )
                ]
            ]
            [ div [] [ text todo.title ]
            ]
        , button [ class "closeBtn", onClick (DelTodo todo.id) ]
            [ text "x"
            ]
        ]


viewKeyedListItem : Todo -> ( String, Html Msg )
viewKeyedListItem todo =
    ( String.fromInt todo.id, viewListItem todo )


filterTodos : String -> Todo -> Bool
filterTodos visibility todo =
    case visibility of
        "Completed" ->
            todo.is_completed

        "Active" ->
            not todo.is_completed

        _ ->
            True


todoListWrapper : String -> Todos -> Html Msg
todoListWrapper visibility todos =
    div [ class "wrapper" ]
        [ div [ class "todoListWrapper" ]
            [ Keyed.ul [] <|
                List.map viewKeyedListItem (List.filter (filterTodos visibility) todos)
            ]
        , footerList todos visibility
        ]


renderActionBtn : String -> String -> Html Msg
renderActionBtn classVal value =
    li []
        [ a [ class classVal, onClick (UpdateVisibility value) ]
            [ text value
            ]
        ]


activeClass : String -> String -> String
activeClass currentVisibility visibility =
    if currentVisibility == visibility then
        "selected"

    else
        ""


footerActionBtns : String -> Html Msg
footerActionBtns visibility =
    ul []
        [ renderActionBtn (activeClass "All" visibility) "All"
        , renderActionBtn (activeClass "Active" visibility) "Active"
        , renderActionBtn (activeClass "Completed" visibility) "Completed"
        ]


clearButton : Html Msg
clearButton =
    button [ class "clearComp", onClick DeleteAllCompletedItems ]
        [ text "Clear completed"
        ]


footerList : Todos -> String -> Html Msg
footerList todos visibility =
    div [ class "footerList" ]
        [ span []
            [ text
                (String.fromInt
                    (List.length
                        (List.filter (filterTodos visibility) todos)
                    )
                    ++ " Items"
                )
            ]
        , footerActionBtns visibility
        , clearButton
        ]


renderTodos : PrivateTodo -> Html Msg
renderTodos privateData =
    div [ class "tasks_wrapper" ] <|
        case privateData.todos of
            RemoteData.NotAsked ->
                [ text "" ]

            RemoteData.Success todos ->
                [ todoListWrapper privateData.visibility todos ]

            RemoteData.Loading ->
                [ span [ class "loading_text" ]
                    [ text "Loading todos ..." ]
                ]

            RemoteData.Failure err ->
                [ text "Error loading todos" ]


handleMutationTodo : GraphQLResponse MaybeMutationResponse -> List (Html msg)
handleMutationTodo (GraphQLResponse mutationTodo) =
    case mutationTodo of
        RemoteData.NotAsked ->
            [ text "" ]

        RemoteData.Success todos ->
            [ text "" ]

        RemoteData.Loading ->
            [ i [ class "fa fa-spinner fa-spin" ] []
            ]

        RemoteData.Failure err ->
            [ text "Error Mutating data:" ]


todoMutation : GraphQLResponse MaybeMutationResponse -> Html msg
todoMutation mutateTodo =
    span [ class "mutation_loader" ] <|
        handleMutationTodo mutateTodo


personalTodos : PrivateTodo -> Html Msg
personalTodos privateData =
    div [ class "col-xs-12 col-md-6 sliderMenu p-30" ]
        [ div [ class "todoWrapper" ]
            [ div [ class "sectionHeader" ]
                [ text "Personal todos"
                ]
            , form [ class "formInput", onSubmit InsertPrivateTodo ]
                [ input [ class "input", placeholder "What needs to be done?", onInput UpdateNewTodo, value privateData.newTodo ]
                    []
                , i [ class "inputMarker fa fa-angle-right" ] []
                , todoMutation privateData.mutateTodo
                ]
            , renderTodos privateData
            ]
        ]



{-
   Public todo render functions
-}


nothing : Html msg
nothing =
    text ""


loadLatestPublicTodo : Int -> Html Msg
loadLatestPublicTodo count =
    case count of
        0 ->
            nothing

        _ ->
            div [ class "loadMoreSection", onClick FetchNewPublicTodos ]
                [ text ("New tasks have arrived! (" ++ String.fromInt count ++ ")")
                ]


loadOldPublicTodos : Bool -> Html Msg
loadOldPublicTodos oldTodosAvailable =
    case oldTodosAvailable of
        True ->
            div [ class "loadMoreSection", onClick FetchOldPublicTodos ]
                [ text "Load older tasks"
                ]

        False ->
            div [ class "loadMoreSection" ]
                [ text "No more public tasks!"
                ]


publicTodoListWrapper : PublicTodoData -> Html Msg
publicTodoListWrapper publicTodoInfo =
    div [ class "wrapper" ]
        [ loadLatestPublicTodo publicTodoInfo.newTodoCount
        , div
            [ class "todoListWrapper" ]
            [ Keyed.ul [] <|
                List.map publicViewKeyedListItem publicTodoInfo.todos
            ]
        , loadOldPublicTodos publicTodoInfo.oldTodosAvailable
        ]


publicViewListItem : Todo -> Html Msg
publicViewListItem todo =
    li []
        [ div [ class "userInfoPublic", title todo.user_id ]
            [ text ("@" ++ todo.user.name)
            ]
        , div [ class "labelContent" ] [ text todo.title ]
        ]


publicViewKeyedListItem : Todo -> ( String, Html Msg )
publicViewKeyedListItem todo =
    ( String.fromInt todo.id, publicViewListItem todo )


publicTodoInsertStatus : Operation -> Html msg
publicTodoInsertStatus status =
    span [ class "mutation_loader" ] <|
        case status of
            NotYetInitiated ->
                [ text "" ]

            OnGoing ->
                [ i [ class "fa fa-spinner fa-spin" ] []
                ]

            OperationFailed error ->
                [ i [ title error, class "fas fa-exclamation-triangle" ] []
                ]


publicTodos : Model -> Html Msg
publicTodos model =
    div [ class "col-xs-12 col-md-6 sliderMenu p-30 bg-gray border-right" ]
        [ div [ class "todoWrapper" ]
            [ div [ class "sectionHeader" ]
                [ text "Public feed (realtime)"
                ]
            , form [ class "formInput", onSubmit InsertPublicTodo ]
                [ input [ class "input", placeholder "What needs to be done?", value model.publicTodoInsert, onInput UpdatePublicNewTodo ]
                    []
                , i [ class "inputMarker fa fa-angle-right" ] []
                , publicTodoInsertStatus model.publicTodoInsertStatus
                ]
            , publicTodoListWrapper model.publicTodoInfo
            ]
        ]



{-
   Login render functions
-}


textInput : String -> String -> (String -> Msg) -> Html Msg
textInput val p onChange =
    div [ class "authentication_input" ]
        [ input
            [ class "form-control input-lg"
            , placeholder p
            , type_ "text"
            , value val
            , onInput onChange
            ]
            []
        ]


passwordInput : String -> (String -> Msg) -> Html Msg
passwordInput val onChange =
    div [ class "authentication_input" ]
        [ input
            [ class "form-control input-lg"
            , placeholder "Password"
            , type_ "password"
            , value val
            , onInput onChange
            ]
            []
        ]


authenticationToggler : String -> String -> DisplayForm -> Html Msg
authenticationToggler val ref onToggle =
    a [ class "authentication_toggle", href ref, onClick (ToggleAuthForm onToggle) ]
        [ text val
        ]


actionButton : String -> Bool -> Msg -> Html Msg
actionButton val isRequestInProgress clickHandler =
    button
        [ classList
            [ ( "btn-success btn-lg remove_border ", True )
            , ( "disabled", isRequestInProgress )
            ]
        , disabled isRequestInProgress
        , onClick clickHandler
        , type_ "button"
        ]
        [ text val ]


showSignupSuccess : Bool -> Html msg
showSignupSuccess isSignupSuccess =
    case isSignupSuccess of
        True ->
            div [ class "signup_success" ]
                [ text "Signup successful! Please login with the same credentials to continue"
                ]

        False ->
            text ""


loginView : AuthData -> Bool -> String -> Bool -> Html Msg
loginView authData isRequestInProgress reqErr isSignupSuccess =
    div [ class "container authentication_wrapper" ]
        [ div [ class "row" ]
            [ div [ class "col-md-12 col-xs-12" ]
                [ showSignupSuccess isSignupSuccess
                , h1 [ class "c_mb_5 ta_center" ]
                    [ text "Sign in"
                    ]
                , p [ class "c_mb_10 ta_center" ]
                    [ authenticationToggler "Register?" "#register" Signup
                    ]
                , form []
                    [ textInput authData.username "Email" EnteredUsername
                    , passwordInput authData.password EnteredPassword
                    , actionButton "Sign in" isRequestInProgress MakeLoginRequest
                    , div [ class "error_auth_response" ] <|
                        case String.length reqErr of
                            0 ->
                                [ text "" ]

                            _ ->
                                [ text ("Login error:  " ++ reqErr) ]
                    ]
                ]
            ]
        ]


signupView : AuthData -> Bool -> String -> Html Msg
signupView authData isRequestInProgress reqErr =
    div [ class "container authentication_wrapper" ]
        [ div [ class "row" ]
            [ div [ class "col-md-12 col-xs-12" ]
                [ h1 [ class "c_mb_5 ta_center" ]
                    [ text "Sign up"
                    ]
                , p [ class "c_mb_10 ta_center" ]
                    [ authenticationToggler "Login?" "#login" Login
                    ]
                , form []
                    [ textInput authData.username "Email" EnteredUsername
                    , passwordInput authData.password EnteredPassword
                    , actionButton "Sign up" isRequestInProgress MakeSignupRequest
                    , text reqErr
                    ]
                ]
            ]
        ]



{-
   The following commented code is TodoMVC code
-}


topNavBar : Html Msg
topNavBar =
    nav [ class "m-bottom-0 navbar navbar-default" ]
        [ div [ class "container-fluid" ]
            [ div [ class "navHeader navbar-header" ]
                [ span [ class "navBrand navbar-brand " ]
                    [ text "Elm Todo Tutorial App"
                    ]
                , ul [ class "nav navbar-nav navbar-right " ]
                    [ li []
                        [ a []
                            [ button
                                [ class "btn btn-primary", onClick ClearAuthToken ]
                                [ text "Log Out" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


getOnlineUsersCount : OnlineUsersData -> Int
getOnlineUsersCount onlineUsers =
    case onlineUsers of
        RemoteData.Success data ->
            List.length data

        _ ->
            0


generateOnlineUsersList : OnlineUsersData -> List (Html msg)
generateOnlineUsersList onlineUser =
    case onlineUser of
        RemoteData.Success d ->
            List.map viewOnlineUser d

        _ ->
            [ text "" ]


viewUserName : String -> Html msg
viewUserName str =
    div [ class "userInfo" ]
        [ div [ class "userImg" ]
            [ i [ class "far fa-user" ] [] ]
        , div [ class "userName" ]
            [ text str ]
        ]


viewOnlineUser : OnlineUser -> Html msg
viewOnlineUser onlineUser =
    case onlineUser.user of
        Just user ->
            viewUserName user.name

        Nothing ->
            text ""


viewTodoSection : Model -> Html Msg
viewTodoSection model =
    div [ class "content" ]
        [ topNavBar
        , div [ class "container-fluid p-left-right-0" ]
            [ div [ class "col-xs-12 col-md-9 p-left-right-0" ]
                [ personalTodos model.privateData
                , publicTodos model
                ]
            , div [ class "col-xs-12 col-md-3 p-left-right-0" ]
                [ div [ class "col-xs-12 col-md-12 sliderMenu p-30 bg-gray" ]
                    [ div [ class "onlineUsersWrapper" ]
                        [ div [ class "sliderHeader" ]
                            [ text ((++) "Online Users - " (String.fromInt (getOnlineUsersCount model.online_users)))
                            ]
                        , div [] <|
                            generateOnlineUsersList model.online_users
                        ]
                    ]
                ]
            ]
        ]



{-
   Main view function
-}


view : Model -> Html Msg
view model =
    div [ class "content" ] <|
        case String.length model.authData.authToken of
            0 ->
                case model.authForm.displayForm of
                    Login ->
                        [ loginView model.authData model.authForm.isRequestInProgress model.authForm.requestError model.authForm.isSignupSuccess
                        ]

                    Signup ->
                        [ signupView model.authData model.authForm.isRequestInProgress model.authForm.requestError
                        ]

            _ ->
                [ viewTodoSection model
                ]
