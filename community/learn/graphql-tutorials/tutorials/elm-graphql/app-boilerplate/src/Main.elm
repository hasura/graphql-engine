port module Main exposing (main)

{-
   Graphql-elm imports
-}

import Array
import Browser
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
import Html.Events exposing (onClick, onInput)
import Html.Keyed as Keyed
import Http
import Json.Decode exposing (Decoder, field, int, string)
import Json.Encode as Encode
import RemoteData exposing (RemoteData)



{-
   Constants
-}


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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



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
    { id : String
    , user : User
    }


type alias PrivateTodo =
    { todos : Todos
    , visibility : String
    , newTodo : String
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


type alias Model =
    { privateData : PrivateTodo
    , publicTodoInsert : String
    , publicTodoInfo : PublicTodoData
    , online_users : OnlineUsers
    , authData : AuthData
    , authForm : AuthForm
    }



{-
   Initial seed data
-}


seedIds : List Int
seedIds =
    [ 1, 2 ]


publicSeedIds : List Int
publicSeedIds =
    [ 1, 2, 3, 4 ]


todoPrivatePlaceholder : String
todoPrivatePlaceholder =
    "This is private todo"


todoPublicPlaceholder : String
todoPublicPlaceholder =
    "This is public todo"


generateUser : Int -> User
generateUser id =
    User ("someUser" ++ String.fromInt id)


generateTodo : String -> Int -> Todo
generateTodo placeholder id =
    let
        isCompleted =
            id == 1
    in
    Todo id ("User" ++ String.fromInt id) isCompleted (placeholder ++ " " ++ String.fromInt id) (generateUser id)


privateTodos : Todos
privateTodos =
    List.map (generateTodo todoPrivatePlaceholder) seedIds


generatePublicTodo : String -> Int -> Todo
generatePublicTodo placeholder id =
    Todo id ("User" ++ String.fromInt id) False (placeholder ++ " " ++ String.fromInt id) (generateUser id)


getPublicTodos : Todos
getPublicTodos =
    List.map (generatePublicTodo todoPublicPlaceholder) publicSeedIds


generateOnlineUser : Int -> OnlineUser
generateOnlineUser id =
    OnlineUser (String.fromInt id) (generateUser id)


getOnlineUsers : OnlineUsers
getOnlineUsers =
    List.map generateOnlineUser seedIds


initializePrivateTodo : PrivateTodo
initializePrivateTodo =
    { todos = privateTodos
    , visibility = "All"
    , newTodo = ""
    }


initialize : Model
initialize =
    { privateData = initializePrivateTodo
    , online_users = getOnlineUsers
    , publicTodoInsert = ""
    , publicTodoInfo = PublicTodoData getPublicTodos 0 1 0 True
    , authData = AuthData "" "" "" ""
    , authForm = AuthForm Login False False ""
    }


init : ( Model, Cmd Msg )
init =
    ( initialize
    , Cmd.none
    )



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
                    updateAuthAndFormData (\authForm -> { authForm | isRequestInProgress = False, isSignupSuccess = False }) (\authData -> { authData | authToken = d.token }) model Cmd.none

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



{-
   Helper funcs
-}


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
                [ input [ checked todo.is_completed, type_ "checkbox", id (String.fromInt todo.id) ] []
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
        , button [ class "closeBtn" ]
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
        [ a [ class classVal ]
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
    button [ class "clearComp" ]
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
    div [ class "tasks_wrapper" ]
        [ todoListWrapper privateData.visibility privateData.todos ]


personalTodos : PrivateTodo -> Html Msg
personalTodos privateData =
    div [ class "col-xs-12 col-md-6 sliderMenu p-30" ]
        [ div [ class "todoWrapper" ]
            [ div [ class "sectionHeader" ]
                [ text "Personal todos"
                ]
            , form [ class "formInput" ]
                [ input [ class "input", placeholder "What needs to be done?" ]
                    []
                , i [ class "inputMarker fa fa-angle-right" ] []
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
            div [ class "loadMoreSection" ]
                [ text ("New tasks have arrived! (" ++ String.fromInt count ++ ")")
                ]


loadOldPublicTodos : Bool -> Html Msg
loadOldPublicTodos oldTodosAvailable =
    case oldTodosAvailable of
        True ->
            div [ class "loadMoreSection" ]
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


publicTodos : Model -> Html Msg
publicTodos model =
    div [ class "col-xs-12 col-md-6 sliderMenu p-30 bg-gray border-right" ]
        [ div [ class "todoWrapper" ]
            [ div [ class "sectionHeader" ]
                [ text "Public feed (realtime)"
                ]
            , form [ class "formInput" ]
                [ input [ class "input", placeholder "What needs to be done?", value model.publicTodoInsert ]
                    []
                , i [ class "inputMarker fa fa-angle-right" ] []
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


getOnlineUsersCount : OnlineUsers -> Int
getOnlineUsersCount onlineUsers =
    List.length onlineUsers


generateOnlineUsersList : OnlineUsers -> List (Html msg)
generateOnlineUsersList onlineUser =
    List.map viewOnlineUser onlineUser


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
    viewUserName onlineUser.user.name


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
