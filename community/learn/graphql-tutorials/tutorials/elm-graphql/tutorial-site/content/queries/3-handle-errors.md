---
title: "Handle loading/errors"
metaTitle: "Error Handling | GraphQL Elm Tutorial"
metaDescription: "We will handle the GraphQL loading and error states in elm app - loading and error "
---

As we saw in the previous step, the respnose data is captured using `RemoteData`. The data can be in one of the following states

- `Loading` : Indicates whether the request is in flight. If it is in `Loading` state, then the request hasn't finished. Typically this information can be used to display a loading spinner.

-  `Failure` : Indicates the request has failed. Current implementation shows a placeholder message.

- `Success`: Indicates the request has succeeded. This information can be used to render the information.

The render function is equipped to handle the above states. It renders
  - `Loading` : Shows a loading spinner

  - `Failure` : Shows a custom error

  - `Success` : Renders the list of todos

```
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
```
