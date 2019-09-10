[@react.component]
let make = () => {

  // Check URL and map path
  let url = ReasonReactRouter.useUrl();

  switch (url.path) {

    // Route for Todo App
    | [] | ["login"]=> <AuthHandler />

    // Route for auth callback
    | ["callback"] => <CallbackHandler urlHash={url.hash}/>

    // Redirect to root
    | _ => {
      ReasonReactRouter.push("/");
      <div>{ReasonReact.string("Please wait...")}</div>
    }
  }
}
