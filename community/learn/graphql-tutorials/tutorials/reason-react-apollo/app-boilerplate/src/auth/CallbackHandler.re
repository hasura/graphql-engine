[@bs.val] external locationReplace : (string) => unit = "window.location.replace";

[@react.component]
let make = (~urlHash: string) => {
  if (urlHash == "") {
    ReasonReactRouter.push("/login");
    <Login />;
  } else {
    let auth0Response = Js.String.split("&", urlHash);
    let numAuthParams = Array.length(auth0Response);
    switch(numAuthParams) {
      | 0 | 1 | 2 => <Login />
      | _ => {
        let idToken = Array.fold_left(
          (value, param) => {
            if (Js.String.startsWith("id_token", param)) {
              Js.String.substr(~from=9, param);
            } else { value }
          },
          "",
          auth0Response
        );
        let expiresIn = Array.fold_left(
          (value, param) => {
            if (Js.String.startsWith("expires_in", param)) {
              Js.String.substr(~from=11, param);
            } else { value }
          },
          "",
          auth0Response
        );
        Util.saveSessionToStorage(idToken, expiresIn);
        locationReplace("/");
        <App />
      }
    };
  }
}