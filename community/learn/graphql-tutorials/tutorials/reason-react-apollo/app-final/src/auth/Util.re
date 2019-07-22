let saveSessionToStorage = (token, expiryDuration) => {
  Dom.Storage.(localStorage |> setItem("@learn.hasura.io:reason-apollo-token", token));
  let expiryTime = (Js.Date.now() /. 1000.0) +. float_of_string(expiryDuration)
    |> string_of_float;
  Dom.Storage.(localStorage |> setItem("@learn.hasura.io:reason-apollo-exp", expiryTime));
};

let getTokenFromStorage = () => {
  Dom.Storage.(localStorage |> getItem("@learn.hasura.io:reason-apollo-token"))
};

let removeSessionFromStorage = () => {
  Dom.Storage.(localStorage |> removeItem("@learn.hasura.io:reason-apollo-token"))
  Dom.Storage.(localStorage |> removeItem("@learn.hasura.io:reason-apollo-exp"))
};

let isSessionValid = () => {
  switch(Dom.Storage.(localStorage |> getItem("@learn.hasura.io:reason-apollo-exp"))) {
    | None => false
    | Some(expiryTime) => float_of_string(expiryTime) > Js.Date.now() /. 1000.0
  };
};